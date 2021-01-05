library(rtweet)

THIS_MONTH <- as.numeric(strftime(Sys.time(), "%m"))
THIS_YEAR <- as.numeric(strftime(Sys.time(), "%Y"))
if (THIS_MONTH < 6) THIS_YEAR <- THIS_YEAR - 1

glue_year <- function(...) {
  glue::glue(..., .year = THIS_YEAR)
}

start_year <- strptime(glue_year("{.year}-01-01 00:00:00"), "%F %T", tz = "UTC")
end_year <- strptime(glue_year("{.year}-12-31 23:59:59"), "%F %T", tz = "UTC")

check_rate_limit <- function(query = "statuses/user_timeline") {
  now <- Sys.time()
  if (!exists(".rate_limit")) {
    .rate_limit <<- list(remaining = 900, reset_at = now + 15 * 60, checked = now)
  }
  if (.rate_limit$checked < (now - 60) || .rate_limit$reset_at < now) {
    rl <- rtweet::rate_limits(query = "statuses/user_timeline")
    .rate_limit <<- list(remaining = rl$remaining, reset_at = rl$reset_at, checked = now)
  }
  return(.rate_limit)
}

get_timeline <- purrr::safely(rtweet::get_timeline)

get_user_tweets <- function(screen_name, ...) {
  get_timeline(
    screen_name,
    include_rts = FALSE,
    exclude_replies = FALSE,
    n = 3200
  )
}

sum_ <- function(x) {
  if (any(!is.na(x))) {
    sum(x, na.rm = TRUE)
  }
}

best <- function(x, by = "favorite") {
  by <- paste0(by, "_count")
  x <- x[!x$is_retweet, ]
  x <- x[x[[by]] == max(x[[by]], na.rm = TRUE), ]
  x[1, ]
}
hashtags <- function(x) {
  x <- unlist(x)
  if(!length(x)) return(list(hashtag = NULL, count = NULL))
  x <- table(x)
  x <- sort(x, decreasing = TRUE)[seq(1, min(length(x), 10))]
  lapply(names(x), function(h) list(hashtag = h, count = x[[h]]))
}
mentions <- function(x) {
  x <- unlist(x)
  if(!length(x)) return(list(mentions = NULL, count = NULL))
  x <- table(x)
  x <- sort(x, decreasing = TRUE)[seq(1, min(length(x), 10))]
  lapply(names(x), function(s) list(screen_name = s, count = x[[s]]))
}
user_info <- function(tw) {
  u <- rtweet::users_data(tw[1, ])
  as.list(u)
}
tweet_stats <- function(tw) {
  tw_year <- tw[tw$created_at >= start_year & tw$created_at <= end_year, ]
  tw_best_favorite <- best(tw_year, "favorite")
  tw_best_retweet <- best(tw_year, "retweet")
  list(
    user = user_info(tw),
    has_tweet_prior_year = any(tw$created_at < start_year),
    created_at_min = min(tw_year$created_at),
    created_at_max = max(tw_year$created_at),
    n = nrow(tw_year[!tw_year$is_retweet, ]),
    favorite_count = sum_(tw_year$favorite_count),
    retweet_count = sum_(tw_year$retweet_count),
    quote_count = sum_(tw_year$quote_count),
    reply_count = sum_(tw_year$reply_count),
    best_favorite = list(
      status_id = tw_best_favorite$status_id,
      url = tw_best_favorite$status_url,
      favorite_count = tw_best_favorite$favorite_count,
      retweet_count = tw_best_favorite$retweet_count
    ),
    best_retweet = list(
      status_id = tw_best_retweet$status_id,
      url = tw_best_retweet$status_url,
      favorite_count = tw_best_retweet$favorite_count,
      retweet_count = tw_best_retweet$retweet_count
    ),
    hashtags = hashtags(tw_year$hashtags),
    mentions = mentions(tw_year$mentions_screen_name),
    calendar = tweet_count_by_day(tw_year)
  )
}

tweet_count_by_day <- function(tw) {
  tw <- as.data.frame(table(as.Date(tw$created_at)), stringsAsFactors = FALSE)
  names(tw) <- c("date", "n_tweets")
  tw$date <- as.Date(tw$date)

  base <- data.frame(
    date = seq(as.Date(start_year), min(as.Date(end_year), Sys.Date()), by = "day"),
    n_tweets = 0,
    stringsAsFactors = FALSE
  )

  # user tweets more than once every three days on average
  days_in_tweet_range <- as.numeric(diff(range(tw$date)))
  if (nrow(tw) / days_in_tweet_range > 0.33) {
    return(tw)
  }

  # fill in 0 days for low-volume tweeters
  tw <- rbind(tw, base)
  tw[!duplicated(tw$date), ]
  tw[order(tw$date), ]
}
