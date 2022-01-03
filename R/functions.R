THIS_MONTH <- as.numeric(strftime(Sys.time(), "%m"))
THIS_YEAR <- as.numeric(strftime(Sys.time(), "%Y"))
if (THIS_MONTH < 6) THIS_YEAR <- THIS_YEAR - 1

glue_year <- function(..., env = parent.frame()) {
  env <- new.env(parent = env)
  env$.year <- THIS_YEAR
  glue::glue(..., .envir = env)
}

start_year <- strptime(glue_year("{.year}-01-01 00:00:00"), "%F %T", tz = "UTC")
end_year <- strptime(glue_year("{.year}-12-31 23:59:59"), "%F %T", tz = "UTC")

check_rate_limit <- function(query = "statuses/user_timeline") {
  now <- Sys.time()
  if (!exists(".rate_limit")) {
    .rate_limit <<- list(remaining = 900, reset_at = now + 15 * 60, checked = now)
  }
  if (.rate_limit$checked < (now - 60) || .rate_limit$reset_at < now) {
    rl <- rtweet::rate_limit("statuses/user_timeline")
    .rate_limit <<- list(remaining = rl$remaining, reset_at = rl$reset_at, checked = now)
  }
  return(.rate_limit)
}

get_timeline <- purrr::safely(rtweet::get_timeline)

get_user_tweets <- function(screen_name, ...) {
  message(strftime(Sys.time(), "[%F %T] "), "[start] Getting tweets for @", screen_name)
  x <- get_timeline(
    screen_name,
    include_rts = FALSE,
    exclude_replies = FALSE,
    n = 3200
  )
  message(strftime(Sys.time(), "[%F %T] "), "[end] Getting tweets for @", screen_name)
  # fill in some data not provided by twitter api v2
  if (!is.null(x$result)) {
    x$result$screen_name <- screen_name
    x$result$created_at <- strptime(x$result$created_at, "%a %b %e %T %z %Y")
    x$result$status_url <- status_url(x$result)
    x$result <- x$result[!is.na(x$result$id_str), ]
  }
  x
}

sum_ <- function(x) {
  if (any(!is.na(x))) {
    sum(x, na.rm = TRUE)
  }
}

best <- function(x, by = "favorite") {
  by <- paste0(by, "_count")
  x <- x[!x$retweeted, ]
  x <- x[x[[by]] == max(x[[by]], na.rm = TRUE), ]
  x[1, ]
}
hashtags <- function(x) {
  x <- x$entities %>%
    purrr::map("hashtags") %>%
    purrr::map("text") %>%
    purrr::discard(~ all(is.na(.x))) %>%
    unlist()
  if(!length(x)) return(list(hashtag = NULL, count = NULL))
  x <- table(x)
  x <- sort(x, decreasing = TRUE)[seq(1, min(length(x), 10))]
  lapply(names(x), function(h) list(hashtag = h, count = x[[h]]))
}
mentions <- function(x) {
  x <- x$entities %>%
    purrr::map("user_mentions") %>%
    purrr::map("screen_name") %>%
    purrr::discard(~ all(is.na(.x))) %>%
    unlist()
  if(!length(x)) return(list(mentions = NULL, count = NULL))
  x <- table(x)
  x <- sort(x, decreasing = TRUE)[seq(1, min(length(x), 10))]
  lapply(names(x), function(s) list(screen_name = s, count = x[[s]]))
}
user_info <- function(tw) {
  u <- rtweet::users_data(tw)
  u <- u[!duplicated(u$id), ]
  as.list(u)
}
status_url <- function(tw) {
  sprintf("https://twitter.com/%s/status/%s", tw$screen_name, tw$id_str)
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
    n = nrow(tw_year[!tw_year$retweeted, ]),
    favorite_count = sum_(tw_year$favorite_count),
    retweet_count = sum_(tw_year$retweet_count),
    # quote_count = sum_(tw_year$quote_count),
    # reply_count = sum_(tw_year$reply_count),
    best_favorite = list(
      status_id = tw_best_favorite$id_str,
      url = tw_best_favorite$status_url,
      favorite_count = tw_best_favorite$favorite_count,
      retweet_count = tw_best_favorite$retweet_count
    ),
    best_retweet = list(
      status_id = tw_best_retweet$id_str,
      url = tw_best_retweet$status_url,
      favorite_count = tw_best_retweet$favorite_count,
      retweet_count = tw_best_retweet$retweet_count
    ),
    hashtags = hashtags(tw_year),
    mentions = mentions(tw_year),
    calendar = tweet_count_by_day(tw_year),
    streak = tweet_streak(tw_year),
    dow = tweet_favorite_day(tw_year),
    days_year = tweet_days_of_year(tw_year)
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

tweet_favorite_day <- function(tw) {
  days <- as.data.frame(table(strftime(tw$created_at, "%A")), stringsAsFactors = FALSE)
  names(days) <- c("day", "n_tweets")
  days$day <- factor(
    days$day,
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
    # labels = c("M", "Tu", "W", "Th", "F", "Sa", "Su"),
    ordered = TRUE
  )
  days[order(days$day), ]
}

tweet_days_of_year <- function(tw) {
  days_year <- length(seq(start_year, end_year, by = "day"))
  tw <- tweet_count_by_day(tw)
  list(tweeted = nrow(tw), year = days_year)
}

tweet_streak <- function(tw) {
  tw %>%
    tweet_count_by_day() %>%
    mutate(
      same = date == lead(date, default = first(date)) - 1,
      drop = !same & !lag(same)
    ) %>%
    filter(!drop) %>%
    mutate(id = cumsum(same & same != lead(same))) %>%
    group_by(id) %>%
    summarize(n = n(), start = min(date), end = max(date)) %>%
    arrange(desc(n))
}
