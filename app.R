library(shiny)
library(purrr)
library(dplyr)
library(memoise)
library(glue)
library(metricsgraphics)

# auth with twitter
library(rtweet)
if (!file.exists(".auth.rds")) {
  stop("Must provide bearer token in `.auth.rds`, see https://docs.ropensci.org/rtweet/articles/auth.html")
}
auth_as(readRDS(".auth.rds"))

source("R/functions.R")
get_tweets <- memoise(get_user_tweets, cache = cache_filesystem(".tweets"))

col_3 <- function(...) div(class = "col-xs-12 col-sm-4", ...)
col_2 <- function(...) div(class = "col-xs-12 col-sm-6", ...)

number <- function(x) {
  div(class = "number", x)
}
number_label <- function(text) {
  div(class = "number-label", text)
}
number_icon <- function(icon) {
  div(class = "number-icon", shiny::icon(icon))
}
number_div <- function(x, label, icon = "heart", ...) {
  col_3(
    class = "number-container",
    ...,
    if (!is.null(icon)) number_icon(icon),
    number(x),
    number_label(label)
  )
}

ui <-
  fluidPage(
    theme = bslib::bs_theme(version = 5, "vapor"),
    metathis::meta_social(
      title = glue_year("Your {.year} on Twitter"),
      description = "Look back on your year online: your best tweets, friends, favorite hashtags and more.",
      image = "https://gadenbuie.shinyapps.io/tweets-of-the-year/og-preview.jpg",
      twitter_card_type = "summary_large_image",
      twitter_creator = "grrrck"
    ),
    tags$head(
      HTML(
        "<!-- Include in <head> to load fonts from Google -->
        <link href='https://fonts.googleapis.com/css?family=Oswald:300' rel='stylesheet' type='text/css'>
        <link href='https://fonts.googleapis.com/css?family=Lora:400italic' rel='stylesheet' type='text/css'>
        <link href='https://fonts.googleapis.com/css?family=Montserrat:400' rel='stylesheet' type='text/css'>"
      ),
      tags$title(glue_year("{.year} on Twitter")),
      tags$link(href = "styles.css", rel = "stylesheet")
    ),
    fluidRow(
      div(
        class = "col-xs-12",
        h1(
          glue_year("In Review: Your {.year} on Twitter"),
          id = "user-year-title",
          class = "text-center"
        ),
        withTags(
          form(
            class = "form-inline search-screen-name",
            div(
              class = "form-group",
              div(
                class = "input-group",
                label(
                  class = "sr-only",
                  `for` = "screen_name",
                  "Twitter Screen Name"
                ),
                div(
                  class = "input-group shiny-input-container",
                  span(class = "input-group-text", "@"),
                  input(id = "screen_name", type = "text", class = "form-control", placeholder = "Twitter User Name", value = ""),
                  button(id = "search", type = "button", class = "btn btn-primary pull-right action-button", "Search")
                )
              )
            )
          )
        ),
        div(
          id = "error-rate-limit",
          class = "help-block red text-center",
          hidden = NA,
          "Oh oh, I'm over the rate limit. Please try again in a few minutes."
        ),
        div(
          id = "error-bad-user",
          class = "help-block red text-center",
          hidden = NA,
          glue_year("That user doesn't exist or didn't tweet in {.year}.")
        ),
        div(
          id = "error-protected-user",
          class = "help-block red text-center",
          hidden = NA,
          "That account is protected."
        )
      )
    ),
    div(
      id = "searching-for-tweets",
      class = "text-center",
      hidden = NA,
      div(
        class = "fadeIn",
        span(class = "spin", icon("twitter", class = "fa-lg twitter-blue")),
        "Looking up",
        span(id = "searching_screen_name", HTML("&commat;SCREEN_NAME"), .noWS = "after"),
        glue_year("'s tweets from {.year}...")
      ),
      HTML('<div class="searching-hold-please fadeIn">Hang on, this could take a little longer...</div>')
    ),
    div(
      id = "search-results",
      class = "fadeIn",
      hidden = NA,
      fluidRow(
        number_div(
          countup::countupOutput("count_tweets"), "tweets", "twitter",
          title = glue_year("Number of original tweets in {.year}")
        ),
        number_div(
          countup::countupOutput("count_likes"), "favorites", "heart",
          title = glue_year("Number of times this user's tweets were favorited in {.year}")
        ),
        number_div(
          countup::countupOutput("count_retweets"), "retweets", "recycle",
          title = glue_year("Number of times this user's tweets were retweeted in {.year}")
        ),
        number_div(
          countup::countupOutput("count_days"), "tweeting days", "calendar-check",
          title = glue_year("Number of days with tweets in {.year}")
        ),
        number_div(
          uiOutput("streak", inline = TRUE), "longest streak", "bolt",
          title = glue_year("Longest streak of consecutive tweeting days in a row in {.year}")
        ),
        number_div(
          uiOutput("favorite_day", inline = TRUE), "favorite day", "calendar",
          title = glue_year("Favorite day to tweet in {.year}")
        )
      ),
      fluidRow(
        class = "tweets-calendar",
        metricsgraphicsOutput("tweets_calendar", height = "200px")
      ),
      fluidRow(
        class = "tweets-most justify-content-center",
        col_2(
          h2("Most Liked", class = "text-center"),
          uiOutput("most_liked")
        ),
        col_2(
          h2("Most Retweeted", class = "text-center"),
          uiOutput("most_retweeted")
        )
      ),
      fluidRow(
        class = "tweets-tables",
        col_2(
          h2("Mentions", class = "text-center"),
          tableOutput("most_mentioned")
        ),
        col_2(
          h2("Hashtags", class = "text-center"),
          tableOutput("most_hashtagged")
        )
      )
    ),
    tags$footer(
      class = "footer sticky-bottom",
      p(
        HTML(
          "Made with &#x2764;&#xFE0F; and &#x2615; by",
          '<a href="https://twitter.com/grrrck" target="_blank">&commat;grrrck</a>'
        ),
        "with help from",
        tags$a(href = "https://rtweet.info", "rtweet", .noWS = "after"),
        ",",
        tags$a(href = "https://shiny.rstudio.com", "shiny", .noWS = "after"),
        ", and",
        tags$a(href = "https://github.com/JohnCoene/countup", "countup", .noWS = "after"),
        ".",
        tags$br(),
        tags$a(
          href = "https://github.com/gadenbuie/tweets-of-the-year",
          icon("github"),
          target = "_blank",
          "gadenbuie/tweets-of-the-year"
        )
      )
    ),
    includeScript("script.js")
  )

server <- function(input, output, session) {
  rv <- reactiveValues(
    tweets = list(error = TRUE),
    last = list(count = 0, likes = 0, retweets = 0)
  )

  observeEvent(paste(input$search, input[["__key_search"]], input$browser_tz), {
    sn <- input$screen_name
    sn <- trimws(sn)
    if (sn == "") return(list(error = TRUE))
    rl <- check_rate_limit()
    if (rl$remaining == 0) {
      session$sendCustomMessage("show", list(show = TRUE, id = "error-rate-limit"))
      rv$tweets <- list(error = TRUE, rate_limit = TRUE, msg = rl$reset_at)
    }

    session$sendCustomMessage("show", list(show = FALSE, id = "error-rate-limit"))
    session$sendCustomMessage("show", list(show = FALSE, id = "error-bad-user"))
    session$sendCustomMessage("show", list(show = FALSE, id = "error-protected-user"))

    session$sendCustomMessage("showWaiting", list(screen_name = input$screen_name, show = TRUE))
    tw <- get_tweets(sn)
    session$sendCustomMessage("showWaiting", list(show = FALSE))

    if (is.null(tw$error) && nrow(tw$result)) {
      ts <- tweet_stats(tw$result, tz = input$browser_tz)
      if (any(user_info(tw$result)$protected)) {
        session$sendCustomMessage("show", list(show = TRUE, id = "error-protected-user"))
      } else {
        session$sendCustomMessage("show", list(show = TRUE))
        session$sendCustomMessage("newScreenName", sn)
        session$sendCustomMessage("updateYourYearTitle", list(screenName = sn, year = THIS_YEAR))
        rv$last <- list(
          count = rv$tweets$n,
          likes = rv$tweets$favorite_count,
          retweets = rv$tweets$retweet_count
        )
        ts$error <- FALSE
        rv$tweets <- ts
      }
    } else {
      session$sendCustomMessage("show", list(show = FALSE))
      session$sendCustomMessage("show", list(show = TRUE, id = "error-bad-user"))
      rv$tweets <- list(error = TRUE, rate_limit = FALSE, msg = tw$error)
      drop_cache(get_tweets)(input$screen_name)
    }
  })

  output$count_tweets <- countup::renderCountup({
    req(!rv$tweets$error)
    countup::countup(
      count = rv$tweets$n,
      start_at = rv$last$count,
      options = list(suffix = if (!rv$tweets$has_tweet_prior_year) "+")
    )
  })
  outputOptions(output, "count_tweets", suspendWhenHidden = FALSE)

  output$count_likes <- countup::renderCountup({
    req(!rv$tweets$error)
    countup::countup(rv$tweets$favorite_count, start_at = rv$last$likes)
  })
  outputOptions(output, "count_likes", suspendWhenHidden = FALSE)

  output$count_retweets <- countup::renderCountup({
    req(!rv$tweets$error)
    countup::countup(rv$tweets$retweet_count, start_at = rv$last$retweets)
  })
  outputOptions(output, "count_retweets", suspendWhenHidden = FALSE)

  output$most_liked <- renderUI({
    req(!rv$tweets$error)
    url <- rv$tweets$best_favorite$url
    req(url)
    tagList(
      tags$div(
        class = "best-tweet-stat",
        tags$div(
          class = "best-tweet-stat-fav",
          icon("heart"),
          rv$tweets$best_favorite$favorite_count
        ),
        tags$div(
          class = "best-tweet-stat-rt",
          icon("recycle"),
          rv$tweets$best_favorite$retweet_count
        )
      ),
      tweetrmd::tweet_embed(url, omit_script = FALSE, theme = "dark")
    )
  })
  outputOptions(output, "most_liked", suspendWhenHidden = FALSE)

  output$most_retweeted <- renderUI({
    req(!rv$tweets$error)
    url <- rv$tweets$best_retweet$url
    req(url)
    if (url == rv$tweets$best_favorite$url) {
      session$sendCustomMessage("joinLikedRetweeted", TRUE)
    } else {
      session$sendCustomMessage("joinLikedRetweeted", FALSE)
    }
    tagList(
      tags$div(
        class = "best-tweet-stat",
        tags$div(
          class = "best-tweet-stat-fav",
          icon("heart"),
          rv$tweets$best_retweet$favorite_count
        ),
        tags$div(
          class = "best-tweet-stat-rt",
          icon("recycle"),
          rv$tweets$best_retweet$retweet_count
        )
      ),
      tweetrmd::tweet_embed(url, omit_script = FALSE, theme = "dark")
    )
  })
  outputOptions(output, "most_retweeted", suspendWhenHidden = FALSE)

  output$most_mentioned <- renderTable({
    req(!rv$tweets$error)
    mentions <- rv$tweets$mentions
    validate(need(length(mentions) > 0, glue::glue("@{input$screen_name} didn't mention anyone else.")))
    req(mentions[[1]])
    x <- data.frame(
      screen_name = purrr::map_chr(mentions, "screen_name"),
      count = purrr::map_int(mentions, "count")
    )
    x$screen_name <- glue::glue(
      '<a href="#{x$screen_name}">{x$screen_name}</a>'
    )
    names(x) <- c("Mentioned", "Times")
    x
  }, rownames = FALSE, sanitize.text.function = function(x) x, width = "100%", align = "lc")
  outputOptions(output, "most_mentioned", suspendWhenHidden = FALSE)

  output$most_hashtagged <- renderTable({
    req(!rv$tweets$error)
    hashtags <- rv$tweets$hashtags
    validate(need(length(hashtags) > 0, glue::glue("@{input$screen_name} didn't use any hashtags.")))
    req(hashtags[[1]])
    x <- data.frame(
      hashtag = purrr::map_chr(hashtags, "hashtag"),
      count = purrr::map_int(hashtags, "count")
    )
    x$hashtag <- glue::glue(
      '<a href="https://twitter.com/hashtag/{x$hashtag}">{x$hashtag}</a>'
    )
    names(x) <- c("Hashtag", "Count")
    x
  }, rownames = FALSE, sanitize.text.function = function(x) x, width = "100%", align = "lc")
  outputOptions(output, "most_hashtagged", suspendWhenHidden = FALSE)

  output$tweets_calendar <- renderMetricsgraphics({
    req(!rv$tweets$error, !is.null(rv$tweets$calendar))

    rv$tweets$calendar %>%
      mjs_plot(x = date, y = n_tweets, left = 28, right = 14, top = 0, bottom = 28) %>%
      mjs_line(area = TRUE, color = "var(--bs-pink)") %>%
      mjs_axis_x(xax_format = "date") %>%
      mjs_add_mouseover(
        "function(d, i) {
           $('#tweets_calendar svg .mg-active-datapoint')
             .text(d.date.toDateString() + ' - ' + d.n_tweets + ' tweets');
         }")
  })

  output$favorite_day <- renderUI({
    req(!rv$tweets$error, !is.null(rv$tweets$dow))

    rv$tweets$dow %>%
      slice_max(n_tweets, n = 1, with_ties = FALSE) %>%
      pull(day)
  })

  output$streak <- renderUI({
    req(!rv$tweets$error, !is.null(rv$tweets$streak))

    top <- rv$tweets$streak[1, ]
    days <- ngettext(top$n, "day", "days")

    paste(top$n, days)
  })

  output$count_days <- countup::renderCountup({
    req(!rv$tweets$error, !is.null(rv$tweets$days_year))

    countup::countup(rv$tweets$days_year$tweeted, start_at = 0)
  })
  outputOptions(output, "count_days", suspendWhenHidden = FALSE)
}

shinyApp(ui, server, enableBookmarking = "url")
