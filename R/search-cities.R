
wmax <- function(x, n = 1) sort(tfse::na_omit(x), decreasing = TRUE)[n]
wmin <- function(x, n = 1) sort(tfse::na_omit(x), decreasing = FALSE)[n]

combine_rows <- function(...) {
  dots <- list(...)
  if (length(dots) == 1 && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }
  do.call("rbind", dots)
}

comma_nums <- function(x, digits = 2) formatC(round(x, digits), big.mark = ",")

search_tweets_geo <- function(search_query = NULL) {
  x <- vector("list", nrow(cities))
  tfse::print_start("Searching for tweets near ", comma_nums(nrow(cities)), " cities")
  for (i in seq_len(nrow(cities))) {
    if (is.null(search_query)) {
      x[[i]] <- rtweet::search_tweets(
        geocode = cities$coords[i], verbose = FALSE,
        include_rts = FALSE)
    } else {
      x[[i]] <- rtweet::search_tweets(search_query,
        geocode = cities$coords[i], verbose = FALSE,
        include_rts = FALSE)
    }
    m <- paste0("Found ", nrow(x[[i]]), " tweets in ",
      cities$city[i], ", ", cities$state[i])
    pct <- paste0(round(i / nrow(cities) * 100, 1), "%")
    sp <- 80 - nchar(pct) - nchar(m)
    sp <- paste(rep(" ", sp), collapse = "")
    m <- paste0(m, sp, pct)
    tfse::print_complete(m)
    if (nrow(x[[i]]) > 0) {
      x[[i]]$state <- cities$state[i]
      x[[i]]$city <- cities$city[i]
    }
  }
  x <- combine_rows(x)
  ## estimate sentiment of each tweet
  x$sent <- syuzhet::get_sentiment(cleanuptweets(x$text), "afinn")
  x
}

cleanuptweets <- function(x) {
  x <- textfeatures:::text_cleaner(x)
  vapply(tokenizers::tokenize_tweets(x), function(x) paste(x, collapse = " "),
    character(1))
}
