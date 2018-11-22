commentScraper <- function(subreddit,
                           startDay,
                           stopDay,
                           numComPerDay,
                           hashtag1,
                           hashtag2,
                           tweetCount) {
  dir.create(paste(subreddit, sep = ""))
  
  mainUrl <-
    paste(
      "https://api.pushshift.io/reddit/search/comment/?subreddit=",
      subreddit,
      "&sort_type=created_utc&size=",
      as.character(numComPerDay),
      "&after=",
      sep = ""
    )
  urls <-
    paste(mainUrl,
          sapply(seq(startDay + 1, stopDay + 1), as.numeric),
          "d&before=",
          sapply(seq(startDay, stopDay), as.numeric),
          "d",
          sep = "")
  
  pushshift <- ""
  
  for (i in seq_along(urls)) {
    tab <- read_html(urls[i])
    data <-
      tab %>% html_text() %>% str_extract_all("(?<=body\": \")[:print:]*,") %>%
      paste(collapse = " ") %>% str_sub(4,-1)
    
    pushshift <- paste(pushshift, data, sep = " ")
  }
  pushshift <- str_remove_all(pushshift, c("\"|\\,|[(]|[)]|\\\\"))
  
  
  for (i in seq_along(pushshift)) {
    write(
      pushshift[[i]],
      paste(subreddit, "/", subreddit, "Comments.txt", sep = ""),
      append = TRUE,
      ncolumns = 1000
    )
  }
  
  cleanedReddit <-
    read_file(paste(subreddit, "/", subreddit, "Comments.txt", sep = "")) %>% iconv("latin1", "ASCII", sub =
                                                                                      "")
  
  tweetdf1 <-
    search_tweets(
      hashtag1,
      n = tweetCount,
      lang = "en",
      include_rts = FALSE,
      retryonratelimit = TRUE
    )
  tweetdf2 <-
    search_tweets(
      hashtag2,
      n = tweetCount,
      lang = "en",
      include_rts = FALSE,
      retryonratelimit = TRUE
    )
  for (i in seq_along(tweetdf1$user_id)) {
    write(
      tweetdf1[[i, 5]],
      paste(subreddit, "/", subreddit, "Tweets_uncleaned.txt", sep = ""),
      append = TRUE,
      ncolumns = 1000
    )
  }
  for (i in seq_along(tweetdf2$user_id)) {
    write(
      tweetdf2[[i, 5]],
      paste(subreddit, "/", subreddit, "Tweets_uncleaned.txt", sep = ""),
      append = TRUE,
      ncolumns = 1000
    )
  }
  tweetComments <-
    read_file(paste(subreddit, "/", subreddit, "Tweets_uncleaned.txt", sep = "")) %>% iconv("latin1", "ASCII", sub =
                                                                                              "")
  cleanedTweets <- str_remove_all(
    tweetComments,
    "<[:print:]*>|\\bhttps[:print:]*\\b|@[:print:]*\\b|#[:print:]*\\b"
  )
  
  write(
    cleanedTweets,
    paste(subreddit, "/", subreddit, "Tweets_cleaned.txt", sep = ""),
    ncolumns = 1000
  )
  
  lengthReddit <- wordcount(cleanedReddit)
  lengthTwitter <- wordcount(cleanedTweets)
  goal <- 1500000
  eachCount <- goal / 2
  
  finalTxt <-
    str_c(
      "There are",
      as.character(lengthReddit),
      "words from Reddit and",
      as.character(lengthTwitter),
      "words from Twitter.   ",
      cleanedReddit,
      cleanedTweets,
      sep = " "
    )
  
  write(finalTxt,
        paste(subreddit, "/", subreddit, "Data.txt", sep = ""),
        ncolumns = 1000)
}
