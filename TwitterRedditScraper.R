# Scraps Reddit (Pushshift) and Twitter (twitteR) data based on inputs

commentScraper <- function(subreddit,
                           startDay,
                           stopDay,
                           numComPerDay,
                           hashtag1,
                           hashtag2,
                           tweetCount) {
  dir.create(paste(subreddit, sep = ""))
  
  # Every Pushshift API call requires a specific URL that depends on the subreddit name, as well as the desired collection time frame.  
  # Due to API call restrictions, we need to call the data for each day in the desired time frame.  The data can NOT be collected all at once. 
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
  
  
  # The following section actually calls the API and retrieves the data.  There probably is a better way of cleaning what is obtained, but this was sufficient for our purposes.
  pushshift <- ""
  
  for (i in seq_along(urls)) {
    tab <- read_html(urls[i])
    data <-
      tab %>% html_text() %>% str_extract_all("(?<=body\": \")[:print:]*,") %>%
      paste(collapse = " ") %>% str_sub(4,-1)
    
    pushshift <- paste(pushshift, data, sep = " ")
  }
  pushshift <- str_remove_all(pushshift, c("\"|\\,|[(]|[)]|\\\\"))
  
  # Save the content scraped to a txt file.  
  for (i in seq_along(pushshift)) {
    write(
      pushshift[[i]],
      paste(subreddit, "/", subreddit, "Comments.txt", sep = ""),
      append = TRUE,
      ncolumns = 1000
    )
  }
  
  # This is a redudant section of code.  However, it is useful when there are several different txt files being used.  
  cleanedReddit <-
    read_file(paste(subreddit, "/", subreddit, "Comments.txt", sep = "")) %>% iconv("latin1", "ASCII", sub =
                                                                                      "")
  
  # We switch now to scraping Twitter data, using the twitteR package.
  # At the present, we can search for up to two hashtags at a given time (for instance, NEU and Northeastern).  
  # In the future, it would be nice to be able to select any number of hashtags.
  # Unlike the Pushshift API, there is not technically a rate cap.  However, the 'n' argument is restricted.  For more info, see the twitteR documentation.
  
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
  
  # Write all tweet contents to a txt file.
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
  
  # Pull the tweets back into R and clean the data.  
  tweetComments <-
    read_file(paste(subreddit, "/", subreddit, "Tweets_uncleaned.txt", sep = "")) %>% iconv("latin1", "ASCII", sub =
                                                                                              "")
  cleanedTweets <- str_remove_all(
    tweetComments,
    "<[:print:]*>|\\bhttps[:print:]*\\b|@[:print:]*\\b|#[:print:]*\\b"
  )
  
  # Write the cleaned tweets to a new txt file.  
  write(
    cleanedTweets,
    paste(subreddit, "/", subreddit, "Tweets_cleaned.txt", sep = ""),
    ncolumns = 1000
  )
  
  #Finally, collect all of the comments from both Twitter and Reddit, and combine into a single txt file.  
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
