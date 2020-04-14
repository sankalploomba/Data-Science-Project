library(twitteR)
library(tidyverse)
library(plyr)
require('ROAuth')

#Sentiment Scoring Function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  scores <- laply(sentences, 
                  function(sentence, pos.words, neg.words)
                  {
                    sentence <- gsub('[[:punct:]]', "", sentence)
                    sentence <- gsub('[[:cntrl:]]', "", sentence)
                    sentence <- gsub('\\d+', "", sentence)
                    sentence <- tolower(sentence)
                    word.list <- str_split(sentence, '\\s+')
                    words <- unlist(word.list, use.names = FALSE)
                    pos.matches <- match(words , pos.words)
                    neg.matches <- match(words , neg.words)
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    score <- sum(pos.matches) - sum(neg.matches)
                    
                    if(score > 0)
                      category = "Positive"
                    
                    else if(score < 0)
                      category = "Negative"
                    
                    else
                      category = "Neutral"
                    
                    x <- list(score, category, sentence)
                    return(x)
                  },
                  pos.words,
                  neg.words,
                  .progress = .progress)
  
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}


#Establishing Connection
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "h033QVqWcY4yDeSLLJLtLqIVo"
consumerSecret <- "23C402jqLoBylrXw9itFmta8cEWi6oYNFQn7P092Bajs1P90Jm"
accessToken <- "435790326-RKIZuhOXgDMmcnwGnPROIrch4IjgMBT9UP3SM5gp"
accessTokenSecret <- "teDzfu4bTDi6qG2bVCD5NqtNsEQ2wa7VEqbUNkQzJMODe"

twitCred <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = reqURL,
                             accessURL = accessURL,
                             authURL = authURL)

#Run the following lines one by one and follow the instructions displayed on console
twitCred$handshake()
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)


#Loading List of words
pos.words <- scan('/home/sankalp/Desktop/Data Sc/attachments/positive_words.txt', what = 'character', comment.char = ';')
neg.words <- scan('/home/sankalp/Desktop/Data Sc/attachments/negative_words.txt', what = 'character', comment.char = ';')


#Fetching Tweets
tweet1 <- searchTwitter("(#india OR #India OR #INDIA) AND (#lockdown OR #Lockdown OR #LOCKDOWN OR #lockdown2 OR #Lockdown2 OR #LOCKDOWN2 OR #lockdown2.0 OR #Lockdown2.0 OR #LOCKDOWN2.0) AND (#extend OR #Extend OR #EXTEND OR extend OR Extend OR EXTEND OR extended OR Extended OR EXTENDED)", n = 1000, lang = "en")
tweet_df <- tbl_df(map_df(tweet1, as.data.frame))


#Sentiment Analysis 
bscore <- score.sentiment((tweet_df$text), pos.words, neg.words, .progress='text')


#Plots
hist(as.numeric(bscore$score.1), main="Histogram of no. of tweets corresponding to scores", labels = TRUE, xlab = "Sentiment Score")

qplot(
      x=as.numeric(bscore$score.1),
      geom = "histogram",
      binwidth = 1,
      main = "Histogram for Tweets", 
      xlab = "Sentiment Score",
      ylab = "Number of Tweets",
      fill = I("orange"), 
      col = I("black")
      )

#Result
positive = count(bscore$score.1 > 0)
negative = count(bscore$score.1 < 0)
neutral = count(bscore$score.1 == 0)

positive
neutral
negative
