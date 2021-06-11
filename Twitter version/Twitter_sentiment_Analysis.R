install.packages("twitteR")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")

library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)

consumerKey <- "O2FmvX2WaegyWhmW9********"
consumerSecret <- "zelDESCaOFc2LvQoGSuta9ExIXhFLwSeERwEYQDT**********"
accessToken <- 	"1941789895-QLKNfqZinj8NsbKioY74tecH4nmly**********"
accessSecret <- "U8dD6zi6sflwKWQONAk3sN78mb0gf8ZPt23**********"

setup_twitter_oauth(consumer_key = consumerKey, 
                    consumer_secret = consumerSecret, 
                    access_token = accessToken, 
                    access_secret = accessSecret)

twitter.data <- searchTwitter("Manchester United", n=1000,  lang='en')
twitter.data.df <- read.csv("data.csv")

twitter.data.df <- twListToDF(twitter.data)

tweets.df <- as.data.frame(twitter.data.df$text)
colnames(tweets.df)[1]<-"text"

tweets.df <- sapply(tweets.df,function(row) iconv(row, "latin1", "ASCII", sub=""))

tweets.df <- gsub("@\\w+", "", tweets.df)
tweets.df <- gsub("#\\w+", '', tweets.df)
tweets.df <- gsub("RT\\w+", "", tweets.df)
tweets.df <- gsub("http.*", "", tweets.df)
tweets.df <- gsub("RT", "", tweets.df)
tweets.df <- sub("([.-])|[[:punct:]]", "\\1", tweets.df)
tweets.df <- sub("(['])|[[:punct:]]", "\\1", tweets.df)

View(tweets.df)

pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  scores <- laply(sentences,
                function(sentence, pos.words, neg.words)
                {
                  sentence <- gsub("[[:punct:]]", "", sentence)
                  sentence <- gsub("[[:cntrl:]]", "", sentence)
                  sentence <- gsub('\\d+', '', sentence)
                  sentence <- tolower(sentence)
                  
                  word.list <- str_split(sentence, "\\s+")
                  words <- unlist(word.list)
                  
                  pos.matches <- match(words, pos)
                  neg.matches <- match(words, neg)
                  
                  pos.matches <- !is.na(pos.matches)
                  neg.matches <- !is.na(neg.matches)
                  score <- sum(pos.matches) - sum(neg.matches)
                  return(score)
                }, pos.words, neg.words, .progress=.progress )
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}
scores_twitter <- score.sentiment(tweets.df, pos, neg, .progress='text')
View(scores_twitter)

summary(scores_twitter)

scores_twitter$score_chr <- as.character(scores_twitter$score)

scores_twitter$score_chr <- gsub("^0$", "Neutral", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^1$|^2$|^3$|^4$", "Positive", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$", "Very Positive", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", scores_twitter$score_chr)

View(scores_twitter)

scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)

Viz1 <- ggplot(scores_twitter, aes(x=score_chr))+geom_bar()
Viz1

write.csv(scores_twitter, file = "manchester_united_sentiment_score.csv")
