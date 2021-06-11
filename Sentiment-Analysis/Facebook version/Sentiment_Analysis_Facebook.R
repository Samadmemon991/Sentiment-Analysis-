install.packages("Rfacebook")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")

library(Rfacebook)
library(plyr)
library(stringr)
library(ggplot2)

fb_auth <- fbOAuth(app_id="119027498778445", 
                    app_secret="53cc2540b464f0c3f52ae9e5f09aa4b2", 
                    extended_permissions = TRUE) 


fb_page <- getPage(page="manchesterunited", token=fb_auth, n = 10,  feed = FALSE, 
              reactions = TRUE,  verbose = TRUE, api = NULL)

View(fb_page)

fb_post <- getPost(post = fb_page$id[6], n=1000, token=fb_auth)
fb_post_df <- as.data.frame(fb_post[3])
View(fb_post_df)


comments_data <- sapply(fb_post_df$comments.message,function(row) iconv(row, "latin1", "ASCII", sub=""))
comments_data <- gsub("@\\w+", "", comments_data)
comments_data <- gsub("#\\w+", '', comments_data)
comments_data <- gsub("RT\\w+", "", comments_data)
comments_data <- gsub("http.*", "", comments_data)
comments_data <- gsub("RT", "", comments_data)
comments_data <- sub("([.-])|[[:punct:]]", "\\1", comments_data)
comments_data <- sub("(['])|[[:punct:]]", "\\1", comments_data)

View(comments_data)

pos<-readLines("positive_words.txt")
neg<-readLines("negative_words.txt")

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

score_fb <- score.sentiment(comments_data, pos, neg, .progress='text')
View(score_fb)

summary(score_fb)

score_fb$score_chr <- as.character(score_fb$score)

score_fb$score_chr <- gsub("^0$", "Neutral", score_fb$score_chr)
score_fb$score_chr <- gsub("^1$|^2$|^3$|^4$", "Positive", score_fb$score_chr)
score_fb$score_chr <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$", "Very Positive", score_fb$score_chr)
score_fb$score_chr <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", score_fb$score_chr)
score_fb$score_chr <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", score_fb$score_chr)

View(score_fb)

score_fb$score_chr<-as.factor(score_fb$score_chr)

Viz1 <- ggplot(score_fb, aes(x=score_chr))+geom_bar()
Viz1

write.csv(score_fb, file = "manchester_united_sentiment_score.csv")
