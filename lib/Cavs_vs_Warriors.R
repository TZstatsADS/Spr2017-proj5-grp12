source("lib/Authenticate.R")
library(twitteR)
library(httr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(tidytext)
library(plyr)
library(stringr)


#get latest tweets referencing "Golden State Warriors"
warriors_all <- searchTwitter("Golden State Warriors",lang="en",n=3000)
warriors_all_df <- tbl_df(map_df(warriors_all,as.data.frame))
#get latest tweets from official @warriors account
warriors_official <- userTimeline("warriors",n=3000)
warriors_official_df <- tbl_df(map_df(warriors_official,as.data.frame))
#get latest tweets referencing "Cleveland Cavaliers
cavs_all <- searchTwitter("Cleveland Cavaliers", lang="en",n=3000)
cavs_all_df <- tbl_df(map_df(cavs_all,as.data.frame))
#get latest tweets from official @cavs account
cavs_official <- userTimeline("cavs",n=3000)
cavs_official_df <- tbl_df(map_df(cavs_official,as.data.frame))


warriorsO_tweets <- warriors_official_df %>%
  select(id, text, created, favoriteCount, retweetCount, location) 

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
warriorsO_words <- warriorsO_tweets %>%
  select(text) %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) 

warriorsO_word_ct <- count(warriorsO_words$word)
names(warriorsO_word_ct) <- c("word","count")

warriorsO_word_ct_bar <- ggplot(data=warriorsO_word_ct[warriorsO_word_ct$count>7,]) + 
  geom_bar(aes(x=word,y=count),stat="identity",fill="blue") + coord_flip() +
  labs(title="Tweets @warriors", subtitle="breakdown by word count")

save(warriorsO_word_ct_bar,file="output/warriorsO_word_ct_bar.Rdata")

warriorsO_byHr <- warriorsO_tweets %>%
  count(hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent)) +
  geom_line(color="blue") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "", title="Official Tweets @warriors",
       subtitle="Breakdown by Hour")

save(warriorsO_byHr,file="output/warriorsO_byHr.Rdata")

warriorsAll_tweets <- warriors_all_df %>%
  select(id, text, created, favoriteCount, retweetCount, location) 

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
warriorsAll_words <- warriorsAll_tweets %>%
  select(text) %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) 

warriorsAll_byHr <- warriorsAll_tweets %>%
  count(hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent)) +
  geom_line(color="blue") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "", title="All Tweets about Warriors",
       subtitle="Breakdown by Hour")

save(warriorsAll_byHr,file="output/warriorsAll_byHr.Rdata")

cavsO_tweets <- cavs_official_df %>%
  select(id, text, created, favoriteCount, retweetCount, location) 


reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
cavsO_words <- cavsO_tweets %>%
  select(text) %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) 

cavsO_word_ct <- count(cavsO_words$word)
names(cavsO_word_ct) <- c("word","count")

cavsO_word_ct_bar <- ggplot(data=cavsO_word_ct[cavsO_word_ct$count>3,]) + 
  geom_bar(aes(x=word,y=count),stat="identity",fill="red") + coord_flip() +
  labs(title="Tweets @cavs", subtitle="breakdown by word count")

save(cavsO_word_ct_bar,file="output/cavsO_word_ct_bar.Rdata")

cavsO_byHr <- cavsO_tweets %>%
  count(hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent)) +
  geom_line(color="red") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "", title="Official Tweets @cavs",
       subtitle="Breakdown by Hour")

save(cavsO_byHr,file="output/cavsO_byHr.Rdata")

cavsAll_tweets <- cavs_all_df %>%
  select(id, text, created, favoriteCount, retweetCount, location) 

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
cavsAll_words <- cavsAll_tweets %>%
  select(text) %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) 

cavsAll_byHr <- cavsAll_tweets %>%
  count(hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent)) +
  geom_line(color="red") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "", title="All Tweets about Cavaliers",
       subtitle="Breakdown by Hour")

save(cavsAll_byHr, file="output/cavsAll_byHr.Rdata")




#laply(warriors_all,function(t) t$get)

warriorsO.df=twListToDF(warriors_official)
warriorsO.df$screenName <- paste0("@",warriorsO.df$screenName)
warriorsAll.df=twListToDF(warriors_all)
warriorsAll.df$screenName <- "other users \non warriors"
cavsO.df=twListToDF(cavs_official)
cavsO.df$screenName <- paste0("@",cavsO.df$screenName)
cavsAll.df=twListToDF(cavs_all)
cavsAll.df$screenName <- "other users \non cavs"

war.games <- data.frame(created=as.POSIXlt(c("2017-04-16 15:30:00 EDT",
                                  "2017-04-19 22:30:00 EDT",
                                  "2017-04-22 22:30:00 EDT",
                                  "2017-04-24 22:30:00 EDT"),"EST"),
                        screenName=rep("warriors",4),stringsAsFactors = F)
cavs.games <- data.frame(created=as.POSIXlt(c("2017-04-15 15:00:00 EDT",
                                             "2017-04-17 19:00:00 EDT",
                                             "2017-04-20 19:00:00 EDT",
                                             "2017-04-23 13:00:00 EDT"),"EST"),
                        screenName=rep("cavs",4),stringsAsFactors = F)


war_cavs_created <- ggplot(rbind(warriorsO.df,cavsO.df,warriorsAll.df,cavsAll.df))+
  geom_point(aes(x=created,y=screenName)) +
  geom_point(data=war.games,aes(created,paste("@",screenName,sep=""),colour="warriors"),shape=7) +
  geom_point(data=cavs.games,aes(created,paste("@",screenName,sep=""),colour="cavs"),shape=7) +
  labs(title="@warriors vs. @cavs", subtitle="Tweets by date created" ,
       y="Twitter User") +
  scale_colour_manual(name="Legend",
                      values=c(cavs="red", warriors="blue"),
                      labels=c("Cavs Playoffs \nR.1 Games",
                               "Warriors Playoffs \nR.1 Games"))
save(war_cavs_created,file="output/war_cavs_created.Rdata")


#############################################################################
## wordcloud

feed_warriorsAll <- laply(warriors_all,function(t) t$getText())
feed_cavsAll <- laply(warriors_all,function(t) t$getText())

#clean text from tweets
feed_warriorsAll = iconv(feed_warriorsAll, "latin1", "ASCII", "byte")
feed_warriorsAll <- gsub(" http.*","",feed_warriorsAll)
feed_warriorsAll <- gsub("[^\\@^\\#[:^punct:]]", "", feed_warriorsAll, perl=T)

feed_cavsAll = iconv(feed_cavsAll, "latin1", "ASCII", "byte")
feed_cavsAll <- gsub(" http.*","",feed_cavsAll)
feed_cavsAll <- gsub("[^\\@^\\#[:^punct:]]", "", feed_cavsAll, perl=T)

#create corpus 
warriorsAll_corpus <- Corpus(VectorSource(feed_warriorsAll))
cavsAll_corpus <- Corpus(VectorSource(feed_cavsAll))

#clean up
warriorsAll_corpus <- tm_map(warriorsAll_corpus,content_transformer(tolower))
warriorsAll_corpus <- tm_map(warriorsAll_corpus,function(x)removeNumbers(x))
warriorsAll_corpus <- tm_map(warriorsAll_corpus,function(x)removeWords(x,c("golden","state",
                                                                           "warriors",
                                                                           stopwords())))
png("figs/WarriorsCloud.png", width=5, height=5, units="in", res=300)
wordcloud(warriorsAll_corpus,colors = brewer.pal(8,name="Blues")[4:8],max.words = 300)
dev.off()

cavsAll_corpus <- tm_map(cavsAll_corpus,content_transformer(tolower))
cavsAll_corpus <- tm_map(cavsAll_corpus,function(x)removeNumbers(x))
cavsAll_corpus <- tm_map(cavsAll_corpus,function(x)removeWords(x,c("cleveland","cavs",
                                                                   "cavaliers",stopwords())))
png("figs/CavsCloud.png", width=5, height=5, units="in", res=300)
wordcloud(cavsAll_corpus,colors = brewer.pal(8,name="Reds")[4:8],max.words = 300)
dev.off()

################################################################################
##sentiment analysis
#NRC Word-Emotion Association lexicon, available fromtidytext package 
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)
nrc <- as.data.frame(nrc)

warriorsAll_senti <- warriorsAll_words %>%
  inner_join(nrc, by = "word") %>%
  ungroup()
warriorsAll_senti <- aggregate(warriorsAll_senti$word,
                               by=list(warriorsAll_senti$sentiment),
                               length)
names(warriorsAll_senti) <- c("sentiment","num_words")
save(warriorsAll_senti,file="output/warriorsAll_senti.Rdata")

cavsAll_senti <- cavsAll_words %>%
  inner_join(nrc, by = "word") %>%
  ungroup() 

cavsAll_senti <- aggregate(cavsAll_senti$word,by=list(cavsAll_senti$sentiment),length)
names(cavsAll_senti) <- c("sentiment","num_words")
save(cavsAll_senti,file="output/cavsAll_senti.Rdata")




########################################################################
##most common positive words in tweets about Cavs and Warriors
nrc_pos <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

warriorsAll_neg <- warriorsAll_words %>%
  semi_join(nrc_pos, by="word") %>%
  count()

warriorsAll_pos <- warriorsAll_pos[order(warriorsAll_pos$freq,decreasing = T),]
save(warriorsAll_pos,file="output/warriorsAll_pos.Rdata")

cavsAll_pos <- cavsAll_words %>%
  semi_join(nrc_pos, by="word") %>%
  count()

cavsAll_pos <- cavsAll_pos[order(cavsAll_pos$freq,decreasing = T),]
save(cavsAll_pos,file="output/cavsAll_pos.Rdata")

###########################################################################
##most common negative words in tweets about Cavs and Warriors

nrc_neg <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

warriorsAll_neg <- warriorsAll_words %>%
  semi_join(nrc_neg, by="word") %>%
  count()

warriorsAll_neg <- warriorsAll_neg[order(warriorsAll_neg$freq,decreasing = T),]
save(warriorsAll_neg,file="output/warriorsAll_neg.Rdata")

cavsAll_neg <- cavsAll_words %>%
  semi_join(nrc_neg, by="word") %>%
  count()

cavsAll_neg <- cavsAll_neg[order(cavsAll_neg$freq,decreasing = T),]
save(cavsAll_neg,file="output/cavsAll_neg.Rdata")
