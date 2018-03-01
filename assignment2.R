#assignment 2
#sentiment analysis for tweets that @BarackObama vs @realDonaldTrump

BO <- search_tweets("@BarackObama", n = 10000, include_rts = FALSE, retryonratelimit = TRUE)
DT <- search_tweets("@realDonaldTrump", n = 10000, include_rts = FALSE, retryonratelimit = TRUE)
library(tidyverse) 
library(tidytext) 
library(rtweet)  
library(stringr) 
library(plotly)  
library(SnowballC)
library(wordcloud)
library(tm)

sentiment_term <- get_sentiments("bing")
head(sentiment_term)

BO_words <- strsplit(BO$text,' ')
BO_words_sent <- sapply(BO_words,function(z){sentiment_term$sentiment[sentiment_term$word %in% z]
})
DT_words <- strsplit(DT$text,' ')
DT_words_sent <- sapply(DT_words,function(z){sentiment_term$sentiment[sentiment_term$word %in% z]
})
table(unlist(BO_words_sent))
table(unlist(DT_words_sent))

#Wordcloud for both
Preprocessing <- function(doc){
  doc.corpus <- Corpus(VectorSource(doc))
  doc.corpus <- tm_map(doc.corpus, function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",x))
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, function(x)removeWords(x,stopwords("english")))
  doc.corpus <- tm_map(doc.corpus, removeWords, c("realdonaldtrump","donaldtrump","donald","trump","amp"))
  return(doc.corpus)
}

textt <- paste(DT$text,collapse=" ")
textt.p <- Preprocessing(textt)
substr(textt.p$content,1,200)

min.freq <- 200
par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))
par(bg="white")
par(col.main="black") 
wordcloud(textt.p, scale=c(2,0.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(10, "Accent"))   
title("Trump")

Preprocessing_2 <- function(doc){
  doc.corpus <- Corpus(VectorSource(doc))
  doc.corpus <- tm_map(doc.corpus, function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",x))
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, function(x)removeWords(x,stopwords("english")))
  doc.corpus <- tm_map(doc.corpus, removeWords, c("obama","barackobama","amp"))
  return(doc.corpus)
}

textt2 <- paste(BO$text,collapse=" ")
textt2.p <- Preprocessing_2(textt2)
substr(textt2.p$content,1,200)

min.freq <- 200
par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))
par(bg="white")
par(col.main="black") 
wordcloud(textt2.p, scale=c(3,0.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(10, "Accent"))   
title("Obama")

# Word Stemming
sentiment_term$word <- wordStem(sentiment_term$word,"english")
sentiment_term <- sentiment_term[!duplicated(sentiment_term$word),] # remove duplicated terms
table(unlist(sapply(BO_words, function(z){sentiment_term$sentiment[sentiment_term$word %in% wordStem(z,"english")]})))
table(unlist(sapply(DT_words, function(z){sentiment_term$sentiment[sentiment_term$word %in% wordStem(z,"english")]})))

#Calculating the ratios
Find_total_terms <- function(nra_text){
  nra_words <- strsplit(nra_text,' ')
  tt <- sum(unlist(sapply(nra_words, length)))
  return(tt)
}

Find_pos_sentiments <- function(nra_text){
  nra_words <- strsplit(nra_text,' ')
  s <- table(unlist(sapply(nra_words, function(z){sentiment_term$sentiment[sentiment_term$word %in% wordStem(z,"english")]})))
  return(positive=s[2])
}

Find_neg_sentiments <- function(nra_text){
  nra_words <- strsplit(nra_text,' ')
  s <- table(unlist(sapply(nra_words, function(z){sentiment_term$sentiment[sentiment_term$word %in% wordStem(z,"english")]})))
  return(negative=s[1])
}

Find_sentimentscore <- function(nra_text){
  nra_words <- strsplit(nra_text,' ')
  s <- table(unlist(sapply(nra_words, function(z){sentiment_term$sentiment[sentiment_term$word %in% wordStem(z,"english")]})))
  score <- (s[2]-s[1])/(s[2]+s[1])
  return(score=score)
}

#The following code is clumpsy but I know no better way to do it...
tlt_b <- Find_total_terms(BO$text)
pos_b <- Find_pos_sentiments(BO$text)
neg_b <- Find_neg_sentiments(BO$text)
ssc_b <- Find_sentimentscore(BO$text)
tlt_d <- Find_total_terms(DT$text)
pos_d <- Find_pos_sentiments(DT$text)
neg_d <- Find_neg_sentiments(DT$text)
ssc_d <- Find_sentimentscore(DT$text)

name <- c("Barack Obama","Donald Trump")
tlt <- c(tlt_b,tlt_d)
pos_p <- c(pos_b / tlt_b, pos_d / tlt_d)
neg_p <- c(neg_b / tlt_b, neg_d / tlt_d)
ssc <- c(ssc_b,ssc_d)
sent_p <- c((pos_b + neg_b) / tlt_b , (pos_d + neg_d) / tlt_d)

df <- data.frame(name,tlt,pos_p,neg_p,ssc,sent_p)

# Plot % of Sentiment terms
df <- df[order(df$sent_p,decreasing=TRUE),]
p <- plot_ly(df, x = ~name, y = ~sent_p, name = ~name, type = 'bar')
p <- layout(p, title = "% of Sentiment Terms", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p

df <- df[order(df$neg_p,decreasing=TRUE),]
p <- plot_ly(df, x = ~name, y = ~neg_p, name = ~name, type = 'bar')
p <- layout(p, title = "% of Negative Sentiment Terms", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p

df <- df[order(df$pos_p,decreasing=TRUE),]
p2 <- plot_ly(df, x = ~name, y = ~pos_p, name = ~name, type = 'bar')
p2 <- layout(p2, title = "% of Positive Sentiment Terms", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p2

df <- df[order(df$ssc,decreasing=TRUE),]
p3 <- plot_ly(df, x = ~name, y = ~ssc, name = ~name, type = 'bar')
p3 <- layout(p3, title = "Sentiment Score", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p3

Sys.setenv("plotly_username"="JuliannaWQY")
Sys.setenv("plotly_api_key"="iGWVxE2ongwlSlviJZKZ")
api_create(p, filename = "assign2-1")
api_create(p2, filename = "assign2-2")

#Find out the sentiment of the two presidents on each of the eight states
state <- c("arizona","california","florida","massachusetts","north carolina","texas","virginia","washington dc")
all_BO <- c()
for(state_name in state){
  st <- search_tweets("@BarackObama", n=1000, include_rts = FALSE, lang="en", geocode = lookup_coords(state_name,"country:US")) 
  if (nrow(st)!=0){
    st$text <- gsub('[:punct:]',' ',st$text)   # removing all punctation
    st$text <- tolower(st$text)  # in lower case
    st$state_name <- state_name
    st$name <- "Barack Obama"
    all_BO <- rbind(all_BO,st)
    }
  Sys.sleep(10)
}
aggregate(text~state_name,all_BO,length)

all_DT <- c()
for(state_name in state){
  st <- search_tweets("@realDonaldTrump", n=1000, include_rts = FALSE, lang="en", geocode = lookup_coords(state_name,"country:US")) 
  if (nrow(st)!=0){
    st$text <- gsub('[:punct:]',' ',st$text)   # removing all punctation
    st$text <- tolower(st$text)  # in lower case
    st$state_name <- state_name
    st$name <- "Barack Obama"
    all_DT <- rbind(all_DT,st)
  }
  Sys.sleep(10)
}
aggregate(text~state_name,all_DT,length)

#making charts for the two presidents on both pos and neg sentiment
#Barack Obama
ad_tlt_b <- aggregate(text~state_name,all_BO,Find_total_terms)
ad_pos_b <- aggregate(text~state_name,all_BO,Find_pos_sentiments)
ad_neg_b <- aggregate(text~state_name,all_BO,Find_neg_sentiments)
ad_ssc_b <- aggregate(text~state_name,all_BO,Find_sentimentscore)
ad_ssc_b$sent_p <- (ad_pos_b$text + ad_neg_b$text) / ad_tlt_b$text
ad_ssc_b$possent_p <- ad_pos_b$text / ad_tlt_b$text
ad_ssc_b$negsent_p <- ad_neg_b$text / ad_tlt_b$text

ad_ssc_b <- ad_ssc_b[order(ad_ssc_b$negsent_p,decreasing=TRUE),]
ad_ssc_b$state_name <- factor(ad_ssc_b$state_name, levels = ad_ssc_b$state_name)
p_1 <- plot_ly(ad_ssc_b, x = ~state_name, y = ~negsent_p, name = ~state_name, type = 'bar')
p_1 <- layout(p_1, title = "% of Negative Sentiment Terms, Obama", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p_1
api_create(p_1, filename = "assign_BO_1")
ad_ssc_b <- ad_ssc_b[order(ad_ssc_b$possent_p,decreasing=TRUE),]
ad_ssc_b$state_name <- factor(ad_ssc_b$state_name, levels = ad_ssc_b$state_name)
p_2 <- plot_ly(ad_ssc_b, x = ~state_name, y = ~possent_p, name = ~state_name, type = 'bar')
p_2 <- layout(p_2, title = "% of Positive Sentiment Terms, Obama", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p_2
api_create(p_2, filename = "assign_BO_2")

# Donald Trump
ad_tlt_d <- aggregate(text~state_name,all_DT,Find_total_terms)
ad_pos_d <- aggregate(text~state_name,all_DT,Find_pos_sentiments)
ad_neg_d <- aggregate(text~state_name,all_DT,Find_neg_sentiments)
ad_ssc_d <- aggregate(text~state_name,all_DT,Find_sentimentscore)
ad_ssc_d$sent_p <- (ad_pos_d$text + ad_neg_d$text) / ad_tlt_d$text
ad_ssc_d$possent_p <- ad_pos_d$text / ad_tlt_d$text
ad_ssc_d$negsent_p <- ad_neg_d$text / ad_tlt_d$text

ad_ssc_d <- ad_ssc_d[order(ad_ssc_d$negsent_p,decreasing=TRUE),]
ad_ssc_d$state_name <- factor(ad_ssc_d$state_name, levels = ad_ssc_d$state_name)
p_3 <- plot_ly(ad_ssc_d, x = ~state_name, y = ~negsent_p, name = ~state_name, type = 'bar')
p_3 <- layout(p_3, title = "% of Negative Sentiment Terms, Trump", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p_3
api_create(p_3, filename = "assign_DT_1")
ad_ssc_d <- ad_ssc_d[order(ad_ssc_d$possent_p,decreasing=TRUE),]
ad_ssc_d$state_name <- factor(ad_ssc_d$state_name, levels = ad_ssc_d$state_name)
p_4 <- plot_ly(ad_ssc_d, x = ~state_name, y = ~possent_p, name = ~state_name, type = 'bar')
p_4 <- layout(p_4, title = "% of Positive Sentiment Terms, Trump", xaxis = list(title = "State"), yaxis = list (title = "Percentage"))
p_4
api_create(p_4, filename = "assign_DT_2")

#Find out where @ the two presidents the most
sort(table(DT$place_name),decreasing = TRUE)[1:10]
sort(table(BO$place_name),decreasing = TRUE)[1:10]