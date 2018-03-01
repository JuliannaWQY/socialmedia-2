#Get 10000 tweets with the hashtag of SuperBowl

if (!require("rtweet")) install.packages("rtweet", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
library("rtweet")
library("wordcloud")
library("tm")

appname <- "julianna_wqy"
consumerKey <- "RGiGCHHf2TtKl5nTW9QpyjHiZ"
consumerSecret <- "pZ827K5BlEQaWKxiCusiLai5PKPzPX9YISJV64KbcvghpXT8NU"

twitter_token <- create_token(app = appname, consumer_key = consumerKey, consumer_secret = consumerSecret, set_renv = TRUE)

SB <- search_tweets("#SuperBowl", n = 10000, include_rts = FALSE, retryonratelimit = TRUE)
class(SB)

all_text <- SB$text

#Process all the text of the tweets
Preprocessing <- function(doc){
  doc.corpus <- Corpus(VectorSource(doc))
  doc.corpus <- tm_map(doc.corpus, function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",x))
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, function(x)removeWords(x,stopwords("english")))
  doc.corpus <- tm_map(doc.corpus, removeWords, c("superbowl","SuperBowl","super","bowl","amp"))
  return(doc.corpus)
}

textt <- paste(all_text,collapse=" ")
textt.p <- Preprocessing(textt)
substr(textt.p$content,1,200)

tdm <- TermDocumentMatrix(Corpus(VectorSource(c(textt.p$content)))) 
tdm <- as.matrix(tdm) 
colnames(tdm) <- c("All_text")

Num_of_terms_shown <- 5
par(mfrow=c(1,2)) # 1x2 panel plot
barplot(tdm[order(tdm[,"All_text"], decreasing=TRUE)[1:Num_of_terms_shown],"All_text"],col="#004e6d", cex.names = 0.8, horiz=FALSE)
title("Words most frequently used when #SuperBowl on Twitter")

min.freq <- 100
par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))
par(bg="white")
par(col.main="black") 
wordcloud(textt.p, scale=c(6,0.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(8, "Accent"))   
title("Word cloud of words trending with #SuperBowl")

#Get the most retweeted tweets with #SuperBowl
for (index in 1:nrow(SB)){
  if (SB$retweet_count[index]>35){
    print(paste(SB$screen_name[index],SB$text[index],sep=":"))
  }   
}

#Compare the word cloud of #eagles and #patriots
EG <- search_tweets("#eagles", n = 5000, include_rts = FALSE)
PA <- search_tweets("#patriots", n = 5000, include_rts = FALSE)

EG_text <- EG$text
PA_text <- PA$text

Preprocessing_2 <- function(doc){
  doc.corpus <- Corpus(VectorSource(doc))
  doc.corpus <- tm_map(doc.corpus, function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",x))
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, function(x)removeWords(x,stopwords("english")))
  doc.corpus <- tm_map(doc.corpus, removeWords, c("superbowl","SuperBowl","super","bowl","amp","eagles","patriots"))
  return(doc.corpus)
}

EG_1 <- paste(EG_text,collapse=" ")
EG.p <- Preprocessing_2(EG_1)
PA_1 <- paste(PA_text,collapse=" ")
PA.p <- Preprocessing_2(PA_1)

min.freq <- 100
par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))
par(bg="white")
par(col.main="black") 
wordcloud(EG.p, scale=c(4,0.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(8, "Accent"))   
wordcloud(PA.p, scale=c(4,0.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(8, "Accent")) 

# Compare four major sports leagues' popularity in the US using NYT api
library(RJSONIO)
library(RCurl)
library(plotly)
api <- "2bc8ff73b9bc44b39aacb22a290da618"

year_range <- 1990:2017 

SearchNYT <- function(sq){ 
  nyt <- data.frame(year=character(0),hits=numeric(0))
  for(year in year_range){
    url <- paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',sq,'&begin_date=',year,'0101&end_date=',year,'1231&api-key=',api,sep="")
    nyt_robj <- fromJSON(getURL(url)) 
    hits <- nyt_robj$response$meta["hits"] 
    nyt <- rbind(nyt,data.frame(year=year,hits=hits)) 
    print(paste(year,hits,sep=":"))
    Sys.sleep(5)
  }
  return(nyt)
}

search_q <- URLencode("'Super Bowl'")
nyt_SB <- SearchNYT(search_q)
search_q <- URLencode("'NBA'")
nyt_NBA <- SearchNYT(search_q)
search_q <- URLencode("'MLB'")
nyt_MLB <- SearchNYT(search_q)
search_q <- URLencode("'NHL'")
nyt_NHL <- SearchNYT(search_q)

p <- plot_ly(x = nyt_SB$year, y = nyt_SB$hits, name = "Super Boal", type = 'scatter', mode = 'lines')
p <- add_trace(p, y = nyt_NBA$hits, name = "NBA")
p <- add_trace(p, y = nyt_MLB$hits, name = "MLB (baseball)")
p <- add_trace(p, y = nyt_NHL$hits, name = "NHL (hockey)")
layout(p, title = "Country's Names mentioned in New York Times (1990 to 2017)", xaxis = list(title = "Year"), yaxis = list (title = "Number of hits"))

Sys.setenv("plotly_username"="JuliannaWQY")
Sys.setenv("plotly_api_key"="iGWVxE2ongwlSlviJZKZ")
api_create(p, filename = "assignment")
