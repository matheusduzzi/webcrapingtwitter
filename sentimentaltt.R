# packs to this code
library(SentimentAnalysis)
library(tidyverse)
library(data.table)
library(tidytext)
library(glue)
library(stringr)
library(stringi)
library(readr)
library(ptstem)
library(wordcloud2)
library(rtweet)
library(dplyr)
library(tidyr)
library(readxl)
library(tm)
library(BatchGetSymbols)

# stop words
sw <- c("a","ainda","alem","ambas","ambos","antes","ao","aonde","aos","apos","aquele","aqueles","as","assim","com","como","contra","contudo","cuja","cujas","cujo","cujos","da","das","de","dela","dele","deles","demais","depois","desde","desta","deste","dispoe","dispoem","diversa","diversas","diversos","do","dos","durante","e","ela","elas","ele","eles","em","entao","entre","essa")
swList2 <- stopwords('portuguese')
glimpse(swList2)
sw_merged <- union(sw,swList2) 
summary(sw_merged)

# test for stop words
tibble(word = sw_merged) %>% 
  group_by(word) %>% 
  filter(n()>1)

# test data to negative and positive indicators
an <- read.table("adjetivos_negativos.txt", header = F, sep = "\t", strip.white = F,
               stringsAsFactors = F)
exn <- read.table("expressoes_negativas.txt", header = F, sep = "\t", strip.white = F,
                stringsAsFactors = F)
vn <- read.table("verbos_negativos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F)
subn <- read.table("substantivos_negativos.txt", header = F, sep = "\t", strip.white = F, 
                 stringsAsFactors = F)
ap <- read.table("adjetivos_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F)
exp <- read.table("expressoes_positivas.txt", header = F, sep = "\t", strip.white = F, 
                stringsAsFactors = F)
vp <- read.table("verbos_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F)
sp <- read.table("substantivos_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F)

# second list of stop words
poskaggle <- read.table("positive_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")
negkaggle <- read.table("negative_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")

# unifing polarities 
dfPolaridades <- an %>% 
  mutate(word = V1, polaridade = -1, tipo='adjetivo', sentimento='negativo') %>%
  select(word,polaridade,tipo,sentimento) %>%
  arrange(word)
icount <-  length(exn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = exn$V1, polaridade=rep(-1,icount),tipo=rep('expressao',icount),sentimento=rep('negativo',icount)))
dfPolaridades %>% arrange(desc(word)) %>% head(3)
icount <-  length(vn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = vn$V1, polaridade=rep(-1,icount),tipo=rep('verbo',icount),sentimento=rep('negativo',icount)))
icount <-  length(subn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = subn$V1, polaridade=rep(-1,icount),tipo=rep('substantivo',icount),sentimento=rep('negativo',icount)))
icount <-  length(negkaggle$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = negkaggle$V1, polaridade=rep(-1,icount),tipo=rep('noclass',icount),sentimento=rep('negativo',icount)))
icount <-  length(ap$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = ap$V1, polaridade=rep(1,icount),tipo=rep('adjetivo',icount),sentimento=rep('positivo',icount)))
icount <-  length(exp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = exp$V1, polaridade=rep(1,icount),tipo=rep('expressao',icount),sentimento=rep('positivo',icount)))
icount <-  length(vp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = vp$V1, polaridade=rep(1,icount),tipo=rep('verbo',icount),sentimento=rep('positivo',icount)))
icount <-  length(sp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = sp$V1, polaridade=rep(1,icount),tipo=rep('substantivo',icount),sentimento=rep('positivo',icount)))
icount <-  length(poskaggle$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = poskaggle$V1, polaridade=rep(1,icount),tipo=rep('noclass',icount),sentimento=rep('positivo',icount)))

# unifing polarities
dfPolaridades$word <- gsub("[[]","",dfPolaridades$word)
dfPolaridades$word <- gsub("[]]","",dfPolaridades$word)
dfPolaridadesUnique <- dfPolaridades[!duplicated(dfPolaridades$word),]
dfPolaridadesUnique %>% count()

# collect data of something that you want to study
#dados <- search_tweets("AMBEV", n=200000, lang = "pt", include_rts = FALSE)
#tmls  <- dados

# timeline of some user
tmls <- get_timelines(c("leiamoneytimes","TraderBolsa"), n = 20)
# data usefull, only words 
tweetxt <- tmls %>% select(text)
tibble(tweetxt)

# change web font 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweetxtUtf <- sapply(tweetxt, function(x) stri_trans_tolower(x,'pt'))
tweetxtUtf <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  tweetxtUtf);
tweetxtUtf <- str_replace(tweetxtUtf,"RT @[a-z,A-Z]*: ","")
tweetxtUtf <- gsub("@\\w+", "", tweetxtUtf)
tweetxtUtf <- removeURL(tweetxtUtf)
tweetxtUtf <- str_replace_all(tweetxtUtf,"@[a-z,A-Z]*","")  
tweetxtUtf <- gsub("[^[:alnum:][:blank:]!?]", " ", tweetxtUtf)
tweetxtUtf <- gsub("[[:digit:]]", "", tweetxtUtf)

# removing duplicateds 
tweetxtUtfUniqueSw <- tm::removeWords(tweetxtUtf,c(sw_merged,'rt'))
tibble(tweetxtUtfUniqueSw)

# 
ttokens <- data_frame(word= tweetxtUtfUniqueSw) %>% unnest_tokens(word,word)
ttokens_filter <- ttokens %>% filter(nchar(word) > 3)
ttokens_filter %>% count(word, sort=T)

ttokens_freq <- ttokens_filter %>% count(word, sort = T) %>% select(word, freq=n) 

# first plot 
wordcloud2(ttokens_freq , minSize = 2, size = 1, backgroundColor = 'black')

# second plot
library(ggplot2)
ttokens_filter %>% 
  count(word, sort = TRUE) %>%
  top_n(25) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +
  geom_col() +
 xlab(NULL) +
  coord_flip() +
 theme_classic()+
  labs(x = "Unique words",
       y = "Count",
       title = "BloombergBrasil tweets")

# sentimental packs
positivas <- dfPolaridadesUnique %>% filter(sentimento == 'positivo') %>% select(word)
negativas <- dfPolaridadesUnique %>% filter(sentimento == 'negativo') %>% select(word)

# dictionary sentimental words
dictionaryPortuguese <- SentimentDictionaryBinary(positivas$word, 
                                                  negativas$word)
# ranking sentimental tweets
tmls$id <- row.names(tmls)
sentiment <- analyzeSentiment(tmls$text,
                              language="portuguese",
                              rules=list("PtSentiment"=list(ruleSentiment, dictionaryPortuguese), 
                                         "Ratio"=list(ruleSentimentPolarity,dictionaryPortuguese),
                                         "Words"=list(ruleWordCount)))
#plot data sentiment
plotSentiment(sentiment)

sentiment$id <- row.names(sentiment)
worstfeelings <- sentiment[order(sentiment$PtSentiment),][1:10,] 
bestfeelings <-  sentiment[order(-sentiment$PtSentiment),][1:10,] 

##Topdown 10 Ruins
tibble(tmls[tmls$id %in% worstfeelings$id,]$text)
##Topdown 10 Bons
tibble(tmls[tmls$id %in% bestfeelings$id,]$text)


write_csv(tibble(tmls[tmls$id %in% worstfeelings$id,]$text),path = "C:/Users/mathe/Downloads/Projetos/appbolsa/worst.csv")
write_csv(tibble(tmls[tmls$id %in% bestfeelings$id,]$text),path = "C:/Users/mathe/Downloads/Projetos/appbolsa/best.csv")

#export data
#write_csv(tibble(word = sw_merged),path = ‘parte2/stopwords.csv’)
#write_csv(as.tibble(dfPolaridadesUnique),path = ‘parte2/polaridades_pt.csv’)
#write_csv(tibble(tweet = tweetxt),path = ‘parte2/tweetxt.csv’)
#write_csv(tibble(tweet = tweetxtUtfUniqueSw),path=‘parte2/tweets_limpo.csv’)
#write_csv(ttokens_freq, path=‘parte2/tokens.csv’)

# change web font (different ways)
#tweets.tweetxt$stripped_text1 <- gsub("http\\S+","",tweets.tweetxt$text)
#tweets.tweetxt$stripped_text1 <- gsub("#([a-z|A-Z|0-9|_])*","", tweets.tweetxt$stripped_text1) # remove hashtags
#tweets.tweetxt$stripped_text1 <- gsub('@([a-z|A-Z|0-9|_])*', '', tweets.tweetxt$stripped_text1) # remove palavras com @ (menções)
#tweets.tweetxt$stripped_text1 <- gsub('https://','', tweets.tweetxt$stripped_text1) # removes https://
#tweets.tweetxt$stripped_text1 <- gsub('http://','', tweets.tweetxt$stripped_text1) # removes http://
#tweets.tweetxt$stripped_text1 <- gsub('[^[:graph:]]', ' ', tweets.tweetxt$stripped_text1) # removes graphic characters like emoticons 
#tweets.tweetxt$stripped_text1 <- gsub('[[:punct:]]', '', tweets.tweetxt$stripped_text1) # removes punctuation 
#tweets.tweetxt$stripped_text1 <- gsub('[[:cntrl:]]', '', tweets.tweetxt$stripped_text1) # removes control characters
#tweets.tweetxt$stripped_text1 <- gsub("\\w*[0-9]+\\w*\\s*", "", tweets.tweetxt$stripped_text1) # removes numbers

# analisys of words that we are not interestesd
#tweets.tweetxt_stem <- tweets.tweetxt %>% select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
#cleaned_tweets.tweetxt <- tweets.tweetxt_stem %>% anti_join(stop_words) %>% anti_join(stop_words)

############ english part part ##################

library(sentimentr)

#aggregated sentiment analysis

#sentiment_by('France reported an increase in new deaths Wednesday, even as hospitalizations continued to decline', by = NULL)

#'France reported an increase in new deaths Wednesday, even as hospitalizations continued to decline' %>% extract_sentiment_terms()

tmls <- get_timelines(c("business"),n=20)
dataframe <- lapply(tmls$text, sentiment_by)
df <- c()
df1 <- c()
for (i in 1:length(dataframe)) {
  df[i] <- dataframe[[i]]$ave_sentiment
  df1[i] <- tmls$text[i]
}

polaridade <- data.frame(df1,df)
colnames(polaridade) <- c("text","rank")

worstfeelings1 <- polaridade[order(polaridade$rank),][1:10,] 
bestfeelings1 <-  polaridade[order(-polaridade$rank),][1:10,] 

##Topdown 10 Ruins
data.frame(worstfeelings1$text)
##Topdown 10 Bons
data.frame(bestfeelings1$text)

write_csv(data.frame(worstfeelings1$text),path = "C:/Users/mathe/Downloads/Projetos/appbolsa/worst1.csv")
write_csv(data.frame(bestfeelings1$text),path = "C:/Users/mathe/Downloads/Projetos/appbolsa/best1.csv")

############ detecting warms #############

# noticias de ações tradersclub

ibov1 <- GetIbovStocks()
ibov1$tickers <- paste0(ibov1$tickers, ":")
tickers1 <- paste0("$",ibov1$tickers)

tmls1 <- data.frame(get_timelines(c("tradersclubbr"), n = 30))

control <- c()
cod <- c()
aux <- c()
for (i in 1:length(tmls1$text)) {
  control <- unlist(strsplit(tmls1$text[i], " ", fixed = TRUE))
  for (k in 1:length(control)) {
    for (j in 1:length(tickers1)) {
      if(control[k] == tickers1[j]){
        aux[i] = i
        cod[i] = tickers1[j]
      }
    }
  }
}

minhaacao <- c()
minhaacao <- na.exclude(tmls1$text[aux])

ibov2 <- GetIbovStocks()
tickers2 <- paste0("#",ibov2$tickers)
tmls2 <- data.frame(get_timelines(c("FilipeVillegas"), n = 8))

control2 <- c()
cod2 <- c()
aux2 <- c()
for (i in 1:length(tmls2$text)) {
  control2 <- unlist(strsplit(tmls2$text[i], " ", fixed = TRUE))
  for (k in 1:length(control2)) {
    for (j in 1:length(tickers2)) {
      if(control2[k] == tickers2[j]){
        aux2[i] = i
        cod2[i] = tickers2[j]
      }
    }
  }
}

minhaacao2 <- c()
minhaacao2 <- na.exclude(tmls2$text[aux2])
