# packs to this code
library(rtweet)
library(tidytext)
library(dplyr)
library(tidyr)
library(readxl)
library(wordcloud)

# some usually words that we will disconsider in this webcrap
palavras <- read_excel("C:/Users/mathe/Downloads/palavras.xlsx")

# collect
corona <- search_tweets("#LiveLocalMariliaMendonca", n=500000, lang = "pt", include_rts = FALSE)

# data usefull, only words and screen name
tweets.corona <- corona %>% select(screen_name,text)

# change web font
tweets.corona$stripped_text1 <- gsub("http\\S+","",tweets.corona$text)

# analisys of words that we are not interestesd
tweets.corona_stem <- tweets.corona %>% select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
cleaned_tweets.corona <- tweets.corona_stem %>% anti_join(stop_words) %>% anti_join(palavras)

# plots
cleaned_tweets.corona %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic()+
  labs(x = "Unique words",
       y = "Count",
       title = "#LiveLocalMariliaMendonca tweets")

wc <- cleaned_tweets.corona %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>% 
  mutate(word = reorder(word,n))

wordcloud(words = wc$word, freq = wc$n, max.words=20,min.freq=3,scale = c(1.15, 0.5), 
          random.order = FALSE,rot.per=.35,colors=palette())
