# Protocol code: Reputation management during a public health crisis: Overcompensating when all else fails. PAR, 2023.
# Varela Castro, Bustos & Saldivia Gonzatti - April 2023.

# The code covers the access to Twitter data (without license #) and the data sentiment aggregation

# Necessary libraries ====

library(dplyr)
library(academictwitteR)

# Twitter access ====

# Twitter config
set_bearer()
options(scipen = 999)

# Get tweets and bind
all <- get_all_tweets(query = "@SSalud_mx",
                      start_tweets = "2020-02-20T00:00:00Z", is_retweet = FALSE, export_query = TRUE,
                      end_tweets =  "2020-07-01T00:00:00Z", n = 10000000, lang = "es",
                      data_path = "../data_sec_2020.1/",
                      bind_tweets = FALSE,
                      file = "../data_sec_2020.1.rds")

all <- get_all_tweets(query = "@SSalud_mx",
                      start_tweets = "2020-07-01T00:00:00Z", is_retweet = FALSE, export_query = TRUE,
                      end_tweets =  "2021-01-01T00:00:00Z", n = 10000000, lang = "es",
                      data_path = "../data_sec_2020.2/",
                      bind_tweets = FALSE,
                      file = "../data_sec_2020.2.rds")

all <- get_all_tweets(query = "@SSalud_mx",
                      start_tweets = "2021-01-01T00:00:00Z", is_retweet = FALSE, export_query = TRUE,
                      end_tweets =  "2021-07-01T00:00:00Z", n = 10000000, lang = "es",
                      data_path = "../data_sec_2021.1/",
                      bind_tweets = FALSE,
                      file = "../data_sec_2021.1.rds")

all <- get_all_tweets(query = "@SSalud_mx",
                      start_tweets = "2021-07-01T00:00:00Z", is_retweet = FALSE, export_query = TRUE,
                      end_tweets =  "2022-02-28T00:00:00Z", n = 10000000, lang = "es",
                      data_path = "../data_sec_2021.2/",
                      bind_tweets = FALSE,
                      file = "../data_sec_2021-2022.2.rds")

tw_20.1 <- readRDS("D_Data_Twitter/data_sec_2020.1.rds")
tw_20.2 <- readRDS("D_Data_Twitter/data_sec_2020.2.rds")
tw_21.1 <- readRDS("D_Data_Twitter/data_sec_2021.1.rds")
tw_21.2 <- readRDS("D_Data_Twitter/data_sec_2021-2022.2.rds")

data <- bind_rows(tw_20.1, tw_20.2, tw_21.1, tw_21.2)

#saveRDS(data, file = "../all_tweets_SSalud_mx.022020-022021.rds")
twitter_data <- readRDS("../D_Data_Twitter/all_tweets_SSalud_mx.022020-022021.rds")

# Analyze Twitter data ====

twitter_data <- twitter_data %>% filter(!author_id=="132225222")
twitter_data <- twitter_data %>% mutate(date = as.Date(created_at))
twitter_data <- twitter_data %>% 
  mutate(month = as.Date(paste0(lubridate::year(date),"-",lubridate::month(date),"-01")))

# table(twitter_data$month >= as.Date("2020-02-01") & twitter_data$month <= as.Date("2021-06-01"))

# Check Tweet count
tweet_count <- twitter_data %>% group_by(date) %>% summarise(count = n())

ggplot(tweet_count, aes(x = month, y = count)) + geom_line() + theme_minimal() + ylab("# Tweets") +
  xlab("Date")

# Generate corpus, tokens, and DFM
tweets_corpus  <- quanteda::corpus(twitter_data, text_field= "text")


tweets_tokens <- quanteda::tokens(tweets_corpus, 
                                  remove_punct = FALSE,
                                  remove_symbols=FALSE, 
                                  remove_separators=FALSE,
                                  split_hyphens = TRUE, 
                                  remove_numbers = FALSE,
                                  remove_url=FALSE) %>% quanteda::tokens_tolower()

# Load sentiment dictionary and get frequencies

# Download from Harvard dataverse with `r load(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/ALFLK6/2K6ZHD"))`. 
# Source: Proksch, S.-O., Lowe, W., Wäckerle, J. and Soroka, S. (2019), Multilingual Sentiment Analysis: A New Approach to Measuring Conflict in Legislative Speeches. Legislative Studies Quarterly, 44: 97-131. https://doi.org/10.1111/lsq.12218.

list <- ls()
load("../D_Data/auto_dictionaries_lsd.RData")
rm(list=setdiff(ls(), c("extendeddict_es", list)))

tweets_dmf <- quanteda::dfm(tweets_tokens, dictionary = c(extendeddict_es))
twitter_data <- twitter_data %>% bind_cols(tweets_dmf %>% as.data.frame()) 

tw.daily_sentiment.legit <- twitter_data %>% group_by(date) %>% 
  summarise(pos = sum(pos, na.rm = TRUE), neg = sum(neg, na.rm = TRUE))

tw.daily_sentiment.legit <- tw.daily_sentiment.legit %>% mutate(sentiment = base::log((pos+0.5)/(neg+0.5)))

# Visualize sentiment over time
ggplot(tw.daily_sentiment.legit, aes(x = date, y = sentiment)) + geom_line() + theme_minimal() + ylab("Tweets sentiment") +
  xlab("Date")

# save(tw.daily_sentiment.legit, tweet_count, file = "data/twitter_aggregated.RData")
