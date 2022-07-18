
library(tidyverse)
library(quanteda) 
library(magrittr)


# ===========================================================================
# 1. FUNCTIONS FOR CALCULATING DICTIONARY SENTIMENT MEASURES
# ===========================================================================

calcDictSentiment <- function(dfm, dict_name){
  
  dict_filename <- dplyr::case_when(
    #EN
    dict_name == "dict_liwc_en" ~ "data/dictionaries/dict_liwc_en.rds",
    dict_name == "dict_lsd_en" ~ "data/dictionaries/dict_lsd_en.rds",
    dict_name == "dict_rheault_en" ~ "data/dictionaries/dict_rheault_en.rds",
    #DE
    dict_name == "dict_liwc_de" ~ "data/dictionaries/dict_liwc_de.rds",
    dict_name == "dict_lsd_de" ~ "data/dictionaries/dict_lsd_de.rds",
    dict_name == "dict_rauh_de" ~ "data/dictionaries/dict_rauh_de.rds",
    #NL
    dict_name == "dict_liwc_nl" ~ "data/dictionaries/dict_liwc_nl.rds",
    dict_name == "dict_lsd_nl" ~ "data/dictionaries/dict_lsd_nl.rds")
  dict <- read_rds(dict_filename)
  
  
  ntokens <- ntoken(dfm)
  
  dict_score <- dfm(dfm, dictionary = dict) %>% 
    convert(to='data.frame') %>% 
    mutate(ntokens = ntokens,
           sentiment = positive/ntokens*100 - negative/ntokens*100)
  
  dict_score %<>% select(doc_id, sentiment)
  
  dict_score %<>% 
    tidylog::rename(speech_id = doc_id,
                    !!dict_name := sentiment)
  
  
  dict_score$speech_id %<>% as.numeric()
  
  return(dict_score)
  
}



# ===========================================================================
# 2. CALC DICT SENTIMENTS
# ===========================================================================

# EN
data <- read_rds(file = "data/coder ratings/coder_ratings_uk.rds")
dfm <- read_rds(file = "data/parlspeech dfms/dfm_parlspeech_uk10k_full.rds")

data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_liwc_en"), by = "speech_id")
data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_lsd_en"), by = "speech_id")
data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_rheault_en"), by = "speech_id")

write_rds(data, file = "data/sentiment_coders_dictionaries_uk.rds", compress = "xz")

# DE
data <- read_rds(file = "data/coder ratings/coder_ratings_de.rds")
dfm <- read_rds(file = "data/parlspeech dfms/dfm_parlspeech_de10k_full.rds")

data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_liwc_de"), by = "speech_id")
data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_lsd_de"), by = "speech_id")
data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_rauh_de"), by = "speech_id")

write_rds(data, file = "data/sentiment_coders_dictionaries_de.rds", compress = "xz")

# NL
data <- read_rds(file = "data/coder ratings/coder_ratings_nl.rds")
dfm <- read_rds(file = "data/parlspeech dfms/dfm_parlspeech_nl10k_full.rds")

data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_liwc_nl"), by = "speech_id")
data <- data %>% left_join(calcDictSentiment(dfm = dfm, dict_name = "dict_lsd_nl"), by = "speech_id")

write_rds(data, file = "data/sentiment_coders_dictionaries_nl.rds", compress = "xz")


