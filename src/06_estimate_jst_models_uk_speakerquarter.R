rpackage

library(tidyverse)
library(quanteda) 
library(quanteda.textstats)
library(magrittr)
library(foreach)
library(doParallel)
library(rngtools)
library(ldatuning)
library(zoo)





dfm <- read_rds(file = "data/parlspeech dfms/dfm_parlspeech_uk_speakerquarter.rds")



seed <- 1899
topics <- seq(80, 200, by = 10)
  
tuning <- FindTopicsNumber(
  dfm,
  topics = topics,
  metrics = c("Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  control = list(seed = seed),
  mc.cores = 7L,
  verbose = TRUE
)
  
saveRDS(tuning, file = "data/lda_tuning.rds")

FindTopicsNumber_plot(tuning)


#-----------------------------------------------------------------------------------------------#
# 1. Run JST
#-----------------------------------------------------------------------------------------------#


excludeNeutral <- FALSE
updateParaStep <- 25
iters <- 500
topics = 180

dict <- read_rds("data/dictionaries/dict_lsd_en.rds")
    
system.time({
      
jst_results <- rpackage::jst(dfm = dfm,
                                sentiLexInput = dict,
                                numTopics = topics,
                                numIters = iters, 
                                updateParaStep = updateParaStep,
                                excludeNeutral = excludeNeutral)

})


write_rds(jst_results, file = "data/jst_results_uk_speakerquarter.rds", compress = "xz")




#================================================================================================#
# Calculate sentiment
#================================================================================================#


source("src/01_utils.R")


jst_results <- read_rds(file = "data/jst_results_uk_speakerquarter.rds")

data <- rpackage::get_parameter(jst_results, "pi") %>%     
  rownames_to_column(var = "speaker_quarter") %>% 
  rename("neutral" = "sent1", "pos" = "sent2", "neg" = "sent3") %>% 
  mutate(sentiment = (pos - neg)) %>% 
  mutate(speaker = str_trim(sub("\\..*", "", speaker_quarter)), 
         parlspeech = speaker) %>% 
  mutate(quarter_zoo = convertQuarters(quarter)) %>% 
  dplyr::select(speaker_quarter, speaker, parlspeech, party, year, quarter, quarter_zoo, sentiment)


write_rds(data, file = "data/jst_sentiment_parlspeech_uk_speakerquarter.rds")







