

library(tidyverse)
library(quanteda) 
library(quanteda.textstats)
library(magrittr)
library(foreach)
library(doParallel)
library(rngtools)
library(rpackage)




# =========================================================================
# 1. SETTINGS
# =========================================================================

# General settings
topics <- c(1, seq(5, 30, by = 5))



# Combinations 
runs <- 10
iters <- 100
dictionary <- c("LIWC", "LSD")
sample <- c("nl10k")
dfm_version <- c("full", "trmd")
exclude_neutral <- c(FALSE, TRUE)
filter_dictionary <- c(FALSE, TRUE)

input <- expand.grid(sample, dfm_version, dictionary, exclude_neutral, filter_dictionary, iters, runs)

colnames(input) <- c("sample", "dfm_version", "dictionary", "exclude_neutral", "filter_dictionary", "iters", "runs")

input<- input %>% 
  tidylog::mutate(sample = sample %>% as.character(),
                  dfm_version = dfm_version %>% as.character(),
                  dictionary = dictionary %>% as.character())

input <- input %>% 
  tidylog::mutate(dfm_file = paste0("data/parlspeech dfms/dfm_parlspeech_", 
                                    sample,
                                    "_",
                                    dfm_version,
                                    ".rds"))
input <- input %>% 
  tidylog::mutate(dictionary_file = case_when(dictionary == "LIWC" ~ "data/dictionaries/dict_liwc_nl.rds",
                                              dictionary == "LSD" ~ "data/dictionaries/dict_lsd_nl.rds"))

# =========================================================================
# 2. ESTIMATE JST
# =========================================================================

source("src/01_utils.R")

# ALL RESULTS STORED
jst_results <- list()


# RUN JST ON ALL COMBINATIONS
for (i in 1:length(input)){
  
  # Load input files
  dfm <- read_rds(input$dfm_file[i])
  dict <- read_rds(input$dictionary_file[i])
  
  # Match or filter dictionary
  if(input$filter_dictionary[i] == TRUE){
    dict %<>% filterDictionary(dfm = dfm, min_freq = 50)
  }else{
    dict %<>% filterDictionary(dfm = dfm, min_freq = 1) 
  }
  
  # JST settings
  iters <- input$iters[i]
  runs <- input$runs[i]
  exclude_neutral <- input$exclude_neutral[i]
  
  # where all results will be stored
  results_list <- list()
  
  
  message(paste0("NOW WORKING ON \n",
                 "sample: ", input$sample[i], 
                 ", dfm: ", input$dfm_version[i], 
                 ", dict: ", input$dictionary[i], 
                 ", filtered: ", input$filter_dictionary[i], 
                 ", exclude neutral label:", input$exclude_neutral[i], "..."))
  
  # loop over topic numbers
  for (j in c(1:length(topics))) {
    
    message(paste0("Estimating JST with ", topics[j], " topic(s)..."))
    
    jst <- rpackage::jstManyRuns(dfm = dfm, 
                                    sentiLexInput = dict,
                                    numTopics = topics[j],
                                    numIters = iters,
                                    n = runs, 
                                    excludeNeutral = exclude_neutral,
                                    seed = 1899)
    
    # extract 3 labels if neutral is included
    if(exclude_neutral == FALSE){
      jst <- jst %>% 
        dplyr::select(docname_, sent1_mean, sent2_mean, sent3_mean) %>% 
        dplyr::rename(neutral = sent1_mean, 
                      pos = sent2_mean,
                      neg = sent3_mean)
    }# otherwise just 2 labels (pos and neg)
    if(exclude_neutral == TRUE){
      jst <- jst %>% 
        dplyr::select(docname_, sent1_mean, sent2_mean) %>% 
        dplyr::rename(pos = sent1_mean,
                      neg = sent2_mean)
    }
    
    
    results_list[[j]] <- jst
  }
  
  
  
  jst_results[[i]] <- list(sample = input$sample[i],
                           dfm_version = input$dfm_version[i],
                           dictionary = input$dictionary[i],
                           n_pos_dictterms = length(dict$positive),
                           n_neg_dictterms = length(dict$negative),
                           exclude_neutral = input$exclude_neutral[i],
                           filter_dictionary = input$filter_dictionary[i],
                           topics = topics,
                           runs = runs,
                           iter = iters,
                           result = results_list)
  
  
}

# Save results
write_rds(jst_results, file = "data/jst_results_nl.rds", compress = "xz")

