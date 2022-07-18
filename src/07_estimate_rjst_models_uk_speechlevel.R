


library(tidyverse)
library(quanteda) 
library(quanteda.textstats)
library(magrittr)
library(rpackage)



source("src/01_utils.R")


dfm <- read_rds("data/parlspeech dfms/dfm_parlspeech_uk_conlab_speechlevel.rds")
dfm

iters <- 1500
excludeNeutral <- FALSE
alpha <- 1
gamma <- 50
updateparastep <- 50
ntopics <- 100

dict <- read_rds(file = "data/dictionaries/dict_lsd_en.rds") %>% 
  filterDictionary(dfm = dfm, min_freq = 1)


system.time(
rjst_uk <- rpackage::jst_reversed(dfm = dfm,
                                     sentiLexInput = dict,
                                     numTopics = ntopics,
                                     numIters = iters,
                                     excludeNeutral = excludeNeutral,
                                     alpha = alpha,
                                     gamma = gamma,
                                     updateParaStep = updateparastep)
)


write_rds(rjst_uk, file = paste0("data/rjst_results_uk_conlab.rds"), compress = "xz")



