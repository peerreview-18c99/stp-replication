

library(tidyverse)
library(magrittr)
library(rpackage)




# =========================================================================
# 1. SETTINGS
# =========================================================================


# ===========================================================================
# 1. FUNCTIONS FOR AGGREGATING JST SENTIMENT MEASURES
# ===========================================================================

getJSTmeanSentiment <- function(jst_list){
  
  
  sentiment_ntopics <- data.frame(matrix(NA, ncol = 1, nrow(jst_list[[1]][[1]])))[-1]
  
  for (i in 1:length(jst_list)){
    
    
    
    sentiments <- data.frame(matrix(NA, ncol = 1, nrow = nrow(jst_list[[i]][[1]])))[-1]
    for(j in 1:length(jst_list[[i]])){
      jst_list[[i]][[j]] <- jst_list[[i]][[j]] %>% 
        mutate(sentiment = pos - neg)
      
      sentiment <- jst_list[[i]][[j]]$sentiment
      sentiments %<>% cbind(sentiment)
      
    }
    
    sentiment <- sentiments %>% rowMeans()
    sentiment_ntopics %<>% cbind(sentiment)
    
    
  }
  
  sentiment_ntopics$speech_id <- jst_list[[1]][[1]]$speech_id
  
  return(sentiment_ntopics)
  
}



# ===========================================================================
# 2. UK
# ===========================================================================

jst_list <- read_rds("data/jst_results_uk.rds")


for(i in 1:length(jst_list)){
  
  jst_list[[i]]$mean_sentiment <- jst_list[[i]]$result %>% getJSTmeanSentiment()
}


results_list <- list.filter(jst_list, sample == "uk10" & exclude_neutral == FALSE & filter_dictionary == FALSE)

write_rds (results_list, file = "data/2-intermediate/parlspeech jst/jst_results_uk_aggregated.rds")


# ===========================================================================
# 3. DE
# ===========================================================================

jst_list <- read_rds("data/jst_results_de.rds")


for(i in 1:length(jst_list)){
  
  jst_list[[i]]$mean_sentiment <- jst_list[[i]]$result %>% getJSTmeanSentiment()
}


results_list <- list.filter(jst_list, sample == "de10" & exclude_neutral == FALSE & filter_dictionary == FALSE)

write_rds (results_list, file = "data/2-intermediate/parlspeech jst/jst_results_de_aggregated.rds")



# ===========================================================================
# 4. NL
# ===========================================================================

jst_list <- read_rds("data/jst_results_nl.rds")


for(i in 1:length(jst_list)){
  
  jst_list[[i]]$mean_sentiment <- jst_list[[i]]$result %>% getJSTmeanSentiment()
}


results_list <- list.filter(jst_list, sample == "nl10" & exclude_neutral == FALSE & filter_dictionary == FALSE)

write_rds (results_list, file = "data/2-intermediate/parlspeech jst/jst_results_nl_aggregated.rds")


