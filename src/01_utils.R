theme_custom <- function (){
  theme_minimal()  %+replace%
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_rect(fill = NA,colour = "black", size = 0.5,
                                      linetype = "solid"),
          plot.title = element_text(size = 15, face = "bold",
                                    vjust = 1.5, hjust = 0.5,
                                    margin=margin(0, 0, 12 ,0)),
          legend.position = "bottom",
          axis.ticks = element_line(size = 0.3),
          axis.ticks.length = unit(0.2, "cm"),
          legend.text=element_text(size = 25),
          legend.title = element_text(size = 25),
          strip.text = element_text(size = 25, hjust = 0.5, face = "bold",
                                    margin = margin(b = 5, r = 5, l = 5, t = 5)),
          strip.background =element_rect(fill="lightgrey"),
          axis.text = element_text(colour = "black", size = 20),
          axis.title = element_text(size = 13, hjust = 0.5)
    )
}

filterDictionary <- function(dict, dfm, min_freq){
  
  dfm_feat_freq <- textstat_frequency(dfm)
  dict_pos <- data.frame(feature = dict$positive)
  dict_pos$feature %<>% as.character()
  dict_neg <- data.frame(feature = dict$negative)
  dict_neg$feature %<>% as.character()
  
  
  dict_pos %<>% 
    left_join(dfm_feat_freq, by = "feature") %>% 
    filter(frequency >= min_freq)
  dict_neg %<>% 
    left_join(dfm_feat_freq, by = "feature") %>% 
    filter(frequency >= min_freq)
  
  dict <- dictionary(list(positive = dict_pos$feature,
                          negative = dict_neg$feature))
  return(dict)
}

convertQuarters <- function(quarters){
  
  year    <- quarters %/% 1  
  quarter <- (quarters %% 1) * 10 
  
  quarter_zoo <- (quarter - 1L) / 4
  zooyq <- year + quarter_zoo
  
  x <- zoo::as.yearqtr(zooyq)
  
  return(x)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

