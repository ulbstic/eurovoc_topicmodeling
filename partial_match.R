#Cette fonction est censée matcher un terme avec un max d'eurovocs et renvoyer le Microthésaurus le plus fréquent

library(readr)
library(tidyverse)

eurovoc_thesaurus <- read_csv("eurovoc_thesaurus_complet.csv")

glimpse(eurovoc_thesaurus)

data <- as.data.frame(c('industry', 'community'), optional=TRUE)
colnames(data) <- "value"

terms_matched <- data %>%
  fuzzyjoin::stringdist_left_join(eurovoc_thesaurus,
                                  by = c(value = "fingerprint_cleaned"),
                                  distance_col = "dist", 
                                  max_dist = 0.1,
                                  method='jw', p=0.1) %>% 
  arrange(term) %>% 
  filter(!is.na(term))

terms_matched

  
