library(readr)
library(tidyverse)
English_250_composition_with_topic_label <- read_csv("C:/Users/ettor/Desktop/English_250_composition_with_topic_label.csv")
View(English_250_composition_with_topic_label)


df <- 
  English_250_composition_with_topic_label %>% 
  group_by(document) %>% 
  top_n(n=6, topic) %>% 
  arrange(document, desc(topic))

View(df)

write_csv(df, "C:/Users/ettor/Desktop/English_250_composition_with_topic_label_sorted.csv")