library(dplyr)
library(data.table)
library(stringr)

#dossier contenant les fichiers
setwd("C:/Users/ettor/Desktop/Eurovoc Topicmodeling/presidencies")

#on merge les trois
files <- list.files(path = getwd(),
                    pattern = ".txt")
temp <- lapply(files, fread, sep="\t")
data <- rbindlist( temp )

#on reshape le tout
DT.m2 <- data.table::melt(data,
             id = c("V1", "V2"),
             measure = patterns("[13579]$", "[02468]$"), 
             value.name = c("topic", "proportion"),
             na.rm=TRUE) %>% 
  filter(!str_detect(proportion, "file")) %>% 
  mutate("presidency" = str_extract(V2, "EU_en_pres\\d"), 
         fichier = str_extract(V2,"\\d{4}.+\\.txt"),
         topic_id = paste0(presidency, "-100_", topic)) %>% 
  select(8, "doc"=1, 6, 7, 4, 5) %>% 
  arrange(topic_id, topic)

#on enregistre le tout
fwrite(DT.m2, "topics_matrice.csv")


#selectionner 40 documents aléatoires par années entre 1973 et 1982

library(readr)
library(dplyr)
documents_commission_1973_1982 <- read_csv("C:/Users/ettor/Desktop/documents_commission_1973-1982.csv")
View(documents_commission_1973_1982)

echantillon <- documents_commission_1973_1982 %>% 
  group_by(year) %>% 
  sample_n(40) %>% 
  write_csv("echantillon.csv")


