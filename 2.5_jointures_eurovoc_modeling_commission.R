#traitement du fichier open refine echantillon
library(dplyr)
library(readr)

#on importe la liste des 400 fichiers qui constituent l'échantillon
echantillon <- read_delim("~/eurovoc_topicmodeling/echantillon.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
View(echantillon)

#on importe la matrice doc.topics
topics_matrice_commission <- read_csv("~/eurovoc_topicmodeling/topics_matrice_commission.csv")
View(topics_matrice_commission)

#on importe les matchings eurovoc
eurovoc_modeling_19_tokens_verif <- read_csv("~/eurovoc_topicmodeling/eurovoc_modeling_19_tokens_verif.csv")
View(eurovoc_modeling_19_tokens_verif)

#on joint l'échantillon avec la matrice sur base du nom de fichier
echantillon_matrice <- 
  echantillon %>% 
  left_join(topics_matrice_commission)

#on joint le tout avec les matchings eurovoc sur base du topic_id
echantillon_final <- 
  echantillon_matrice %>% 
  left_join(eurovoc_modeling_19_tokens_verif)

#on groupe les poucentages de chaque match et on ne garde que ceux supérieurs à 5 %
echantillon2 <- 
  echantillon_final %>% 
  group_by(fichier, match, BT1) %>% 
  summarise(somme = sum(proportion_percent)) %>% 
  na.omit() %>% 
  filter(somme > 5) %>% 
  arrange(desc(fichier), desc(somme))

write_csv(echantillon2, "echantillon2.csv")

#ça ne semble pas donner grand_chose, notamment à cause du stemming-matching trop sauvage et
#du fait que beaucoup de documents ne contiennent même pas d'anglais...

#On essaye cette fois avec les topics entiers, en ne considérant plus
#que l'on peut faire la somme des tokens

echantillon3 <- 
  echantillon_final %>% 
  select(fichier, topic_id, proportion_percent, match, tokens1_19) %>% 
  group_by(fichier, topic_id, tokens1_19, match, proportion_percent) %>% 
  na.omit() %>% 
  filter(proportion_percent > 5) %>% 
  arrange(desc(fichier), desc(proportion_percent))

echantillon3 <- data.table::as.data.table(echantillon3)
echantillon4 <- echantillon3[,.(concat_match = paste(match, collapse=" | ")), by = .(topic_id, fichier, tokens1_19, proportion_percent)]

data.table::fwrite(echantillon4, "echantillon4.csv")
