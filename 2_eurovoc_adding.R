#Prend le résultat d'un topic modeling effectué avec mallet et ajoute les eurovocs
library(readr)
library(dplyr)
library(tidyr) 
library(data.table)
source("get_fingerprint.R") #imitation du fingerprint d'Open Refine

#on importe les labels
dcb_topic_labels <- fread("dcb-topic-labels.csv",
                          col.names = c("topic_id", "token"),
                          encoding = "Latin-1")

View(dcb_topic_labels)

#ATTENTION : problème avec les étranges caractères si on importe en UTF-8

#la variable "n.topwords" vient du topic modeling (en général 20 labels)
labels <- 
  dcb_topic_labels %>%
  separate(token, into=as.character(1:n.topwords), sep=" ")

#réorganiser topics sous forme de DB, afin de minimiser les matchings ?
labels_tidy <- 
  melt(labels, id.vars = c("topic_id"),
                    measure.vars = 2:20) %>% 
  select(topic_id, value) %>% 
  arrange(topic_id)

  
#on importe le thésaurus nettoyé
thesaurus <-
  fread("~/eurovoc_topicmodeling/eurovoc_thesaurus_complet.csv", encoding="UTF-8")

#On stemme les tokens (vérifier si ça marche mieux que sans)
#verifier avec un algo moins agressif que porter
# labels_tidy_stem <-
#   labels_tidy[,.(topic_id, value, stem = SnowballC::wordStem(value, language = "porter"))]

######################################
#jointure des unigrammes
######################################

#jointure avec le thesaurus sur les stems
labels_tidy_joined_unigrams <- labels_tidy %>%
  fuzzyjoin::stringdist_left_join(thesaurus,
                                  by = c(value = "fingerprint_cleaned"),
                                  distance_col = "dist",
                                  max_dist = 0,
                                  method = "lv") %>% 
  arrange(as.integer(topic_id), term) %>% 
  filter(!is.na(term)) %>% 
  select(topic_id, value, term, french, length, profondeur, category, link, bt1, mt, domain)

# matched <- colMeans(!is.na(labels_tidy_joined))['term']
# print(paste0("Le poucentage de matching est de ", round(matched, 2)))

#essais de summarisation
# df <- labels_tidy_joined %>% 
#   group_by(topic_id, mt) %>% 
#   summarise(total.domain=n()) %>% 
#   arrange(as.integer(topic_id))
# 
# View(df)

######################################
#création des bigrammes et fingerprint
######################################

bigrammes <-
  melt(setDT(labels_tidy), 
       id.var = c("topic_id"))[, combn(value, 2, FUN = paste, collapse = ' '), topic_id]

bigrammes <- bigrammes[,.(topic_id, V1, fingerprint = get_fingerprint(V1))]

setnames(bigrammes, "V1", "value")

#on stemme les bigrammes
bigrammes$fingerprint_stem <-  strsplit(bigrammes$fingerprint, " ", fixed = TRUE) %>%
  lapply(., function(x) SnowballC::wordStem(x, language = "porter") ) %>%
  vapply(., function(x) paste(x, collapse = " "), character(1))

#test de fuzzy join sur une partie des bigrammes
labels_tidy_joined_bigrams <- bigrammes %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2],
                                   by = c(fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=0,
                                   ignore_case = TRUE,
                                   method = "lv") %>% 
  select(topic_id, value, term, french, length, profondeur, category, link, bt1, mt, domain)

######################################
#création des trigrammes et fingerprint
######################################
trigrammes <-
  melt(setDT(labels_tidy), 
       id.var = c("topic_id"))[, combn(value, 3, FUN = paste, collapse = ' '), topic_id]

trigrammes <- trigrammes[,.(topic_id, V1, fingerprint = get_fingerprint(V1))]

setnames(trigrammes, "V1", "value")

#on stemme les bigrammes
trigrammes$fingerprint_stem <-  strsplit(trigrammes$fingerprint, " ", fixed = TRUE) %>%
  lapply(., function(x) SnowballC::wordStem(x, language = "porter") ) %>%
  vapply(., function(x) paste(x, collapse = " "), character(1))

#test de fuzzy join sur une partie des bigrammes
labels_tidy_joined_trigrams <- trigrammes %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length<2],
                                   by = c(fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=0,
                                   ignore_case = TRUE,
                                   method = "lv") %>% 
  select(topic_id, value, term, french, length, profondeur, category, link, bt1, mt, domain)

#On merge les unigrams, bigrammes et trigrammes matchés
merged <- rbind(labels_tidy_joined_unigrams, 
                labels_tidy_joined_bigrams, 
                labels_tidy_joined_trigrams)

#on réconcilie les topics_id des termes matchés avec la matrice doc.topics transposée
doc.topics_t <- 
  doc.topics %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("fichier") %>% 
  gather(topic_id, proportion, 2:21)

final <- 
  doc.topics_t %>% 
  left_join(merged)

#On retente de faire la somme cumulée des proportions de chaque token
somme_cumulee <- 
  final %>% 
  group_by(fichier, term) %>% 
  summarise(somme = sum(proportion)*100) %>%
  mutate(file_id=stringr::str_extract(""))
  filter(somme >= 5)
  
fwrite(somme_cumulee, "somme_cumulee.csv")

#il faut maintenant comparer avec les eurovocs manuels
jrc_metadata <- fread("~/eurovoc_topicmodeling/jrc_acquis_metadata.csv") %>% 
  select(-body)

fwrite(jrc_metadata, "jrc_metatada_sansbody.csv")