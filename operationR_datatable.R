library(dplyr)
library(stringr)
library(janitor)
library(data.table)
source("get_fingerprint.R") #imitation du fingerprint d'Open Refine

#on importe le fichier de topic modeling 19 tokens
topics <-
  fread("~/eurovoc_topicmodeling/eurovoc_modeling_all.csv")
topics <- clean_names(topics) %>% dplyr::select(c(1, 8:27 ))


#on importe le thésaurus nettoyé
thesaurus <-
  fread("~/eurovoc_topicmodeling/eurovoc_thesaurus_complet.csv", encoding="UTF-8")
thesaurus <- clean_names(thesaurus)


#on importe la matrice
matrice <-
  fread("~/eurovoc_topicmodeling/presidencies/topics_matrice.csv")


#réorganiser topics sous forme de DB, afin de minimiser les matchings ?

topics_tidy <- melt(topics, id.vars = c("topic_id", "file"),
             measure.vars = 4:22)

#On va isoler la liste de tokens dans un dataframe
tokens <- topics_tidy[,"value"]

#On les stemme : mais est-ce utile ?
tokens <-
  tokens[,.(value, stem = SnowballC::wordStem(value, language = "porter"))]

#test de fuzzy join sur les unigrams : trouver la bonne méthode dans stringdist...
joined_unigrams <- tokens %>%
  fuzzyjoin::stringdist_left_join(thesaurus,
                                  by = c(stem = "fingerprint_stem"),
                                  distance_col = "dist",
                                  max_dist=0,
                                  method = "lv")
View(joined_unigrams[,.(value, match, fingerprint_stem, term, dist)])

#création des bigrammes et fingerprint
bigrammes <-
  melt(setDT(topics), 
       id.var = c("topic_id", "distribution"))[, combn(value, 2, FUN = paste, collapse = ' '), topic_id]

bigrammes <- bigrammes[,.(topic_id, V1, bigrams_fingerprint = get_fingerprint(V1))]

setnames(bigrammes, "V1", "bigrams")

#on stemme les bigrammes
bigrammes$bigrams_fingerprint_stem <-  strsplit(bigrammes$bigrams_fingerprint, " ", fixed = TRUE) %>%
  lapply(., function(x) SnowballC::wordStem(x, language = "porter") ) %>%
  vapply(., function(x) paste(x, collapse = " "), character(1))

#test de fuzzy join sur une partie des bigrammes
joined_bigrams <- bigrammes[1:100] %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2],
                                  by = c(bigrams_fingerprint_stem = "fingerprint_stem"),
                                  distance_col = "dist",
                                  max_dist=0.1,
                                  ignore_case = TRUE,
                                  method = "jaccard", q=2)




#liste des fichiers dans le répertoire presidency_73_82, dont on extrait l'année et le nom de fichier
liste_docs <- as.data.table(list.files(path = "H:/Corpus_commission/presidencies_73_82",
                    pattern = "\\.txt$",
                    full.names = TRUE))[,.(V1, 
                                           year = stringr::str_extract(V1, "(\\d{4})"), 
                                           file = stringr::str_extract(V1, "(\\d{4}_.+txt$)"))]

setnames(liste_docs, "V1", "path")

#échantillon de 400 textes, 40 par année, reproductible
set.seed(42)
echantillon <- liste_docs %>% 
  group_by(year) %>% 
  sample_n(40) %>% 
  as.data.table()

fwrite(echantillon, "echantillon.csv")

# #join entre echantillon et la matrice
# setkey(matrice,fichier)
# setkey(echantillon,file)
# 
# # perform the left join
# Result <- echantillon[matrice, nomatch=0]


#un moyen d'imiter la concaténation des valeurs d'un records, comme dans Open refine
queries <- data.table(queries)
queries[,.(concat_match = paste(query_fingerprint,collapse=" ")), by = .(visit_id)]
