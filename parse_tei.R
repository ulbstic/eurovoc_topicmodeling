library(XML)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)

#liste des fichiers XML du corpus JRC Acquis version anglaise (http://optima.jrc.it/Acquis/JRC-Acquis.3.0/corpus/jrc-en.tgz)
liste <-
  list.files(
    path = "C:/Users/ettor/Desktop/Eurovoc/JRC Acqis Corpus/jrc-en/en",
    recursive = TRUE,
    ignore.case = FALSE,
    include.dirs = FALSE,
    full.names = TRUE
  )

#fonction pour parser les TEI
parseTei <- function(fichier, path, xmlelement) {
  text_parsed <- tryCatch({
    doc <- xmlParse(fichier,   trim = FALSE)
    text <- unlist(xpathSApply(doc, path, xmlelement))
    text <- gsub("%quot%", '"', text)
  },
  error = function(cond) {
    message(paste("file does not seem to exist:", fichier))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning = function(cond) {
    message(paste("file caused a warning:", fichier))
    message("Here's the original warning message:")
    message(cond)
    return(NULL)
  },
  finally = {
    message(paste("Processed file:", fichier))
  })
  return(c(fichier, text_parsed))
}

#récupération des titres, body and filename
body <-
  lapply(liste,
         parseTei,
         path = '//body|/TEI.2/teiHeader/fileDesc/titleStmt/title[1]',
         xmlelement = xmlValue)

#list to dataframe
library(plyr)
dfrm = ldply(body, rbind)
colnames(dfrm) <- c("file", "doc", "body")

#ajout d'une colonne file_id (oublié de l'extraire des XML...)
dfrm <-
  dfrm %>%
  mutate(file_id = str_extract(doc, "\\S+\\d+\\S+"))

#fichier texte contenant les codes eurovocs (http://optima.jrc.it/Acquis/JRC-Acquis.3.0/corpus/jrc-acquis-eurovoc-descriptors.txt)
jrc_eurovoc <-
  read_delim(
    "C:/Users/ettor/Desktop/Eurovoc/JRC Acqis Corpus/jrc-acquis-eurovoc-descriptors.txt",
    "\t",
    escape_double = FALSE,
    col_names = c("file_id", "X2", "eurovocs"),
    col_types = cols(X2 = col_skip()),
    trim_ws = TRUE
  )

jrc_eurovocs_tidy <-
  jrc_eurovoc %>%
  tidyr::separate_rows(eurovocs, sep = " ")

#fichier contenant le label des codes eurovoc en anglais (http://eurovoc.europa.eu/drupal/?q=fr/download/list_pt&cl=en)
listPt <-
  read_excel("C:/Users/ettor/Desktop/Eurovoc/listPt EurovocEN.xls")

#jointure entre eurovoc_tidy et la liste des labels
jrc_eurovocs_tidy <-
  jrc_eurovocs_tidy %>%
  left_join(listPt, by = c("eurovocs" = "ID"))
View(jrc_eurovocs_tidy)

#concatenation des lables dans une seule cellule, pour plus de lisibilité
library(data.table)
final_concat <-
  setDT(jrc_eurovocs_tidy)[, .(concat_eurovocs = paste(EN, collapse = " || ")), by = .(file_id)]

#on réunit le tout
final_merge <- 
  dfrm %>% 
  left_join(final_concat) %>% 
  na.omit()

#on ajoute quelques métadonnées
jrc_metatada <- 
  final_merge %>% 
  dplyr::mutate(file_name=paste0(stringr::str_extract(file, "jrc(\\d+|[aA-bB]).+"), ".txt")) %>% 
  dplyr::mutate(path = paste0("C:\\Users\\ettor\\Documents\\eurovoc_topicmodeling\\texts_jrc\\", file_name, ".txt")) %>% 
  select(file_name, concat_eurovocs) 
write_csv(jrc_metatada,"C:/Users/ettor/Desktop/jrc_metadata.csv")


data.table::fwrite(final_merge,"jrc_acquis_metadata.csv")

###############################################################
#PARTIE 2 : on crée un fichier dans le dossier  text_jrc 
#pour chaque cellule body du CSV
###############################################################

library(readr)
final_merge <- read_csv("jrc_acquis_metadata.csv")
csv <- 
  final_merge %>% 
  mutate(file_name=stringr::str_extract(file, "jrc(\\d+|[aA-bB]).+")) %>% 
  select(file_name, body) %>% 
  write_csv("C:/Users/ettor/Documents/eurovoc_topicmodeling/texts_jrc/csv.csv")

source("~/eurovoc_topicmodeling/csv2txts.R")

csv2txt("texts_jrc", label=1)

setwd("C:/Users/ettor/Documents/eurovoc_topicmodeling/")

###############################################################
#PARTIE 3 : quels mots reviennent trop souvent dans le corpus ?
###############################################################

library(quanteda)
library(readtext)

texte <- readtext("C:/Users/ettor/Desktop/final_merge.csv", textfield = "body")
myCorpus <- corpus(texte)

resume_corpus <- summary(myCorpus)

mydfm <- dfm(myCorpus, 
             remove = stopwords("english"), 
             remove_punct = TRUE,
             remove_numbers = TRUE,
             stem=TRUE)

topfeatures(mydfm, 100)  # 100 top words




