#Cette fonction est censée matcher un terme avec un max d'eurovocs et renvoyer le Microthésaurus le plus fréquent

library(readr)
library(tidyverse)

eurovoc_thesaurus <- read_csv("eurovoc_thesaurus_complet.csv")

glimpse(eurovoc_thesaurus)


