library(data.table)
library(stringr)
library(dplyr)

tokens_test <- fread("tokens_for_test.csv", encoding = "UTF-8")

joined_unigrams <- tokens_test %>%
  fuzzyjoin::stringdist_left_join(thesaurus,
                                  by = c(stem = "fingerprint_stem"),
                                  distance_col = "dist",
                                  max_dist=0,
                                  method = "lv")
View(joined_unigrams[,.(value, match, fingerprint_stem, term, dist)])

joined_bigrams <- bigrammes %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2 | length ==3],
                                   by = c(bigrams_fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=0,
                                   method = "lv")

View(joined_bigrams[,.(value, match, fingerprint_stem, term, dist)])

View(thesaurus[fingerprint_stem=="commun"])
