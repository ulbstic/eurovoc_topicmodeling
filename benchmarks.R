library(xtable)
library(janitor)
library(microbenchmark)
library(broom)


View(thesaurus)

hist(thesaurus$length)

tabyl(thesaurus$length, sort = TRUE)


#benchmarking
mbm <-
microbenchmark(
jaccard <- bigrammes[1:100] %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2],
                                   by = c(bigrams_fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=0.1,
                                   ignore_case = TRUE,
                                   method = "jaccard", q=2),

cosine <- bigrammes[1:100] %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2],
                                   by = c(bigrams_fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=0.1,
                                   ignore_case = TRUE,
                                   method = "cosine", q=2),

levensthein <- bigrammes[1:100] %>%
  fuzzyjoin::stringdist_inner_join(thesaurus[length==2],
                                   by = c(bigrams_fingerprint_stem = "fingerprint_stem"),
                                   distance_col = "dist",
                                   max_dist=1,
                                   ignore_case = TRUE,
                                   method = "lv"),
times=5
)

print(mbm)

library(ggplot2)
autoplot(mbm)

str(mbm)
