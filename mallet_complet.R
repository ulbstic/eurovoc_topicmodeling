Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_131/jre")
options(java.parameters = "-Xmx10g")

#this whole script is based on Ben Marwick's Day of Archaeology work https://github.com/benmarwick/dayofarchaeology
Sys.setenv(NOAWT = TRUE)

require(mallet)
require(dplyr)
library(tidytext)
#require(qdap) # a tester pour le préprocessing du corpus

#import the documents from the folder
documents <- mallet.read.dir("texts_jrc")

#Processing text : remove digits, (hyphens ?), ?non ascii characters, short words

documents$text <- gsub("[[:digit:]]+", "", documents$text)
#documents$text <- gsub("\\s?-\\s?", "", documents$text)
#cette fonction efface les non ascii, mais tranforme dönut en dnut...
#note : la fonction "latinize" de fingerprint() peut latiniser a posterirori les charactères non-ascii
#documents$text <- iconv(documents$text, "latin1", "ASCII", sub = "")
documents$text <- gsub("\\b\\w{1,2}\\s","", documents$text)

#stem. Très lent, à éviter. Voir si un package en C n'existe pas.
#Note : pas moyen d'utiliser plus d'un mc.cores dans Windows
#Note2 : essayer un algo moins aggressif que Porter
#source("~/eurovoc_topicmodeling/stemming.R")
#documents$text <- stem_text(documents$text, language = 'en', mc.cores = 1)

## Generate and save Stopwords
#Note : objectiver un peu mieux nos choix : tf-idf (voir dataframe word.freqs) ? 
#Voir aussi https://mimno.infosci.cornell.edu/papers/schofield_eacl_2017.pdf
#Ajouter des stopwords français, italiens ou allemands ?
stops <- c(
  tm::stopwords("english"),
  tm::stopwords("SMART"),    
  "commission",
  "shall",
  "agreement",
  "annex", 
  "regulation", 
  "decision", 
  "article", 
  "state", 
  "member", 
  "european", 
  "community",
  "protocol",
  "states",
  "council",
  "directives",
  "directive"
)  

write(stops, "~/eurovoc_topicmodeling/stopwords_en.txt")

#create Mallet instances
mallet.instances <-
  mallet.import(documents$id,
                documents$text,
                "~/eurovoc_topicmodeling/stopwords_en.txt",
                token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#tidytext. Voir comment l'adapter
# create a vector with one string per chapter
# collapsed <- documents %>%
#   anti_join(stop_words, by = "word") %>%
#   mutate(word = str_replace(word, "'", "")) %>%
#   group_by(document) %>%
#   summarize(text = paste(word, collapse = " "))
# # create an empty file of "stopwords"
# file.create(empty_file <- tempfile())
# docs <- mallet.import(collapsed$document, collapsed$text,
#                       empty_file)

#create topic trainer object
n.topics <- 127
topic.model <- MalletLDA(n.topics)

#load documents
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. 
topic.model$train(1000)

## NEW: run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <-
  mallet.doc.topics(topic.model, smoothed = T, normalized = T)

topic.words <-
  mallet.topic.words(topic.model, smoothed = T, normalized = T)

# UTILISER AUTRE METHODE
# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
# topic.docs <- t(doc.topics)
# topic.docs <- topic.docs / rowSums(topic.docs)
# write.csv(topic.docs, "~/eurovoc_topicmodeling/dcb-topic-docs.csv")

#methode https://txtplorer.wordpress.com/2015/07/23/reshape-mallet-output-r/
# tab <- doc.topics
# 
# NTOPICS = 30 # number of topics here
#   
# names <- c('num', 'text', paste(c('topic', 'proportion'), 
#                                   rep(1:NTOPICS, each = 2), sep = ""))
# 

rownames(doc.topics) <- documents$id
colnames(doc.topics) <- as.character(1:127)

# have a look at keywords for each topic
topics.labels

write.csv(topics.labels,
          "~/eurovoc_topicmodeling/dcb-topic-labels.csv")


# What are the top words in topic 2? 
# Notice that R indexes from 1 and Java from 0, so this will be the topic that mallet called topic 1.
mallet.top.words(topic.model, word.weights = topic.words[2,], num.top.words = 100)

## cluster based on shared words
#plot(hclust(dist(topic.words)), labels = topics.labels)

