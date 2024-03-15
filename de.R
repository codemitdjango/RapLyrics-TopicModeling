# Benötigte Librarys
library("quanteda")
library("tm")
library("tidyverse")
library("topicmodels")
library("LDAvis")
library("tsne")
library("wordcloud2")

# Daten sammeln
de_data <- read.csv("lyrics-german.csv", encoding = "UTF-8", header = TRUE)
de_corpus <- corpus(de_data$lyric)
de_lemma_data <- read.csv("baseform-german-transformed.csv", header = TRUE, encoding = "UTF-8")
de_lemma_data$inflected_form <- tolower(de_lemma_data$inflected_form)
de_lemma_data$lemma <- tolower(de_lemma_data$lemma)
de_lemma_data$inflected_form[is.na(de_lemma_data$inflected_form)] <- ""
de_lemma_data$lemma[is.na(de_lemma_data$lemma)] <- ""
de_stopwords <- readLines("german_stopwords.txt", encoding = "UTF-8")
de_topwords <- c("ja", "yeah", "n", "ey", "i", "the", "ah", "nen", "ne", "yo", "nem", "oh", "eh", "uh", "mi", "u", "ya", "yeahyeah", "woah", "ouh", "jaja", "pu_pu_pu_pu", "la", "ohoh", "like", "got", "get", "ma", "cho", "ek", "nich", "gehen", "sagen", "wouh", "я", "в", "ab", "tun", "ganz", "mehr", "per","g", "rrah", "bwa", "ol", "cant", "wouwouwou", "chu", "bangbang") 

# Vorverarbeitung des Textkorpus
de_corpus_tokens <- de_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace("_", " ", valuetype = "fixed") %>%
  tokens_replace("gehn", "gehen", valuetype = "fixed") %>%
  tokens_replace("gggucci", "gucci", valuetype = "fixed") %>%
  tokens_replace("nehmn", "nehmen", valuetype = "fixed") %>%
  tokens_replace("wolln", "wollen", valuetype = "fixed") %>%
  tokens_replace("seeln", "seelen", valuetype = "fixed") %>%
  tokens_replace("fahrn", "fahren", valuetype = "fixed") %>%
  tokens_replace(de_lemma_data$inflected_form, de_lemma_data$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = de_stopwords) %>%
  tokens_remove(pattern = de_topwords) %>%
  tokens_remove(pattern = eng_stopwords)

# eigentliche Analyse
de_collocations <- quanteda.textstats::textstat_collocations(de_corpus_tokens, min_count = 25)
de_corpus_tokens <- tokens_compound(de_corpus_tokens, de_collocations)
de_DTM <- de_corpus_tokens %>%
  dfm()
de_K <- 10 #Anzahl an Themen
de_topicModel <- LDA(de_DTM, de_K, method = "Gibbs", control = list(
  iter = 500, #Iterationen
  seed = 1, #Startwert
  verbose = 25, #wie häufig informartionen ausgegeben werden sollen
  alpha = 0.02))

# Daten überprüfen
de_tmResult <- posterior(de_topicModel)
attributes(de_tmResult)
ncol(de_DTM)
de_beta <- de_tmResult$terms
dim(de_beta)
rowSums(de_beta)
nrow(de_DTM)
de_theta <- de_tmResult$topics
dim(de_theta)
rowSums(de_theta)[1:10]
terms(de_topicModel, 10)
de_top5termsPerTopic <- terms(de_topicModel, 5)
de_topicNames <- apply(de_top5termsPerTopic, 2, paste, collapse = " ")

# VISUALISIERUNG FÜR JEDES THEMA (1-10)
de_topicToViz <- 1 # hier Thema angeben
de_top30terms <- sort(de_tmResult$terms[de_topicToViz, ], decreasing = TRUE)[1:30]
de_words <- names(de_top30terms)
de_probabilities <- sort(de_tmResult$terms[de_topicToViz, ], decreasing = TRUE)[1:30]
wordcloud2(data.frame(de_words, de_probabilities), shuffle = FALSE)

# Visualisierung
de_svd_tsne <- function(y) tsne(svd(y)$u)
de_json <- createJSON(phi = de_beta, theta = de_theta, doc.length = rowSums(de_DTM), vocab = colnames(de_DTM), term.frequency = colSums(de_DTM), mds.method = de_svd_tsne, plot.opts = list(xlab = "", ylab = ""))
serVis(de_json)
