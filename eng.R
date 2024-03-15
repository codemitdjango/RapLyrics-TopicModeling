# Benötigte Librarys
library("tidyverse")
library("quanteda")
library("topicmodels")
library("LDAvis")
library("tsne")
library("wordcloud2")

# Daten sammeln
eng_data <- read.csv("updated_rappers.csv", encoding = "UTF-8", header = TRUE)
eng_corpus <- corpus(eng_data$lyric)
eng_lemma_data <- read.csv("baseform_en.csv", encoding = "UTF-8")
eng_stopwords <- readLines("english_stopwords", encoding = "UTF-8")
eng_top_words <- c("yeah", "like", "im", "oh", "hey", "uh", "ayy", "get", "yo", "la", "ooh", "wee", "ya", "em", "ah", "e", "huh", "man", "thats", "put", "u", "go", "cause", "aint", "yall", "put", "see", "come","say", "know_know", "mi", "de", "know", "youre", "one", "let", "two", "ho", "dont", "cant")

# Vorverarbeitung des Textkorpus
eng_corpus_tokens <- eng_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% #alle Symbole, Zahlen, etc entfernen
  tokens_tolower() %>% #alle Buchstaben klein machen
  tokens_replace(eng_lemma_data$inflected_form, eng_lemma_data$lemma, valuetype = "fixed") %>% #Grundformen umwandel
  tokens_remove(pattern = eng_stopwords) %>% #padding = TRUE) # StopWörter werden entfernt, padding true, da wo entfernt wird ist eine lücke und nicht einfach weg, sinnvoll für meinen code?? fraglich
  tokens_remove(pattern = eng_top_words) %>% #Weitere Überflüssige Wörter entfernt
  tokens_replace("niggas", "nigga", valuetype = "fixed") %>%  #aus gegebenen Anlass
  tokens_replace("_", " ", valuetype = "fixed")

# eigentliche Analyse
eng_collocations <- quanteda.textstats:: textstat_collocations(eng_corpus_tokens, min_count = 25)
eng_corpus_tokens <- tokens_compound(eng_corpus_tokens, eng_collocations)
eng_DTM <- eng_corpus_tokens %>%
  dfm()
eng_zero_rows <- which(rowSums(eng_DTM) == 0)
eng_DTM <- eng_DTM[-eng_zero_rows, ]
eng_K <- 10 #Anzahl an Themen
eng_topicModel <- LDA(eng_DTM, eng_K, method = "Gibbs", control = list(
  iter = 500, #Iterationen
  seed = 1, #Startwert
  verbose = 25, #wie häufig informartionen ausgegeben werden sollen
  alpha = 0.02)) #Verteilung der Themen

# Daten überprüfen
eng_tmResult <- posterior(eng_topicModel)
attributes(eng_tmResult)
ncol(eng_DTM)
eng_beta <- eng_tmResult$terms
dim(eng_beta)
rowSums(eng_beta)
nrow(eng_DTM)
eng_theta <- eng_tmResult$topics
dim(eng_theta)
rowSums(eng_theta)[1:10]
terms(eng_topicModel, 10)
eng_top5termsPerTopic <- terms(eng_topicModel, 5)
eng_topicNames <- apply(eng_top5termsPerTopic, 2, paste, collapse = " ")

# VISUALISIERUNG FÜR JEDES THEMA (1-10)
eng_topicToViz <- 10 # hier Thema angeben
eng_top30terms <- sort(eng_tmResult$terms[eng_topicToViz, ], decreasing = TRUE)[1:30]
eng_words <- names(eng_top30terms)
eng_probabilities <- eng_top30terms
wordcloud2(data.frame(eng_words, eng_probabilities), shuffle = FALSE)


# Visualisierung
eng_svd_tsne <- function(x) tsne(svd(x)$u)
eng_json <- createJSON(phi = eng_beta, theta = eng_theta, doc.length = rowSums(eng_DTM), vocab = colnames(eng_DTM), term.frequency = colSums(eng_DTM), mds.method = eng_svd_tsne, plot.opts = list(xlab = "", ylab = ""))
serVis(eng_json)
