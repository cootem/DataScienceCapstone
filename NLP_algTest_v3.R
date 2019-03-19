#### NLP - algorithm testing ####
# Michael Coote
# 3/17/2019

library(stringi)
library(tidyverse)
library(quanteda)

source("NLP_FUNS_v3.R")

# load
myCorpusData <- loadCorpus(folder = "final", filter = "US", sampleN = 10)
myCorpusData <- unlist(myCorpusData, use.names = TRUE)
myCorpus <- corpus(myCorpusData)

# explore
summary(myCorpus)

kwic(myCorpus, pattern = "movie")

# tokenize

unigrams <- tokens(myCorpus, 
                   remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE, 
                   remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE )
bigrams <- tokens(myCorpus, 
                  remove_numbers = TRUE, remove_punct = TRUE, 
                  remove_symbols = TRUE, remove_separators = TRUE, 
                  remove_twitter = TRUE, remove_hyphens = TRUE, 
                  remove_url = TRUE,
                  ngrams = 2, concatenator = " ")
bigrams_all <- unlist(bigrams, use.names = FALSE)

bigrams_dfm <- dfm(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE,
                   ngrams = 2, concatenator = " ")
bigrams_dfm <- enframe(topfeatures(bigrams_dfm, 1e5))
names(bigrams_dfm) <- c("ngram", "count")

bigrams <- tokenizer(corpus, ng = 2)
#### move to function, replace column rather than add new, use data table
bigrams$start <- word(bigrams$ngram, start = 1, end = 1)
bigrams$nextWord <- stri_extract_last_words(bigrams$ngram)

trigrams <- tokenizer(corpus, ng = 3)
trigrams$start <- word(trigrams$ngram, start = 1, end = 2)
trigrams$nextWord <- stri_extract_last_words(trigrams$ngram)

quadgrams <- tokenizer(corpus, ng = 4)
quadgrams$start <- word(quadgrams$ngram, start = 1, end = 3)
quadgrams$nextWord <- stri_extract_last_words(quadgrams$ngram)

save(bigrams, trigrams, quadgrams, file = "nGrams.RData", compress = FALSE)

