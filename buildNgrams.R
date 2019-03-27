### Build Ngrams ####
# Data Science Specialization - Capstone Project
#
# builds ngrams, save to disk in RData files
#
# Michael Coote
# 3/27/2019


library(stringi)
library(data.table)
library(quanteda)

source("NLP_FUNS_v4.R")

# load data
# getCorpusFiles()
myCorpusData <- loadCorpus(folder = "final", filter = "US", sampleN = 10)
myCorpusData <- unlist(myCorpusData, use.names = TRUE)
myCorpusData <- paste(myCorpusData, collapse = ". ")
myCorpus <- corpus(myCorpusData)
rm(myCorpusData); gc()

hexagrams <- tokenize(myCorpus, ng = 6)
save(hexagrams, file = "hexagrams.RData", compress = FALSE)
rm(hexagrams)

quintgrams <- tokenize(myCorpus, ng = 5)
save(quintgrams, file = "quintgrams.RData", compress = FALSE)
rm(quintgrams)

quadgrams <- tokenize(myCorpus, ng = 4)
save(quadgrams, file = "quadgrams.RData", compress = FALSE)
rm(quadgrams)

trigrams <- tokenize(myCorpus, ng = 3)
save(trigrams, file = "trigrams.RData", compress = FALSE)
rm(trigrams)

bigrams <- tokenize(myCorpus, ng = 2)
save(bigrams, file = "bigrams.RData", compress = FALSE)
rm(bigrams)

unigrams <- tokenize1(myCorpus, ng = 1)
save(unigrams, file = "unigrams.RData", compress = FALSE)
rm(unigrams)
