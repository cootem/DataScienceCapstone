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

# chunk lines into a list
chunk <- function(vec, cSize = 1e4) {
  l <- length(vec)
  split(vec, rep(1:ceiling(l/cSize), c(rep(cSize, l%/%cSize),l%%cSize) ) )
}

# load data
#getCorpusFiles()
# fileCleaner()
myCorpusData <- loadCorpus(folder = "final", filter = "clean", sampleN = 1)
sapply(myCorpusData, function(f) format(object.size(f), units = "Mb"))
sapply(myCorpusData, length)
myCorpus <- unlist(myCorpusData, use.names = TRUE)
myCorpus <- chunk(myCorpus, cSize = 1e5)
myCorpus <- lapply(myCorpus, function(f) paste(f, collapse = ". "))
myCorpus <- lapply(myCorpus, corpus)
rm(myCorpusData); gc()

unigrams <- lapply(myCorpus, function(corp) tokenize1(corp, ng = 1))
unigrams <- mergeNgramList1(unigrams)
unigrams[, P := count / sum(unigrams$count)]
setkey(unigrams, nextWord, P)
save(unigrams, file = "unigrams.RData" )
rm(unigrams)
gc()

# Tokenize ngrams to disk
files_hexagrams <- tokenizeList(myCorpus, ng = 6)
files_quintgrams <- tokenizeList(myCorpus, ng = 5)
files_quadgrams <- tokenizeList(myCorpus, ng = 4)
files_trigrams <- tokenizeList(myCorpus, ng = 3)
files_bigrams <- tokenizeList(myCorpus, ng = 2)

# read back, merge, create progabilitites
files_hexagrams <- list.files(path = "ngrams/", pattern = "6grams", 
                              full.names = TRUE)
hexagrams <- lapply(files_hexagrams, function(f) {
  print(f)
  h <- readRDS(f)
  print(nrow(h))
  h <- h[count >1]
  print(nrow(h))
  return(h)
  })
hexagrams <- mergeNgramList(hexagrams)
hexagrams <- preEstimateProbs(hexagrams)
save(hexagrams, file = "hexagrams.RData" )
rm(hexagrams)
gc()

files_quintgrams <- list.files(path = "ngrams/", pattern = "5grams", 
                              full.names = TRUE)
quintgrams <- lapply(files_quintgrams, readRDS)
quintgrams <- mergeNgramList(quintgrams)
quintgrams <- preEstimateProbs(quintgrams)
save(quintgrams, file = "quintgrams.RData" )
rm(quintgrams)
gc()

files_quadgrams <- list.files(path = "ngrams/", pattern = "4grams", 
                              full.names = TRUE)
quadgrams <- lapply(files_quadgrams, readRDS)
quadgrams <- mergeNgramList(quadgrams)
quadgrams <- preEstimateProbs(quadgrams)
save(quadgrams, file = "quadgrams.RData" )
rm(quadgrams)
gc()

files_trigrams <- list.files(path = "ngrams/", pattern = "3grams", 
                                            full.names = TRUE)
trigrams <- lapply(files_trigrams, readRDS)
trigrams <- mergeNgramList(trigrams)
trigrams <- preEstimateProbs(trigrams)
save(trigrams, file = "trigrams.RData" )
rm(trigrams)
gc()

files_bigrams <- list.files(path = "ngrams/", pattern = "2grams", 
                              full.names = TRUE)
bigrams <- lapply(files_bigrams, readRDS)
bigrams <- mergeNgramList(bigrams)
bigrams <- preEstimateProbs(bigrams)
save(bigrams, file = "bigrams.RData" )
rm(bigrams)
gc()

