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
# getCorpusFiles()
myCorpusData <- loadCorpus(folder = "final", filter = "US", sampleN = 1)
sapply(myCorpusData, function(f) format(object.size(f), units = "Mb"))
sapply(myCorpusData, length)
myCorpus <- unlist(myCorpusData, use.names = TRUE)
myCorpus <- chunk(myCorpus, cSize = 1e5)
myCorpus <- lapply(myCorpus, function(f) paste(f, collapse = ". "))
myCorpus <- lapply(myCorpus, corpus)
rm(myCorpusData); gc()

unigrams <- lapply(myCorpus, function(corp) tokenize1(corp, ng = 1))
unigrams <- mergeNgramList1(unigrams)
unigrams[, P := count / nrow(unigrams)]
setkey(unigrams, nextWord, P)
save(unigrams, file = "unigrams.RData", compress = FALSE)
rm(unigrams)
gc()

# hexagrams <- lapply(myCorpus, function(co) tokenize(co, ng = 6))
hexagrams <- lapply(myCorpus, function(corp) tokenize(corp, ng = 6))
hexagrams <- mergeNgramList(hexagrams)
hexagrams <- preEstimateProbs(hexagrams)
save(hexagrams, file = "hexagrams.RData", compress = FALSE)
rm(hexagrams)
gc()

quintgrams <- lapply(myCorpus, function(corp) tokenize(corp, ng = 5))
quintgrams <- mergeNgramList(quintgrams)
quintgrams <- preEstimateProbs(quintgrams)
save(quintgrams, file = "quintgrams.RData", compress = FALSE)
rm(quintgrams)
gc()

quadgrams <- lapply(myCorpus, function(corp) tokenize(corp, ng = 4))
quadgrams <- mergeNgramList(quadgrams)
quadgrams <- preEstimateProbs(quadgrams)
save(quadgrams, file = "quadgrams.RData", compress = FALSE)
rm(quadgrams)
gc()

trigrams <- lapply(myCorpus, function(corp) tokenize(corp, ng = 3))
trigrams <- mergeNgramList(trigrams)
trigrams <- preEstimateProbs(trigrams)
save(trigrams, file = "trigrams.RData", compress = FALSE)
rm(trigrams)
gc()

bigrams <- lapply(myCorpus, function(corp) tokenize(corp, ng = 2))
bigrams <- mergeNgramList(bigrams)
bigrams <- preEstimateProbs(bigrams)
save(bigrams, file = "bigrams.RData", compress = FALSE)
rm(bigrams)
gc()

