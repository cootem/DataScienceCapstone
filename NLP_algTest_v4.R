#### NLP - algorithm testing ####
# Michael Coote
# 3/23/2019

library(stringi)
library(data.table)
library(quanteda)

source("NLP_FUNS_v4.R")

# load data
# getCorpusFiles()
myCorpusData <- loadCorpus(folder = "final", filter = "US", sampleN = 100)
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

#### testing ####
load("unigrams.RData")
load("bigrams.RData")
load("trigrams.RData")
load("quadgrams.RData")
load("quintgrams.RData")
load("hexagrams.RData")

phrase <- "of the"
phrase <- "a big thank you to"
phrase <- "big thank you to"
phrase <- "thank you to"
phrase <- "you to"
phrase <- "i'd"
phrase <- "the baseball"
phrase <- "at the end of the"
phrase <- "sarah likes to have"
phrase <- "test of jjkjklj"
nw <- nextWord(unigrams, bigrams, trigrams, quadgrams, quintgrams, hexagrams, phrase)
nw[1:min(20, length(nw))]

phrase <- "might"
bigrams[phrase][.N][,nextWord]
bigrams[phrase][order(count),nextWord]
bigrams[phrase][order(-count), nextWord]

#### experiment with other methods ####
