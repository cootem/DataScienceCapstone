#### NLP - algorithm testing ####
# Michael Coote
# 3/27/2019

library(stringi)
library(data.table)
library(quanteda)

source("NLP_FUNS_v4.R")

# load ngrams
load("unigrams.RData")
load("bigrams.RData")
load("trigrams.RData")
load("quadgrams.RData")
load("quintgrams.RData")
load("hexagrams.RData")

# test pulling next word
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


