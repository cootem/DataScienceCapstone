#### NLP - algorithm testing ####
# Michael Coote
# 3/23/2019

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

hexagrams <- tokens(myCorpus, ng = 6)
save(hexagrams, file = "hexagrams.RData", compress = FALSE)
rm(hexagrams)

quintgrams <- tokens(myCorpus, ng = 5)
save(quintgrams, file = "quintgrams.RData", compress = FALSE)
rm(quintgrams)

quadgrams <- tokens(myCorpus, ng = 4)
save(quadgrams, file = "quadgrams.RData", compress = FALSE)
rm(quadgrams)

trigrams <- tokens(myCorpus, ng = 3)
save(trigrams, file = "trigrams.RData", compress = FALSE)
rm(trigrams)

bigrams <- tokens(myCorpus, ng = 2)
save(bigrams, file = "bigrams.RData", compress = FALSE)
rm(bigrams)

phrase <- "of the"
phrase <- "a big thank you to"
phrase <- "big thank you to"
phrase <- "thank you to"
phrase <- "you to"
nw <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, phrase)
nw[1:100]

phrase <- "might"
bigrams[phrase][.N][,nextWord]
bigrams[phrase][order(count),nextWord]
bigrams[phrase][order(-count), nextWord]

