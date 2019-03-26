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

#### testing ####
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
phrase <- "sarah like to have"
nw <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, phrase)
nw[1:100]

phrase <- "might"
bigrams[phrase][.N][,nextWord]
bigrams[phrase][order(count),nextWord]
bigrams[phrase][order(-count), nextWord]

#### experiment with other methods ####
ng <- 2
ngram <- tokens(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                 remove_symbols = TRUE, remove_separators = TRUE,
                 remove_twitter = TRUE, remove_hyphens = TRUE, 
                 remove_url = TRUE,
                 ngrams = ng, concatenator = " ", verbose = TRUE)
ngram <- dfm(ngram)
ngram_features <- textstat_frequency(ngram)
ngram_features <- setDT(ngram_features)
ngram_features <- ngram_features[, c(-4, -5)]
setnames(ngram_features, "feature", "ngram_start")
setnames(ngram_features, "frequency", "count")
ngram_features[, nextWord := tstrsplit(ngram_start, " ", fixed = TRUE, keep = ng)]
cols_list <- as.list(1:(ng-1))
ngram_features[, ngram_start := do.call(paste, tstrsplit(ngram_start, " ", fixed = TRUE, keep = cols_list)) ]
setcolorder(ngram_features, c("ngram_start", "nextWord", "count"))
setkey(ngram_features, ngram_start, count)

