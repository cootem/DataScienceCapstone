#### NLP - algorithm testing ####
# Michael Coote
# 3/23/2019

library(stringi)
library(data.table)
library(quanteda)

source("NLP_FUNS_v3.R")

# load data
# getCorpusFiles()
myCorpusData <- loadCorpus(folder = "final", filter = "US", sampleN = 1000)
myCorpusData <- unlist(myCorpusData, use.names = TRUE)
myCorpusData <- paste(myCorpusData, collapse = ". ")
myCorpus <- corpus(myCorpusData)

# explore
summary(myCorpus)

kwic(myCorpus, pattern = "movie")

# tokenize
ng <- 5
cols_list <- as.list(1:(ng-1))
ngrams <- dfm(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
              remove_symbols = TRUE, remove_separators = TRUE,
              remove_twitter = TRUE, remove_hyphens = TRUE, 
              remove_url = TRUE,
              ngrams = ng, concatenator = " ")
tokenCount <- ntoken(ngrams)
ngrams_dt <- convert(ngrams, to = "data.frame")
ngrams_dt_t <- setDT(transpose(ngrams_dt))
ngrams_dt_t[, ngram_start := colnames(ngrams_dt)]
ngrams_dt_t <- ngrams_dt_t[-1]
setnames(ngrams_dt_t, "V1", "count")
ngrams_dt_t[, nextWord := tstrsplit(ngram_start, " ", fixed = TRUE, keep = ng)]
ngrams_dt_t[, ngram_start := do.call(paste, tstrsplit(ngram_start, " ", fixed = TRUE, keep = cols_list)) ]
setkey(ngrams_dt_t, count)
setorder(ngrams_dt_t, -count)
ngrams_dt_t[1:3]

setcolorder(ngrams_dt_t, c("ngram_start", "count"))



bigrams <- tokenizer(corpus, ng = 2)
bigrams$start <- word(bigrams$ngram, start = 1, end = 1)
bigrams$nextWord <- stri_extract_last_words(bigrams$ngram)

trigrams <- tokenizer(corpus, ng = 3)
trigrams$start <- word(trigrams$ngram, start = 1, end = 2)
trigrams$nextWord <- stri_extract_last_words(trigrams$ngram)

quadgrams <- tokenizer(corpus, ng = 4)
quadgrams$start <- word(quadgrams$ngram, start = 1, end = 3)
quadgrams$nextWord <- stri_extract_last_words(quadgrams$ngram)

save(bigrams, trigrams, quadgrams, file = "nGrams.RData", compress = FALSE)

#### old ####
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
