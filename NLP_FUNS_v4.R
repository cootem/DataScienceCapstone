#### Natural Language Processing Functions ####
# Coursera - Data Science Specialization
# Functions to support Capstone Project
#
# Michael Coote
# 3/31/2019

library(data.table)
library(readr)
library(quanteda)

getCorpusFiles <- function(url_f = "", file_corpus = "Coursera-Swiftkey.zip") {
  if(url_f == "") url_f <- 
     "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  # download.file(url = url_f, destfile = file_corpus, method = "libcurl")
  download.file(url = url_f, destfile = file_corpus)
  file <- list.files(pattern = "zip", full.names = TRUE)
  unzip(file)
  folder <- "final"
  files_corpus <- list.files(folder, full.names = TRUE, recursive = TRUE)
}

fileSampler <- function(fname, sampleN) {
  # awk returns every nth row of file
  pipeName <- sprintf("awk '!(NR%%%s)' %s", sampleN, fname)
  # shuf returns random sample of n rows, has a resource limit of 1e5 rows
  # pipeName <- sprintf("shuf -n %s %s", sampleN, fname)
  file.pipe <- pipe(pipeName)
}

loadCorpus <- function(folder = "final", filter = "US", sampleN = 1000) {
  files_corpus <- list.files(folder, full.names = TRUE, recursive = TRUE)
  files_corpus_filtered <- files_corpus[grepl(filter, files_corpus)]
  corpus <- lapply(files_corpus_filtered, function(f) {
    file.pipe <- fileSampler(f, sampleN)
    # read_lines(f, n_max = 2e6/sampleN)
    read_lines(file.pipe)
    } )
  files_corpus_sh <- list.files(folder, full.names = FALSE, recursive = TRUE)
  files_corpus_sh <- gsub("^(.*)/", "", files_corpus_sh)
  files_corpus_sh_US <- files_corpus_sh[grepl("US", files_corpus_sh)]
  names(corpus) <- gsub('en_US.|.txt', "", files_corpus_sh_US)
  return(corpus)
}

tokenize <- function(myCorpus, ng = 2) {
  cols_list <- as.list(1:(ng-1))
  ngrams <- tokens(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = TRUE, remove_hyphens = TRUE,
                   remove_url = TRUE,
                   ngrams = ng, concatenator = " ", verbose = TRUE)
  ngrams <- dfm(ngrams)
  ngrams <- dfm_trim(ngrams, min_termfreq = 0.9, termfreq_type = "quantile", 
                     verbose = TRUE)
  # ngrams <- dfm_trim(ngrams, min_termfreq = 2, termfreq_type = "count", verbose = TRUE)
  ngrams <- textstat_frequency(ngrams)
  ngrams <- setDT(ngrams)
  ngrams <- ngrams[, c(-3, -4, -5)]
  setnames(ngrams, "feature", "ngram_start")
  setnames(ngrams, "frequency", "count")
  ngrams[, nextWord := tstrsplit(ngram_start, " ", fixed = TRUE, keep = ng)]
  ngrams[, ngram_start := do.call(paste, 
                                  tstrsplit(ngram_start, " ", 
                                            fixed = TRUE,
                                            keep = cols_list)) ]
  setcolorder(ngrams, c("ngram_start", "nextWord", "count"))
  setkey(ngrams, ngram_start, count)
  return(ngrams)
}

tokenize1 <- function(myCorpus, ng = 2) {
  cols_list <- as.list(1:ng-1)
  ngrams <- tokens(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = TRUE, remove_hyphens = TRUE,
                   remove_url = TRUE,
                   ngrams = ng, concatenator = " ", verbose = TRUE)
  ngrams <- dfm(ngrams)
  ngrams <- dfm_trim(ngrams, min_termfreq = 0.9, termfreq_type = "quantile", 
                     verbose = TRUE)
  # ngrams <- dfm_trim(ngrams, min_termfreq = 2, termfreq_type = "count", verbose = TRUE)
  ngrams <- textstat_frequency(ngrams)
  ngrams <- setDT(ngrams)
  ngrams <- ngrams[, c(-3, -4, -5)]
  setnames(ngrams, "feature", "nextWord")
  setnames(ngrams, "frequency", "count")
  setcolorder(ngrams, c("nextWord", "count"))
  setkey(ngrams, nextWord, count)
  return(ngrams)
}

tokenizeList <- function(myCorpusList, ng = 2) {
  file_ngram <- character()
  for(i in names(myCorpusList)) {
    print(paste("creating", ng, "- grams for sub-corpus", i))
    ngrams <- tokenize(myCorpus[[i]], ng)
    file_ngram[i] <- paste0("ngrams/", ng, "grams_", i, ".RData")
    saveRDS(ngrams, file = file_ngram[i])
  }
  return(file_ngram)
}

mergeNgramList <- function(ngramsL) {
  ngramsL <- rbindlist(ngramsL)
  ngramsL <- ngramsL[, count := sum(count), .(ngram_start, nextWord)]
  ngramsL <- unique(ngramsL)
}

mergeNgramList1 <- function(ngramsL) {
  ngramsL <- rbindlist(ngramsL)
  ngramsL <- ngramsL[, count := sum(count), nextWord]
  ngramsL <- unique(ngramsL)
}

preEstimateProbs <- function(ngrams) {
  ngrams[, cStart := sum(count), ngram_start]
  ngrams[, P := count / cStart]
  ngrams <- ngrams[, .(ngram_start, nextWord, count, P)]
  setkey(ngrams, ngram_start)
  }

# return the next word from a phrase
nextWord <- function(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                     hexagrams, phrase) {
  pWords <- tolower(phrase)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 4) > 0, nWords - 4, 1):nWords]
  nWords <- length(pWords)
  nw <- data.table(pTrunc = character(), nextWord = character(), P = numeric(), Pn = numeric())
  k <- 0
  # use backoff prob Pngram(nw|w-1) = 0.4 * P(nw|w-1)
  for(i in (6 - nWords):6) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    nw <- switch(EXPR = j,
                 a = hexagrams[pTrunc][order(-P), .(pTrunc, nextWord, P, Pn = P)],
                 b = rbind(nw, quintgrams[pTrunc][order(-P),
                                                   .(pTrunc, nextWord, P, Pn = P*.4^(k-1))][1:.N]),
                 c = rbind(nw, quadgrams[pTrunc][order(-P), 
                                                  .(pTrunc, nextWord, P, Pn = P*.4^(k-1))][1:.N]),
                 d = rbind(nw, trigrams[pTrunc][order(-P), 
                                                 .(pTrunc, nextWord, P, Pn = P*.4^(k-1))][1:.N]),
                 e = rbind(nw, bigrams[pTrunc][order(-P), 
                                                .(pTrunc, nextWord, P, Pn = P*.4^(k-1))][1:.N]),
                 f = rbind(nw, unigrams[order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))][1:.N]) )
  }
  return(nw)
}

