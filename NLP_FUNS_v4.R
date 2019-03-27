#### Natural Language Processing Functions ####
# Coursera - Data Science Specialization
# Functions to support Capstone Project
#
# Michael Coote
# 3/18/2019

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
    # file.pipe <- fileSampler(f, sampleN)
    read_lines(f, n_max = 2e6/sampleN)
    # read_lines(file.pipe)
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
  ngrams <- ngrams[, c(-4, -5)]
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

tokenize1 <- function(myCorpus, ng = 1) {
  cols_list <- as.list(1:(ng-1))
  ngrams <- tokens(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = TRUE, remove_hyphens = TRUE,
                   remove_url = TRUE,
                   ngrams = ng, concatenator = " ", verbose = TRUE)
  ngrams <- dfm(ngrams)
  ngrams <- textstat_frequency(ngrams)
  ngrams <- setDT(ngrams)
  ngrams <- ngrams[, c(-4, -5)]
  setnames(ngrams, "feature", "nextWord")
  setnames(ngrams, "frequency", "count")
  setcolorder(ngrams, c("nextWord", "count"))
  setkey(ngrams, nextWord)
  return(ngrams)
}

# return the next word from a phrase
nextWord <- function(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                     hexagrams, phrase) {
  pWords <- tolower(phrase)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 4) > 0, nWords - 4, 1):nWords]
  nWords <- length(pWords)
  nw <- ""
  k <- 0
  for(i in (6 - nWords):6) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    nw <- switch(EXPR = j,
                 a = hexagrams[pTrunc][order(-count, -rank), nextWord],
                 b = append(nw, quintgrams[pTrunc][order(-count, -rank),
                                                   nextWord][1:100]),
                 c = append(nw, quadgrams[pTrunc][order(-count, -rank), 
                                                  nextWord][1:100]),
                 d = append(nw, trigrams[pTrunc][order(-count, -rank), 
                                                 nextWord][1:100]),
                 e = append(nw, bigrams[pTrunc][order(-count, -rank), 
                                                nextWord][1:100]),
                 f = append(nw, unigrams[order(-count, -rank), nextWord][1:20]) )
  }
  nw <- unique(nw)
  nw <- nw[!is.na(nw)]
  nw <- nw[nw != ""]
  return(nw)
}

