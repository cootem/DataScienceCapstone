#### Natural Language Processing Functions ####
# Coursera - Data Science Specialization
# Functions to support Capstone Project
#
# Michael Coote
# March, 2019

library(tidyverse)
library(tm)

getCorpusFiles <- function(url_f = "") {
  if(url_f == "") url_f <- 
      "https://d396qusza40orc.cloudfront.net/dsscapstone/corpusset/Coursera-SwiftKey.zip"
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

loadCorpus <- function(folder = "final", filter = "US", sampleN = 100) {
  files_corpus <- list.files(folder, full.names = TRUE, recursive = TRUE)
  files_corpus_filtered <- files_corpus[grepl(filter, files_corpus)]
  corpus <- lapply(files_corpus_filtered, function(f) {
    file.pipe <- fileSampler(f, sampleN)
    read_lines(file.pipe) })
  files_corpus_sh <- list.files(folder, full.names = FALSE, recursive = TRUE)
  files_corpus_sh <- gsub("^(.*)/", "", files_corpus_sh)
  files_corpus_sh_US <- files_corpus_sh[grepl("US", files_corpus_sh)]
  names(corpus) <- gsub('en_US.|.txt', "", files_corpus_sh_US)
  return(corpus)
}

cleanCorpus <- function(corpus) {
  names <- names(corpus)
  corpus <- VCorpus(VectorSource(corpus))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, PlainTextDocument)
  names(corpus) <- names
  return(corpus)
}

buildNGrams <- function(corpus, ngramType = "unigram") {
  n <- switch(ngramType,
              unigram = 1,
              bigram = 2,
              trigram = 3)
  bigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  }
  tdm_ngrams <- as.matrix(
    TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer)))
  tdm_ngrams <- as_tibble(tdm_ngrams, rownames = "ngram")
  tdm_ngrams <- tdm_ngrams %>% 
    mutate(all = rowSums(.[-1])) %>% 
    arrange(desc(all))
}

nextWord <- function(bigrams, trigrams, phrase) {
  pWords <- tolower(words(phrase))
  nWords <- length(pWords)
  pTrunc <- paste(pWords[(nWords-1):nWords], collapse = " ")
  if(nWords == 1) {
    searchPhrase <- paste0("^", pTrunc)
    bigrams$ngram[grepl(searchPhrase, bigrams$ngram)]
  } else {
    searchPhrase <- paste0("^", pTrunc)
    trigrams$ngram[grepl(searchPhrase, trigrams$ngram)]
  }
}
