#### Natural Language Processing Functions ####
# Coursera - Data Science Specialization
# Functions to support Capstone Project
#
# Michael Coote
# 3/18/2019

library(tidyverse)

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
    read_lines(file.pipe)   } )
  files_corpus_sh <- list.files(folder, full.names = FALSE, recursive = TRUE)
  files_corpus_sh <- gsub("^(.*)/", "", files_corpus_sh)
  files_corpus_sh_US <- files_corpus_sh[grepl("US", files_corpus_sh)]
  names(corpus) <- gsub('en_US.|.txt', "", files_corpus_sh_US)
  return(corpus)
}

tokenizer <- function(corpus, ng = 2) {
  ngrams <- dfm(myCorpus, remove_numbers = TRUE, remove_punct = TRUE, 
                remove_symbols = TRUE, remove_separators = TRUE,
                remove_twitter = TRUE, remove_hyphens = TRUE, 
                remove_url = TRUE,
                ngrams = ng, concatenator = " ")
  ngrams <- enframe(topfeatures(ngrams, 1e6))
  names(ngrams) <- c("ngram", "count")
  return(ngrams)
}

