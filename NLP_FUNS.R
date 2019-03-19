#### Natural Language Processing Functions ####
# Coursera - Data Science Specialization
# Functions to support Capstone Project
#
# Michael Coote
# 3/17/2019

library(tidyverse)
library(tm)
library(tm.plugin.webmining)
library(data.table)

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
  # cores <- 1
  # options(mc.cores = cores)  
  # tm_parLapply_engine(parallel::mclapply) 
  names <- names(corpus)
  corpus <- VCorpus(VectorSource(corpus))
  corpus <- tm_map(corpus, removeNonASCII)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, PlainTextDocument)
  names(corpus) <- names
  return(corpus)
}

removeRareWords <- function(corpus, highFreq) {
  tdm <- TermDocumentMatrix(corpus)
  tokens_to_remove <- findFreqTerms(tdm, highfreq = highFreq)
  print(length(findFreqTerms(tdm)))
  print(length(tokens_to_remove))
  rm(tdm)
  gc()
  n <- 1
  while(n < length(tokens_to_remove)) {
    print(n)
    st <- n
    en <- n + 500
    n <- en
    # corpus <- tm_map(corpus, content_transformer(removeWords), tokens_to_remove[st:en])
    corpus <- tm_map(corpus, removeWords, tokens_to_remove[st:en])
  }
  return(corpus)
}

buildNGrams <- function(corpus, ngramType = "unigram", myDictionary) {
  n <- switch(ngramType,
              unigram = 1,
              bigram = 2,
              trigram = 3,
              quadgram = 4)
  ngramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  }
  tdm_ngrams <- TermDocumentMatrix(corpus,
                                   control = list(tokenize = ngramTokenizer) )
  tdm_ngrams <- as_tibble(as.matrix(tdm_ngrams), rownames = "ngram")
  tdm_ngrams <- tdm_ngrams %>% 
    mutate(all = rowSums(.[-1])) %>% 
    arrange(desc(all))
}

enhanceNgram <- function(ngram) {
  # convert to data table
  # store last word in differnet column
  # index on first column
  # use only good ngrams, top x
}

nextWord <- function(quadgrams, bigrams, trigrams, phrase) {
  # enhance to use second column of data table
  pWords <- tolower(words(phrase))
  nWords <- length(pWords)
  pWords <- pWords[(nWords - 2):nWords]
  nWords <- length(pWords)
  for(i in 1:3) {
    pTrunc <- paste(pWords[(i):nWords], collapse = " ")
    searchPhrase <- paste0("^", pTrunc, " ")
    if(i == 1) {
      nw <- quadgrams$ngram[grepl(searchPhrase, quadgrams$ngram)]
    } else if(i == 2) {
      nw <- append(nw, trigrams$ngram[grepl(searchPhrase, trigrams$ngram)])
    }
    else {
      nw <- append(nw, bigrams$ngram[grepl(searchPhrase, bigrams$ngram)])
    }
  }
  return(nw)
}
