#### Next Word ####
# finds the next word prediction from a set of ngrams
# uses Stupid Back-Off
#
# Michael Coote
# 4/3/2019

library(data.table)

# return the =next word from a phrase
nextWord4 <- function(unigrams, bigrams, trigrams, quadgrams, phrase) {
  pWords <- tolower(phrase)
  pWords <- gsub("(?!')[[:punct:]]", "", pWords, perl=TRUE)
  pWords <- gsub("\\d+", "", pWords, perl=TRUE)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 2) > 0, nWords - 2, 1):nWords]
  nWords <- length(pWords)
  nw <- data.table(pTrunc = character(), nextWord = character(), P = numeric(), Pn = numeric())
  k <- 0
  # use backoff prob Pngram(nw|w-1) = 0.4 * P(nw|w-1)
  for(i in (4 - nWords):4) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    nw <- switch(EXPR = j,
                 a = quadgrams[pTrunc][order(-P), .(pTrunc, nextWord, P, Pn = P)],
                 b = rbind(nw, 
                           trigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 c = rbind(nw, 
                           bigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 d = rbind(nw, 
                           unigrams[order(-P), 
                                    .(pTrunc, 
                                      nextWord, 
                                      P, 
                                      Pn = P*.4^(k-1))]
                           [1:3]) )
  }
  nw <- nw[order(-P)]
  return(nw)
}

# return the next word from a phrase
nextWord5 <- function(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                      phrase) {
  pWords <- phrase #### strip punctuation, numbers
  pWords <- tolower(pWords)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 3) > 0, nWords - 3, 1):nWords]
  nWords <- length(pWords)
  nw <- data.table(pTrunc = character(), nextWord = character(), P = numeric(), 
                   Pn = numeric())
  k <- 0
  # use backoff prob Pngram(nw|w-1) = 0.4 * P(nw|w-1)
  for(i in (5 - nWords):5) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    
    nw <- switch(EXPR = j,
                 a = quintgrams[pTrunc][order(-P), 
                                        .(pTrunc, nextWord, P, Pn = P)],
                 b = rbind(nw, 
                           quadgrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 c = rbind(nw, 
                           trigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 d = rbind(nw, 
                           bigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 e = rbind(nw, 
                           unigrams[order(-P), 
                                    .(pTrunc, 
                                      nextWord, 
                                      P, 
                                      Pn = P*.4^(k-1))]
                           [1:3]) )
  }
  nw <- nw[order(-Pn)]
  nw[!duplicated(nextWord)]
}

# return the next word from a phrase
nextWord6 <- function(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                      phrase) {
  pWords <- phrase #### strip punctuation, numbers
  pWords <- tolower(pWords)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 3) > 0, nWords - 3, 1):nWords]
  nWords <- length(pWords)
  nw <- data.table(pTrunc = character(), nextWord = character(), P = numeric(), 
                   Pn = numeric())
  k <- 0
  # use backoff prob Pngram(nw|w-1) = 0.4 * P(nw|w-1)
  for(i in (5 - nWords):5) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    
    nw <- switch(EXPR = j,
                 a = quintgrams[pTrunc][order(-P), 
                                        .(pTrunc, nextWord, P, Pn = P)],
                 b = rbind(nw, 
                           quadgrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 c = rbind(nw, 
                           trigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 d = rbind(nw, 
                           bigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 e = rbind(nw, 
                           unigrams[order(-P), 
                                    .(pTrunc, 
                                      nextWord, 
                                      P, 
                                      Pn = P*.4^(k-1))]
                           [1:3]) )
  }
  nw <- nw[order(-Pn)]
  nw[!duplicated(nextWord)]
}
