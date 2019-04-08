#### Next Word ####
# finds the next word prediction from a set of ngrams
# uses Stupid Back-Off
#
# Michael Coote
# 4/7/2019

library(data.table)


# return the next word from a phrase
nextWord6 <- function(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                      hexagrams, phrase) {
  pWords <- gsub('[[:punct:] ]+',' ',phrase)
  pWords <- tolower(pWords)
  pWords <- gsub(' [0-9]* ', ' ', pWords)
  pWords <- unlist(strsplit(pWords, split = " "))
  nWords <- length(pWords)
  pWords <- pWords[ifelse((nWords - 4) > 0, nWords - 4, 1):nWords]
  nWords <- length(pWords)
  nw <- data.table(pTrunc = character(), nextWord = character(), P = numeric(), 
                   Pn = numeric())
  k <- 0
  # use backoff prob Pngram(nw|w-1) = 0.4 * P(nw|w-1)
  for(i in (6 - nWords):6) {
    j <- letters[i]
    k <- ifelse(k < nWords, k + 1, k)
    pTrunc <- paste(pWords[k:nWords], collapse = " ")
    nw <- switch(EXPR = j,
                 a = hexagrams[pTrunc][order(-P), 
                                       .(pTrunc, nextWord, P, Pn = P)],
                 b = rbind(nw, 
                           quintgrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 c = rbind(nw, 
                           quadgrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 d = rbind(nw, 
                           trigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 e = rbind(nw, 
                           bigrams[pTrunc]
                           [order(-P), .(pTrunc, nextWord, P, Pn = P*.4^(k-1))]
                           [1:.N]),
                 f = rbind(nw, 
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
