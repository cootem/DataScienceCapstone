#### Prune Ngrams ####
# Data Science Specialization - Capstone Project
#
#
# Michael Coote
# 4/2/2019

library(data.table)

#### add length test to ngram_start and nextWord
#### remove outer quariles

# take count > 1 and top 4 in each group only
pruneNgram <- function(ngrams, minStLen = 1, maxStLen = 30, minNwLen = 1, maxNwLen = 20, minCnt = 1) {
  ngrams <- ngrams[count >= minCnt]
  ngrams[, StLen := nchar(ngram_start)]
  ngrams <- ngrams[StLen >= minStLen & StLen <= maxStLen]
  ngrams[, NwLen := nchar(nextWord)]
  ngrams <- ngrams[NwLen >= minNwLen & NwLen <= maxNwLen]
  ngrams <- ngrams[,.(ngram_start, nextWord, P)]
  ngrams <- ngrams[order(-P), head(.SD, 4), ngram_start]
  setkey(ngrams, ngram_start)
}

load("bigrams.RData")
format(object.size(bigrams), units = "Mb")
bigrams <- pruneNgram(bigrams)
format(object.size(bigrams), units = "Mb")
save(bigrams, file = "bigrams_sm.RData")
rm(bigrams)
gc()

load("trigrams.RData")
format(object.size(trigrams), units = "Mb")
trigrams <- pruneNgram(trigrams, minStLen = 4, maxStLen = 15, minNwLen = 1, maxNwLen = 10, minCnt = 0)
format(object.size(trigrams), units = "Mb")
save(trigrams, file = "trigrams_sm.RData")
rm(trigrams)
gc()

load("quadgrams.RData")
format(object.size(quadgrams), units = "Mb")
quadgrams <- pruneNgram(quadgrams, minStLen = 10, maxStLen = 18, minNwLen = 1, maxNwLen = 10, minCnt = 2)
format(object.size(quadgrams), units = "Mb")
save(quadgrams, file = "quadgrams_sm.RData")
rm(quadgrams)
gc()

load("quintgrams.RData")
format(object.size(quintgrams), units = "Mb")
quintgrams <- pruneNgram(quintgrams)
format(object.size(quintgrams), units = "Mb")
save(quintgrams, file = "quintgrams_sm.RData")
rm(quintgrams)
gc()

load("hexagrams.RData")
format(object.size(hexagrams), units = "Mb")
hexagrams <- pruneNgram(hexagrams)
format(object.size(hexagrams), units = "Mb")
save(hexagrams, file = "hexagrams.RData", compress = FALSE)
rm(hexagrams)
gc()
