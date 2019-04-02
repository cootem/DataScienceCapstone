#### Prune Ngrams ####
# Data Science Specialization - Capstone Project
#
#
# Michael Coote
# 4/2/2019

library(data.table)

# take count > 1 and top 3 in each group only
pruneNgram <- function(ngrams) {
  ngrams <- ngrams[count > 1]
  ngrams <- ngrams[order(-P), head(.SD, 3), ngram_start]
  ngrams[,count := NULL]
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
trigrams <- pruneNgram(trigrams)
format(object.size(trigrams), units = "Mb")
save(trigrams, file = "trigrams_sm.RData")
rm(trigrams)
gc()

load("quadgrams.RData")
format(object.size(quadgrams), units = "Mb")
quadgrams <- pruneNgram(quadgrams)
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
