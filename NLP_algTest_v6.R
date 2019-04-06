#### NLP - algorithm testing ####
# Michael Coote
# 3/27/2019

library(data.table)

source("nextWord.R")

# load ngrams
load("unigrams.RData")
load("bigrams_sm.RData")
load("trigrams_sm.RData")
load("quadgrams_sm.RData")
load("quintgrams_sm.RData")
load("hexagrams_sm.RData")

save(unigrams, file = "unigrams.RData", compress = FALSE)
save(bigrams, file = "bigrams_sm.RData", compress = FALSE)
save(trigrams, file = "trigrams_sm.RData", compress = FALSE)
save(quadgrams, file = "quadgrams_sm.RData", compress = FALSE)
save(quintgrams, file = "quintgrams_sm.RData", compress = FALSE)
save(hexagrams, file = "hexagrams_sm.RData", compress = FALSE)

# setkey(unigrams, nextWord)
# setkey(bigrams, ngram_start)
# setkey(trigrams, ngram_start)
# setkey(quadgrams, ngram_start)
# setkey(quintgrams, ngram_start)

# test pulling next word
phrase <- "of the"
phrase <- "a big thank you to"
phrase <- "big thank you to"
phrase <- "thank you to"
phrase <- "you to"
phrase <- "i'd"
phrase <- "the baseball"
phrase <- "at the end of the"
phrase <- "sarah likes to have"
phrase <- "test of jjkjklj"
nw <- nextWord4(unigrams, bigrams, trigrams, quadgrams, phrase)
nw <- nextWord5(unigrams, bigrams, trigrams, quadgrams, quintgrams, phrase)
nw <- nextWord6(unigrams, bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                phrase)
nw

phrase <- q1_phrase
phrase <- q2_phrase
phrase <- q10_phrase
phrase <- "adam sandler"

phrase <- 'you been way, way too'

phrase <- "might"
bigrams[phrase, nextWord]

