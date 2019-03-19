#### NLP - algorithm testing ####
# Michael Coote
# 3/10/2019

source("NLP_FUNS.R")

# load whole corpus
corpus_all <- loadCorpus(folder = "final", filter = "US", sampleN = 1)

sapply(corpus_all, length)
format(object.size(corpus_all), units = "Mb")

phrase <- "Be grateful for the good times and keep the faith during the"
phrase_sh <- "during the "
# phrase_cleaned <- removeWords(phrase, stopwords())
# phrase_cleaned

# limit corpus to only items containing this phrase
corpus <- corpus_all; rm(corpus_all); gc()
#corpus <- lapply(corpus_all, function(f) f[grepl(phrase_sh, f)])

sapply(corpus, length)
format(object.size(corpus), units = "Mb")

corpus <- cleanCorpus(corpus)
corpus <- removeRareWords(corpus, highFreq = 10)
save(corpus, file = "corpus.RData")

gc()
format(object.size(corpus), units = "Mb")

# build ngrams
quadgrams <- buildNGrams(corpus, "quadgram")
trigrams <- buildNGrams(corpus, "trigram")
bigrams <- buildNGrams(corpus, "bigram")
# unigrams <- buildNGrams(corpus, "unigram")

# rows <- c(nrow(unigrams), nrow(bigrams), nrow(trigrams))
rows <- c(nrow(bigrams), nrow(trigrams))
names(rows) <- c("bigrams", "trigrams")
prettyNum(rows, big.mark = ",")

save(bigrams, trigrams, quadgrams, file = "nGrams.RData")

# predict
nextWords <- nextWord(quadgrams, bigrams, trigrams, phrase)
nextWords[1:20]
which(grepl('defense', nextWords))
which(grepl('crowd', nextWords))
which(grepl('players', nextWords))
which(grepl('referees', nextWords))

which(grepl('mall', nextWords))
which(grepl('grocery', nextWords))
which(grepl('beach', nextWords))
which(grepl('movies', nextWords))

which(grepl('worse', nextWords))
which(grepl('hard', nextWords))
which(grepl('sad', nextWords))
which(grepl('bad', nextWords))


# wrong: 4, 5, 9
# beer, world, happiest, (-crowd, -players), (-movies, -grocery, beach), way, time, fingers, (-sad, bad), insane

