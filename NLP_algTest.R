#### NLP - algorithm testing ####
# Michael Coote
# 3/10/2019

source("NLP_FUNS.R")

corpus_all <- loadCorpus(folder = "final", filter = "US", sampleN = 1)
gc()

sapply(corpus_all, length)
format(object.size(corpus_all), units = "Mb")

phrase <- "his little"
# phrase_cleaned <- removeWords(phrase, stopwords())
# phrase_cleaned

corpus <- lapply(corpus_all, function(f) f[grepl(phrase, f)])
format(object.size(corpus), units = "Kb")

corpus <- cleanCorpus(corpus)

format(object.size(corpus), units = "Kb")

trigrams <- buildNGrams(corpus, "trigram")
bigrams <- buildNGrams(corpus, "bigram")
unigrams <- buildNGrams(corpus, "unigram")

rows <- c(nrow(unigrams), nrow(bigrams), nrow(trigrams))
names(rows) <- names(corpus)
prettyNum(rows, big.mark = ",")

# save(unigrams, bigrams, trigrams, file = "nGrams.RData")

nextWord(bigrams, trigrams, phrase)[1:50]


