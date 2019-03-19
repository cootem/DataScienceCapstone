#### NLP - algorithm testing ####
# Michael Coote
# 3/17/2019

source("NLP_FUNS.R")

corpus <- loadCorpus(folder = "final", filter = "US", sampleN = 1)
sapply(corpus_all, length)
names <- names(corpus)
corpus <- SimpleCorpus(VectorSource(corpus))

corpus <- tm_map(corpus, removeNonASCII)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)
names(corpus) <- names

