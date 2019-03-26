#### Data Science Capstone - Quiz 3 ####
#
# Michael Coote
# 3/24/2019

source("NLP_FUNS_v4.R")

load("bigrams.RData")
load("trigrams.RData")
load("quadgrams.RData")
load("quintgrams.RData")
load("hexagrams.RData")

#### Q1 ####
q1_phrase <- 
  "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
q1_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
               phrase = q1_phrase)
q1_choices <- c('give', 'eat', 'sleep', 'die')
unlist(sapply(q1_choices, function(ch) which(q1_ans == ch)), use.names = TRUE)
q1_ans[1:min(length(q1_ans), 10)]
# give   eat sleep   die 
# 18    93   225    85 
# give - wrong
# die

#### Q2 ####
q2_phrase <- 
  "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
q2_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q2_phrase)
q2_choices <- c('financial', 'horticulture', 'spiritual', 'marital')
unlist(sapply(q2_choices, function(ch) which(q2_ans == ch)), use.names = TRUE)
q2_ans[1:min(length(q2_ans), 10)]
# financial spiritual   marital 
# 870      2099      4473 
# financial - wrong
# marital

#### Q3 ####
q3_phrase <- 
  "I'd give anything to see arctic monkeys this"
q3_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q3_phrase)
q3_choices <- c('decade', 'month', 'morning', 'weekend')
sapply(q3_choices, function(ch) which(q3_ans == ch))
q3_ans[1:min(length(q3_ans), 10)]
# decade   month morning weekend 
# 468      10       4       7 
# morning - wrong
# weekend

#### Q4 ####
q4_phrase <- 
  "Talking to your mom has the same effect as a hug and helps reduce your"
q4_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q4_phrase)
q4_choices <- c('stress', 'happiness', 'sleepiness', 'hunger')
unlist(sapply(q4_choices, function(ch) which(q4_ans == ch)), use.names = TRUE)
q4_ans[1:min(length(q4_ans), 10)]
# stress happiness    hunger 
# 863       396      1342 
# happiness - wrong
# stress

#### Q5 ####
q5_phrase <- 
  "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
q5_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q5_phrase)
q5_choices <- c('minute', 'look', 'walk', 'picture')
sapply(q5_choices, function(ch) which(q5_ans == ch))
q5_ans[1:min(length(q5_ans), 10)]
# minute    look    walk picture 
# 51       3      14       5 
# look - wrong
# minute - wrong

#### Q6 ####
q6_phrase <- 
  "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
q6_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q6_phrase)
q6_choices <- c('account', 'incident', 'case', 'matter')
sapply(q6_choices, function(ch) which(q6_ans == ch))
q6_ans[1:min(length(q6_ans), 10)]
# account incident     case   matter 
# 1990      401        2      361 
# case - wrong
# matter

#### Q7 ####
q7_phrase <- 
  "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
q7_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q7_phrase)
q7_choices <- c('arm', 'finger', 'hand', 'toe')
unlist(sapply(q7_choices, function(ch) which(q7_ans == ch)), use.names = TRUE)
q7_ans[1:min(length(q7_ans), 10)]
# arm hand 
# 193   15 
# hand - correct

#### Q8 ####
q8_phrase <- 
  "Every inch of you is perfect from the bottom to the"
q8_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q8_phrase)
q8_choices <- c('side', 'center', 'top', 'middle')
unlist(sapply(q8_choices, function(ch) which(q8_ans == ch)), use.names = TRUE)
q8_ans[1:min(length(q8_ans), 10)]
# side center    top middle 
# 41    110      1     94 
# top - correct

#### Q9 ####
q9_phrase <- 
  "I’m thankful my childhood was filled with imagination and bruises from playing"
q9_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q9_phrase)
q9_choices <- c('outside', 'weekly', 'inside', 'daily')
unlist(sapply(q9_choices, function(ch) which(q9_ans == ch)), use.names = TRUE)
q9_ans[1:min(length(q9_ans), 10)]
# outside  inside 
# 57     262 
# outside - correct

#### Q10 ####
q10_phrase <- 
  "I like how the same people are in almost all of Adam Sandler's"
q10_ans <- nextWord(bigrams, trigrams, quadgrams, quintgrams, hexagrams, 
                   phrase = q10_phrase)
q10_choices <- c('pictures', 'movies', 'stories', 'novels')
unlist(sapply(q10_choices, function(ch) which(q10_ans == ch)), use.names = TRUE)
q10_ans[1:min(length(q10_ans), 10)]
# WRONG - no answers
# movies

#### lessons ####
# build ngrams individual and save to allow for larger sets
# use backoff
# use kn prediction
# lexical dispersion - https://quanteda.io/articles/pkgdown/replication/digital-humanities.html


