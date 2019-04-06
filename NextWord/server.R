#### Next Word Predictor Shiny Server #####
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Michael Coote
# 4/3/2019

library(shiny)

source('nextWord.R')

# load ngrams
load("unigrams.RData")
load("bigrams_sm.RData")
load("trigrams_sm.RData")
load("quadgrams_sm.RData")
load("quintgrams_sm.RData")
load("hexagrams_sm.RData")

# Define server logic required to show next word predictions
shinyServer(
  function(input, output) {
    nw <- reactive({
      nextWord6(unigrams, bigrams, trigrams, quadgrams, quintgrams, 
                hexagrams, input$stem) 
    })
    output$nextWord1 <- renderText( nw()[1,nextWord] )
    output$nextWord2 <- renderText( nw()[2,nextWord] )
    output$nextWord3 <- renderText( nw()[3,nextWord] )
    output$yourNextWord <- renderDataTable(nw())
    # verbatimTextOutput(nw)
})
