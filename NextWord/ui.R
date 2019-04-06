#### Next Word Predictor Shiny UI ####
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Michael Coote
# 4/3/2019

library(shiny)

# Define UI for application to show next word predictions
shinyUI(
    fluidPage(
        headerPanel("Data Science Capstone"),
        titlePanel("Next Word Predictor"),
        textInput("stem", "Enter a phrase", "type here"),
        div(style="width:500px; margin-left:20px",
            fluidRow( h3("My recomendations for the next word:") ),
            fluidRow(
                column(4, verbatimTextOutput("nextWord1")),
                column(4, verbatimTextOutput("nextWord2")),
                column(4, verbatimTextOutput("nextWord3")) 
            ),
            fluidRow( h3("Table of Probabilities"))
        ),
        fluidRow(dataTableOutput("yourNextWord"))
    )
)
