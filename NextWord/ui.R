#### Next Word Predictor Shiny UI ####
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Michael Coote
# 4/7/2019

library(shiny)

# Define UI for application to show next word predictions
shinyUI(
    fluidPage(
        headerPanel("Data Science Capstone"),
        titlePanel("Next Word Predictor"),
        h4("Michael Coote"),
        a(href="https://cootem.github.io/DataScienceCapstone/",
          "https://cootem.github.io/DataScienceCapstone/",
          target = "_blank"),
        div(style="
            width: 800px;
            margin: 0 auto;
            border-radius: 25px;
            background-color: lightblue;
            box-shadow: 10px 10px 5px 0px rgba(0,0,0,0.75);",
            br(),
            div(style="
                width: 750px; 
                margin: 0 auto; 
                color: blue",
            textInput("stem", h3("Enter a Phrase"), "type here", width = "80%")
            ),
            div(style="width:750px; 
                margin:0 auto",
                fluidRow( h3("My recomendations for the next word:") ),
                fluidRow(
                    column(4, verbatimTextOutput("nextWord1")),
                    column(4, verbatimTextOutput("nextWord2")),
                    column(4, verbatimTextOutput("nextWord3")) 
                ),
                radioButtons("view", 
                             h3("Known Stem, Next Word and Probabilities"),
                             choices = c("On", "Off"),
                             selected = "Off"),
                fluidRow(dataTableOutput("yourNextWord"))
            )
            ),
        br()
    )
)
