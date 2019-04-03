#### Next Word Predictor Shiny UI ####
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Michael Coote
# 4/3/2019

library(shiny)

source('nextWord.R')

# Define UI for application to show next word predictions
shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Predictor"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
