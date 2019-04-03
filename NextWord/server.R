#### Next Word Predictor Shiny Server #####
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Michael Coote
# 4/3/2019

library(shiny)

# Define server logic required to show next word predictions
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
