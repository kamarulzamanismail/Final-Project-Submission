#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(fluidPage(
  fluidRow(
    column(10,
      offset = 1,
      
      titlePanel("Next Word Predictor"),
      wellPanel("Next Word Predictor facilitate  users to continue with their typing by supplying
                the next word predictions. Please enter your word below."),
      fixedRow(
        mainPanel(
          span(
            textInput(
              "phrase",
              "Enter your word / phrase",
              value = ""
            ),
            actionButton("predictButton", "Next Word")
          )
        ),
        sidebarPanel(
          p(h5("Your input text:")),
          textOutput("phrase"),
          
          p(h5("Prediction of next word:")),
          textOutput("word")
        ),
        tabsetPanel(
        tabPanel("Documentation",
                 p(h4("Next Word Predictor:")),
                 br(),
                 helpText("This application predict the next word after your word"),
                 HTML("<u><b>Checking Method : </b></u>
                      <br>
                      1. User open the apps. <br>
                      2. User key-in word / phrase. <br>
                      3. User click button 'Next Word'.<br>
                      4. The Apps giving the answer by comparing the input with the dataset from ngrams.
                      
                      ")                
        )
      )
  )
)
)
))