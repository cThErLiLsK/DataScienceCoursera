library(shiny)
library(ggplot2)
library(dplyr)
library(tidytext)

shinyUI(pageWithSidebar(
  headerPanel("Word prediction using n-grams"),
  sidebarPanel(
    textInput('inputString','Input'),
    submitButton('Submit')
  ),
  mainPanel(
    h4('This app predicts the next word given an input string.'),
    h4('Give it a try!'),
    br(),
    h4('Next word predicted:'),
    h4(textOutput('wordPrediction'))
  )
))
