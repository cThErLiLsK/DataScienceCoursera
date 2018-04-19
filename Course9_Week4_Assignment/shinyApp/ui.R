library(shiny)
library(UsingR)
library(ggplot2)
library(caret)
library(dplyr)
library(tidyr)
library(ElemStatLearn)
library(Hmisc)
library(HistData)
library(MASS)
library(bindrcpp)
library(survival)
library(Formula)
library(lattice)
library(e1071)

shinyUI(pageWithSidebar(
  headerPanel("Understanding ROC curves"),
  sidebarPanel(
    sliderInput('perc', 'Percentage of training data',value = 70, min = 10, max = 100, step = 5),
    checkboxInput('var1', 'Systolic blood pressure', value = TRUE),
    checkboxInput('var2', 'Tobacco', value = TRUE),
    checkboxInput('var3', 'Adiposity', value = TRUE),
    checkboxInput('var4', 'Age', value = TRUE),
    sliderInput('thresholdCat', 'Threshold between categories',value = 0.5, min = 0, max = 1, step = 0.01),
    submitButton('Submit')
  ),
  mainPanel(
    p('This app displays ROC curves based on South African Hearth Disease data.'),
    p('The generalized linear model that is used here predicts coronary heart disease on a test set.'),
    p('The app allows you to select a number of options:'),
    p('- You can chose how much of the training data to use for actually training the model.'),
    p('- You can also select to include or exclude a number of variables.'),
    p('- Finally, you can select a threshold for the decision boundary, and the graph will display a line representing this threshold.'),
    h4(' '),
    h4('Area Under Curve (AUC):'),
    h4(textOutput('auc')),
    plotOutput('plot')
  )
))
