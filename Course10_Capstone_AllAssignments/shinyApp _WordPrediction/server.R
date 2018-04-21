library(shiny)
library(ggplot2)
library(dplyr)
library(tidytext)

shinyServer(
  function(input, output) {
    if (!exists('oneGram2Relevant')) {
      table <- read.csv("oneGram2Relevant.csv")
      oneGram2Relevant <- table[, 2:dim(table)[2]]
    }
    oneGram2Relevant$word <- as.character(oneGram2Relevant$word)
    
    if (!exists('biGram2Relevant')) {
      table <- read.csv("biGram2Relevant.csv")
      biGram2Relevant <- table[, 2:dim(table)[2]]
    }
    names(biGram2Relevant)[c(1,4)] <- c('ngram', 'nminusonegram')
    
    if (!exists('threeGram2Relevant')) {
      table <- read.csv("threeGram2Relevant.csv")
      threeGram2Relevant <- table[, 2:dim(table)[2]]
    }
    names(threeGram2Relevant)[c(1,4)] <- c('ngram', 'nminusonegram')
    
    if (!exists('fourGram2Relevant')) {
      table <- read.csv("fourGram2Relevant.csv")
      fourGram2Relevant <- table[, 2:dim(table)[2]]
    }
    names(fourGram2Relevant)[c(1,4)] <- c('ngram', 'nminusonegram')
    
    relevant <- rbind(biGram2Relevant, threeGram2Relevant, fourGram2Relevant)
    relevant$ngram <- as.character(relevant$ngram)
    relevant$nminusonegram <- as.character(relevant$nminusonegram)
    
    
    # function returns tidy input text
    inputTextReactive <- reactive ({
      inText <- input$inputString
      df_inText <- data.frame(text = inText)
      df_inText$text <- as.character(df_inText$text)
      df_words <- df_inText %>% unnest_tokens(word, text)
      words <- as.vector(df_words$word)
      l <- length(words)
      if(l == 0) {
        inputText <- ''
      }
      else if (l < 4) {
        inputText <- paste(words, collapse = ' ')
      }
      else {
        #print(words)
        words <- words[(l-2):l]
        #print(words)
        inputText <- paste(words, collapse = ' ')
      }
      inputText
    })
    
    prediction <- reactive ({
      inputText <- input$inputString
      
      if (inputText =='') {
        prediction <- oneGram2Relevant$word[1]
      }
      else {
        prediction <- relevant %>% filter(nminusonegram == inputText)
        prediction <- prediction$ngram[1]
        prediction <- strsplit(prediction, ' ')[[1]]
        prediction <- prediction[length(prediction)]
        if (is.na(prediction)) {
          inputText <- strsplit(inputText, ' ')[[1]]
          if (length(inputText) > 1) {
            inputText <- paste(inputText[2:length(inputText)], collapse = ' ')
          }
          else {
            inputText <- ''
          }
          print('Text used for prediction:')
          print(inputText)
          prediction <- prediction2(inputText)
        }
        else {
          print('Prediction found!')
          prediction
        }
      }
    })
    
    prediction2 <- function(inputText) {
      if (inputText =='') {
        prediction <- oneGram2Relevant$word[1]
      }
      else {
        prediction <- relevant %>% filter(nminusonegram == inputText)
        prediction <- prediction$ngram[1]
        prediction <- strsplit(prediction, ' ')[[1]]
        prediction <- prediction[length(prediction)]
        if (is.na(prediction)) {
          inputText <- strsplit(inputText, ' ')[[1]]
          if (length(inputText) > 1) {
            inputText <- paste(inputText[2:length(inputText)], collapse = ' ')
          }
          else {
            inputText <- ''
          }
          print('Text used for prediction:')
          print(inputText)
          prediction <- prediction2(inputText)
        }
        else {
          print('Prediction found!')
          prediction
        }
      }
    } 

    output$wordPrediction <- renderText({
      prediction()
    })

  }
)
