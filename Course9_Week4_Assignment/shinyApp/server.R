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

shinyServer(
  function(input, output) {
    dfPlot <- reactive({
      set.seed(42)
      data(SAheart)
      df <- SAheart
      df$chd <- as.factor(df$chd)
      
      inTrain = createDataPartition(df$chd, p = 0.7, list = FALSE)
      train = df[ inTrain,]
      test = df[-inTrain,]
      
      var1 <- input$var1 #systolic blood pressure 1
      var2 <- input$var2 #tobacco 2
      var3 <- input$var3 #adiposity 4
      var4 <- input$var4 #age 9
      
      threshold <- input$thresholdCat
      
      percSample <- input$perc / 100
      
      selectedCols <- 1:10 * c(var1, var2, TRUE, var3, rep(TRUE, 4), var4, TRUE)
      
      trainSelected <- train[, selectedCols]
      testSelected <- test[, selectedCols]
      
      trainSelected <- trainSelected[sample(nrow(trainSelected), nrow(trainSelected) * percSample), ]
      
      model <- train(chd ~ ., method = 'glm', family = binomial(), preProcess = c('center', 'scale'), data = trainSelected)
      
      threshs <- data.frame(t(matrix(rep(seq(0, 1, by=0.01), dim(testSelected)[1]), 101, dim(testSelected)[1])))
      
      predictTestProb <- predict(model, testSelected, type = 'prob')[2]
      predictTest101 <- data.frame(sapply(threshs, function(x) predictTestProb > x))
      
      testSelectedTrue <- testSelected$chd == 1
      
      tpTest101 <- apply(predictTest101, 2, function(x) sum(x == testSelectedTrue & x) / length(x))
      tnTest101 <- apply(predictTest101, 2, function(x) sum(x == testSelectedTrue & !x) / length(x))
      fpTest101 <- apply(predictTest101, 2, function(x) sum(!(x == testSelectedTrue) & x) / length(x))
      fnTest101 <- apply(predictTest101, 2, function(x) sum(!(x == testSelectedTrue) & !x) / length(x))
      
      tprTest101 <- tpTest101 / (tpTest101 + fnTest101)
      fprTest101 <- fpTest101 / (tnTest101 + fpTest101)
      
      dfPlot <- data.frame(thresh = seq(0, 1, by=0.01), tpr = tprTest101, fpr = fprTest101)
      
      dfPlot <- dfPlot %>% group_by(fpr) %>% filter(tpr == max(tpr), thresh == max(thresh)) %>% ungroup() %>% arrange(fpr) 
    })  
    
    aucCalc <- reactive({
      with(dfPlot(), sum(diff(fpr) * (head(tpr,-1)+tail(tpr,-1)))/2)
    })
    
    output$auc <- renderText({
      round(aucCalc(),2)
    })
    
    thresholdFpr <- reactive({
      result <- with(dfPlot(), fpr[which(thresh <= input$thresholdCat)[1]]) 
    })
    
    plot <- reactive({    
      g <- ggplot(dfPlot(), aes(fpr, tpr)) + 
        geom_ribbon(aes(ymin = 0, ymax = tpr), fill = 'gray') + 
        labs(
          x = 'False Positive Rate (= 1- Specificity)', 
          y = 'True Positive Rate (= Sensitivity)') +
          coord_cartesian(xlim=c(0, 1)) +
        theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16)) +
        geom_vline(xintercept = thresholdFpr(),size = 1.5, color = 'red') +
        geom_label(x = thresholdFpr() + 0.05, y = 1- 0.05, 
                   label = paste("Threshold between\ncategories:", input$thresholdCat)) +
        theme_classic()
      g
      })
    
    output$plot <- renderPlot({plot()})
    
  }
)
