Word Prediction
========================================================
author: Christian Tellkamp
date: 2018/04/30
autosize: true


Task and challenges
========================================================

The capstone project in the Data Science specialization by Johns Hopkins consisted in developing a prediction model that returned the next word given an input string and building a Shiny app to demonstrate the model which should be hosted on shiny.io platform.  

This task posed a number of challenges:

- The task itself is not an easy one - my phone failed almost all the test questions!
- Developing a model requires training the model with a large amount of data, which is computationally demanding.
- The shiny.io platform does not allow to store large files of potentially several hundred megabytes even if a fast algorithm to search the data could be implemented.


My approach
========================================================

I opted for a relatively simple "hand-made" approach rather than any sophisticated "off-the-shelf" solution as I find it important in a learning setting to understand what I am doing.  

Specifically, my approch looked as follows:

- I died some data cleaning and then calcuated 1- up to 4-grams using `tidytext` library for support.
- For each n-gram with 1 < n < 5, I calculated how frequent this was given a certain (n-1)-gram.
- In order to reduce the data volume, I chose only combinations which appeared more than once. 
- Based on this data, I then tried to find a matching n-gram, starting with n = 4, given the user input. If there was no match, I decreased n by one.
- This prediction step was implemented in a recursive function (shown on the next slide).


Code example - Recursive prediction algorithm
========================================================


```r
prediction <- function(inputText) {
  if (inputText =='') { prediction <- oneGram2Relevant$word[1] }
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
      else { inputText <- '' }
      prediction <- prediction(inputText)
    }
    else { prediction }
  }
}
```


Review - Pro's and Con's
========================================================

I managed to build the model in reasonable time and it works. Reviewing the model and my approach, I see a number of Pro's and Con's:   

Pro's:  

- I build the model largely from scratch, which was a great learning experience.
- The prediction process itself is very quick and robust to user entries.

Con's:  

- The initial generation of the n-grams etc. takes a while, and training with the whole dataset or using n-grams of order n > 4 was computationally not feasible, which limits the quality of the predictions.
- As the prediction basically uses a database for prediction, quite a bit of memory is needed. In the current version, using less than 5% of the available data, already > 20 megabytes of memory are needed.
- The prediction process is fairly simplistic and does not take context beyond the n-gram used for prediction into account.