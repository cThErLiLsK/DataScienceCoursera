<style>
.small-code pre code {
  font-size: 1em;
}
</style>

Word Prediction - Capstone Project
========================================================
author: Christian Tellkamp
date: 2018/04/30
autosize: true


Task and challenges
========================================================

The capstone project asks learner to develop a prediction text model and implement a Shiny app to showcase the solution.  

This task poses a number of challenges:
<small>
- The task itself is not an easy one - my phone failed almost all the test questions!
- Developing a model requires training the model with a large amount of data, which is computationally demanding.
- The shiny.io platform does not allow to store large files of raw or processed data.
</small>


My approach
========================================================

I opted for a relatively simple "hand-made" approach as I find it important in a learning setting to understand what I am doing.  

Specifically, my approach looks as follows:
<small>
- After some data cleaning, I created 1- up to 4-grams using the `tidytext` library.
- For each n-gram, I then calculated how frequent this was given a certain (n-1)-gram.
- In order to reduce the data volume, I chose only combinations which appeared more than once. 
- Based on this data, I searched a matching n-gram, starting with n = 4, given the user input. 
- This prediction step was implemented using a recursive function.
</small>


Code example - Recursive prediction algorithm
========================================================
class: small-code

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

There are a number of Pro's and Con's for my approach:

<small>
Pro's:
- I build the model largely from scratch, which was a great learning experience.
- The prediction process itself is very quick and robust to user entries.

Con's:
- Training with the whole dataset or using n-grams of order n > 4 was computationally not feasible, which limits the quality of the predictions.
- The n-gram database requires quite some memory.
- The prediction process is fairly simplistic without taking context beyond n-grams into account.</small>
