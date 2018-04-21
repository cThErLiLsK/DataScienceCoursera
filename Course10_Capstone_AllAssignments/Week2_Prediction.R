library(tidytext)

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
inputText <- function(inText) {
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
}

prediction <- function(inputText) {
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
      prediction <- prediction(inputText)
    }
    else {
      print('Prediction found!')
      prediction
    }
  }
}

inputs <-c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
           "You're the reason why I smile everyday. Can you follow me please? It would mean the",
           "Hey sunshine, can you follow me and make me the",
           "Very early observations on the Bills game: Offense still struggling but the",
           "Go on a romantic date at the",
           "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
           "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
           "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
           "Be grateful for the good times and keep the faith during the",
           "If this isn't the cutest thing you've ever seen, then you must be")

#inputs <- c('after is a')

for (i in inputs) {
  print('Input text:')
  print(i)
  inp <- inputText(i)
  print('Phrase normalized:')
  print(inp)
  pred <- prediction(inp)
  print('Next predicted word:')
  print(pred)
  print(' ')
}

inputs <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
            "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
            "I'd give anything to see arctic monkeys this",
            "Talking to your mom has the same effect as a hug and helps reduce your",
            "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
            "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
            "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
            "Every inch of you is perfect from the bottom to the",
            "I’m thankful my childhood was filled with imagination and bruises from playing",
            "I like how the same people are in almost all of Adam Sandler's")

for (i in inputs) {
  print('Input text:')
  print(i)
  inp <- inputText(i)
  print('Phrase normalized:')
  print(inp)
  pred <- prediction(inp)
  print('Next predicted word:')
  print(pred)
  print(' ')
}