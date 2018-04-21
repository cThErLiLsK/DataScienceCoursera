library(dplyr)
library(tidytext)
library(ggplot2)

set.seed(42)

path <- './Coursera-SwiftKey/final/en_US/'

blogs <- 'en_US.blogs.txt'
news <- 'en_US.news.txt'
twitter <- 'en_US.twitter.txt'

if (!exists('linesBlogs')) {
  connection <- file(paste0(path, blogs), "rb")
  linesBlogs <- readLines(connection, encoding="utf-8")
  close(connection)
}

if (!exists('linesNews')) {
  connection <- file(paste0(path, news), "rb")
  linesNews <- readLines(connection, encoding="utf-8")
  #lines <- readLines(connection, 1)
  close(connection)
}

if (!exists('linesTwitter')) {
  connection <- file(paste0(path, twitter), "rb")
  linesTwitter <- readLines(connection, encoding="utf-8")
  #lines <- readLines(connection, 1)
  close(connection)
} 

# sample for testing purposes
sampleBlogs <- linesBlogs[sample(length(linesBlogs), 20000)]
sampleNews <- linesNews[sample(length(linesNews), 20000)]
sampleTwitter <- linesTwitter[sample(length(linesTwitter), 20000)]

# combine into one dataframe
text_df <- rbind(data.frame(text = sampleBlogs), data.frame(text = sampleNews), data.frame(text = sampleTwitter))
text_df$text <- as.character(text_df$text) 

# remove numbers as they do not seem really helpful for prediction and are not removed automatically at later stage when creating ngrams
text_df$text <- gsub('[0-9]+', '', text_df$text)

# replace 'â€™' and 'â€œ' with single and apostrophe as apostprophe is not properly read
text_df$text <- gsub('â€™', '\'', text_df$text)
text_df$text <- gsub('â€œ', '"', text_df$text)

# remove remaining 'â' and 'Â' 
text_df$text <- gsub('â€', '"', text_df$text)
text_df$text <- gsub('[âÂ]', '"', text_df$text)

# generate oneGrams
oneGram <- text_df %>% unnest_tokens(word, text)

# count oneGrams, select most popular 10 and display
oneGramTop10 <- oneGram %>% count(word, sort = TRUE) %>% 
  top_n(10) %>% mutate(word = reorder(word, n))

# this is ordering without using dplyr
# oneGramTop10$word <- factor(oneGramTop10$word , levels = oneGramTop10$word [order(oneGramTop10$n)])
ggplot(aes(word, n), data = oneGramTop10) + geom_col() + xlab(NULL) + coord_flip()

# generate biGrams
biGram <- text_df %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

# count biGrams, select most popular 10 and display
biGram <- biGram %>% count(bigram, sort = TRUE) %>% mutate(bigram = reorder(bigram, n))
biGramTop10 <- biGram[1:10,]

ggplot(aes(bigram, n), data = biGramTop10) + geom_col() + xlab(NULL) + coord_flip()

# display density distribution of biGrams on log scale
biGram$index <- seq(1, dim(biGram)[1])
ggplot(aes(index, log(n)), data = biGram) + geom_line()

# generate threeGrams
threeGram <- text_df %>% unnest_tokens(threegram, text, token = "ngrams", n = 3)

# count threeGrams, select most popular 10 and display
threeGram <- threeGram %>% count(threegram, sort = TRUE) %>% mutate(threegram = reorder(threegram, n))
threeGramTop10 <- threeGram[1:10,]

ggplot(aes(threegram, n), data = threeGramTop10) + geom_col() + xlab(NULL) + coord_flip()

# display density distribution of threeGrams on log scale
threeGram$index <- seq(1, dim(threeGram)[1])
ggplot(aes(index, log(n)), data = threeGram) + geom_line()

# generate fourGrams
fourGram <- text_df %>% unnest_tokens(fourgram, text, token = "ngrams", n = 4)

# count fourGrams, select most popular 10 and display
fourGram <- fourGram %>% count(fourgram, sort = TRUE) %>% mutate(fourgram = reorder(fourgram, n))
fourGramTop10 <- fourGram[1:10,]

ggplot(aes(fourgram, n), data = fourGramTop10) + geom_col() + xlab(NULL) + coord_flip()

# display density distribution of fourGrams on log scale
fourGram$index <- seq(1, dim(fourGram)[1])
ggplot(aes(index, log(n)), data = fourGram) + geom_line()

# add (n-1)-grams to n-grams
biGram$bigram <- as.character(biGram$bigram)
biGram$onegram <- sapply(biGram$bigram, function(x) paste(strsplit(x, ' ')[[1]][1]))

threeGram$threegram <- as.character(threeGram$threegram)
threeGram$bigram <- sapply(threeGram$threegram, function(x) paste(strsplit(x, ' ')[[1]][1], 
                                                          strsplit(x, ' ')[[1]][2]))

fourGram$fourgram <- as.character(fourGram$fourgram)
fourGram$threegram <- sapply(fourGram$fourgram, function(x) paste(strsplit(x, ' ')[[1]][1], 
                                                                  strsplit(x, ' ')[[1]][2],
                                                                  strsplit(x, ' ')[[1]][3]))

# find most frequent (n-1)-grams
oneGram2Relevant <- oneGramTop10 # no need for further filtering

biGram2 <- biGram %>% group_by(onegram) %>% mutate(count = n())
biGram2 <- biGram2 %>% filter(n == max(n)) %>% arrange(desc(n), onegram, bigram, n) # please note: this can return more than one row per (n-i)-gram
biGram2Relevant <- biGram2 %>% filter((n>1) | (count > 1))

threeGram2 <- threeGram %>% group_by(bigram) %>% mutate(count = n())
threeGram2 <- threeGram2 %>% filter(n == max(n)) %>% arrange(desc(n), bigram, threegram, n) # please note: this can return more than one row per (n-i)-gram
threeGram2Relevant <- threeGram2 %>% filter((n>1) | (count > 1))

fourGram2 <- fourGram %>% group_by(threegram) %>% mutate(count = n())
fourGram2 <- fourGram2 %>% filter(n == max(n)) %>% arrange(desc(n), threegram, fourgram, n) # please note: this can return more than one row per (n-i)-gram
fourGram2Relevant <- fourGram2 %>% filter((n>1) | (count > 1))

# write to csv for later use for prediction
write.csv(oneGram2Relevant, file = "oneGram2Relevant.csv")
write.csv(biGram2Relevant, file = "biGram2Relevant.csv")
write.csv(threeGram2Relevant, file = "threeGram2Relevant.csv")
write.csv(fourGram2Relevant, file = "fourGram2Relevant.csv")