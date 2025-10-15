### Today we are going to learn how to work with words and {stringr} ####
### Created by: Ally Malilay #############
### Updated on: 2025-10-14 ####################
#######################################
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)

words<-"This is a string"
words
words_vector<-c("Apples", "Bananas","Oranges")
words_vector

paste("High Temp", "Low pH")
paste("High temp", "Low pH", sep = "-")
paste0("High temp", "Low pH")

#Working with vectors
shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best","worst")
paste("It was the", two_cities, "of times") #can try using mutate to create a new column doing this

#Want to know how long a string is
shapes
str_length(shapes)



seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA


#modify strings
str_sub(seq_data, start = 3, end = 3) <- "A" # add an A in the 3rd position
seq_data
str_dup(seq_data, times = c(2, 3)) # times is the number of times to duplicate each string

### Whitespace
#If there are columns that need to all be put together, remove the whitespace from any parts of the dataset
badtreatments <- c(" High", "High", "High ", "Low", "Low")
badtreatments
str_trim(badtreatments) #removes whitespace from both sides
str_trim(badtreatments, side = "left") #remove whitespace from left side
str_pad(badtreatments, 5, side = "right") #add a white space to the right side
str_pad(badtreatments, 5, side = "right", pad = "1") #add a 1 to the right side

#Locale sensitive 
x <- "I love R and you are great!"
str_to_upper(x) #make everything upper case
str_to_lower(x) #make everything lower case
str_to_title(x) #cap first letter of each word

data <- c("AAA", "TATA", "CTAG", "GCTT")
#find all the strings with an A
str_view(data, pattern = "A") 
str_detect(data, pattern = "A") #detect a specific pattern
str_detect(data, pattern = "AT") #detect a specific pattern
str_locate(data, pattern = "AT") #locate a pattern

vals <- c("a.b","b.c","c.d")
str_replace(vals, "\\.", " ") #replace the . with a space
vals <- c("a.b.c","b.c.d","c.d.e")
#string, pattern, replace
str_replace(vals, "\\.", " ")
str_replace_all(vals, "\\.", " ")


#subset the vector to only keep strings with digits for sequences
val2 <- c("test 123", "test 456", "test")
str_subset(val2, "\\d")


#### Activity
strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
# Which strings contain phone numbers?
str_detect(strings, phone)
# subset only the strings with phone numbers
test<-str_subset(strings, phone)
test
testing <- str_replace_all(test, "\\.", "-" ) %>%
  str_replace_all("[a-z]|\\:", "") %>%
  str_trim()
testing

#Nyssa solution
test %>%
  str_replace_all(pattern = "\\.", replacement = "-") %>% # replace periods with -
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") %>% # remove all the things we don't want
  str_trim() # trim the white space

#tidy text
# explore it
head(austen_books())
tail(austen_books())

original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # count the chapters (starts with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() # ungroup it so we have a dataframe again
# don't try to view the entire thing... its >73000 lines...
head(original_books)

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column
head(tidy_books) # there are now >725,000 rows. Don't view the entire thing!

head(get_stopwords())

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords
head(cleaned_books)

#example: find out how many times the word 'coral' pops up, count words
cleaned_books %>%
  count(word, sort = TRUE)

#sentiment analysis, show us how many positive and negative words are used
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them
head(sent_word_counts)[1:3,]

sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it goes from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words
