#1.0 Setting up

##1.1 Setting Working Directory
setwd("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/EC349 Assignment 3.0")

##1.2 Download and Load Tidyverse Package
install.packages("tidyverse") 
library(tidyverse)

##1.3 Set seed to control randomisation
set.seed(1)

#1.4 Clear
cat("\014")  
rm(list=ls())





#2.0 DATA COLLECTION: Loading the data

##2.1 Load the .RDA data for users and reviews (these are the smaller datasets as I couldn't load the big ones)

###2.1.1 Load the dataset "review_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_review_small.Rda")

##2.2 View all the data (note the capital letter for "View" command)
View(review_data_small)





#3.0 DATA UNDERSTANDING: View summary of star ratings 
install.packages("janitor")
library(janitor)
tabyl(review_data_small$stars, sort=TRUE)
#Output: We observe that there is skewed result toward 1-star, 4-star, and 5-star --> (1star: 15.3% | 2star: 7.82% | 3star: 9.91% | 4 star: 20.77% | 5star: 46.22%)





#4.0 DATA PREPARATION: Clean the data

##4.1 In review_data_small --> only extract relevant columns (4th column is "stars", 8th column is "text")
review_data_small2 = subset(review_data_small, select = c(4,8) )
View(review_data_small2)



##4.2 Clearing Memory
rm(review_data_small)




#5.0 DATA PREPARATION: Split the data "review_data_small2"

##5.1 Create the TRAINING data

train <- sample(1:nrow(review_data_small2), 7*nrow(review_data_small2)/8) #split 7/8 and 1/8
review_train <- review_data_small2[train,]

###5.1.1 Predictor variables in training data 
review_x_train <- review_train[,5] #Create data frame containing only the predictor variables (text) which is in column 5 in "review_data_small2"

###5.1.2 Output variable (stars) in training data
review_y_train <- review_train[,4] #Create vector containing ONLY the output (ie stars) which is in column 4 in "review_data_small2"



##5.2 Create the TEST data (ie the data not in the training data)
review_test<- review_data_small2[-train,]

###5.2.1 Predictor variables in test data 
review_x_test<- review_test[,5]

###5.2.2 Output variable (stars) in test data
review_y_test <- review_test[,4]

##5.3 Clear Memory
rm(review_test)
rm(review_train)
rm(train)
rm(review_data_small2)



#6.0 DATA PREPARATION: Create tokens from the words appearing in the input corpus --> use the tokens to construct a model
install.packages("tm")
library(tm)

##6.1 Creating tokens within TRAINING data

###6.1.1 SCRAPPED Provides the corpus of text data from which the DTM will be constructed -- The corpus is typically represented as a collection of documents, each containing text data
reviewCorpus <- Corpus(VectorSource(review_x_train$text))

###6.1.2 EDITED TO COMBINE CORPUS AND MATRIX CONVERSION - Generates a document text matrix from the provided corpus
DTM_review_x_train <- as.matrix(DocumentTermMatrix(Corpus(VectorSource(review_x_train))), control = list(
  verbose=TRUE, #enables verbose output, providing detailed information about the preprocessing steps
  stem=TRUE, #convert words to their root form
  stopwords=TRUE, #removes common stopwords (ie words considered to be less informative for text analysis)
  asPlain=TRUE, #convert input text into plain text, removing html tags and other formatting)
  tolower=TRUE, #converts all tags into lower case
  removePunctuation=TRUE, #remove punctuation
  removeSeparators=TRUE, #remove whitespace separators (eg.tabs and newline characters) --> leave only single spaces between words
  stripWhitespace=TRUE, #removes leading and trailing whitespace from each word
  minWordLength=1, #includes words of any length in the vocabulary, including single-letter words
  bounds=list(global=c(122330,Inf)))) #specifies that only words appearing in at least 12233 documents (ie repeated at least 12233 times, which is 1% of the training data) will be included, with no upper bound (Inf) on document frequency

###6.1.3 SCRAPPED Convert DTM (a sparse matrix representation of the training data) into a dense matrix representation where all entries are explicitly stored
DTM_review_x_train <- as.matrix(vectorizer)





##6.2 Creating tokens within TEST data

###6.2.1 Provides the corpus of text data from which the DTM will be constructed -- The corpus is typically represented as a collection of documents, each containing text data
reviewCorpus_test <- Corpus(VectorSource(review_x_test$text))

###6.2.2 Generates a document text matrix from the provided corpus
vectorizer_test <- DocumentTermMatrix(reviewCorpus_test, control = list(
  verbose=TRUE, #enables verbose output, providing detailed information about the preprocessing steps
  stem=TRUE, #convert words to their root form
  stopwords=TRUE, #removes common stopwords (ie words considered to be less informative for text analysis)
  asPlain=TRUE, #convert input text into plain text, removing html tags and other formatting)
  tolower=TRUE, #converts all tags into lower case
  removePunctuation=TRUE, #remove punctuation
  removeSeparators=TRUE, #remove whitespace separators (eg.tabs and newline characters) --> leave only single spaces between words
  stripWhitespace=TRUE, #removes leading and trailing whitespace from each word
  minWordLength=1, #includes words of any length in the vocabulary, including single-letter words
  bounds=list(global=c(1,Inf)))) #specifies that only words appearing in at least one document (lower bound) will be included, with no upper bound (Inf) on document frequency


###6.2.3 Convert DTM (a sparse matrix representation of the training data) into a dense matrix representation where all entries are explicitly stored
DTM_review_x_test <- as.matrix(vectorizer_test)






#7.0 MODEL 1: Unregularised Linear Regression





#SCRAPPED -- 6.0 Get a general sense of the words associated with 5-star and 1-star --> using Wordcloud 
#NOTE: Consider scrapping this since not particularly useful 

##6.1 Install and load packages needed to create wordcloud
install.packages("tm")
install.packages("wordcloud2")
install.packages("wordcloud")
library(tm)
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)

##6.2 Create wordcloud for 5-star ratings
General5star <- subset(review_data_small2, stars == 5, select = text)
General5star_text <- paste(General5star, collapse = " ")
Worldcloud_5star <- wordcloud(words = strsplit(General5star_text, " ")[[1]], freq = rep(1, length(strsplit(General5star_text, " ")[[1]])),colors=c("chartreuse", "cornflowerblue", "darkorange"),max.words=100)
wordcloud2(Wordcloud_5star) # Display the word cloud 


##6.3 Create wordcloud for 1-star ratings
General1star <- subset(review_data_small2, stars == 1)
General1star_text <- paste(General1star$text, collapse = " ")
Worldcloud_1star <- wordcloud(words = strsplit(General1star_text, " ")[[1]], freq = rep(1, length(strsplit(General1star_text, " ")[[1]])),colors=c("chartreuse", "cornflowerblue", "darkorange"),max.words=100)
wordcloud2(Wordcloud_1star) # Display the word cloud 



