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

##2.3 View all the data (note the capital letter for "View" command)
View(review_data_small)




#3.0 DATA UNDERSTANDING: View summary of star ratings 
install.packages("janitor")
library(janitor)
tabyl(review_data_small$stars, sort=TRUE)
#Output: We observe that there is skewed result toward 1-star, 4-star, and 5-star --> (1star: 15.3% | 2star: 7.82% | 3star: 9.91% | 4 star: 20.77% | 5star: 46.22%)





#4.0 DATA PREPARATION: Filter the data

##4.1 In review_data_small --> only extract relevant columns (4-stars // 8-text)
review_data_small2 = subset(review_data_small, select = c(4,8) )
View(review_data_small2)


##4.2 Clearing Memory
rm(review_data_small)






#5.0 DATA PREPARATION: clean the text
review_data_small2 <- review_data_small2 |>
  dplyr::mutate(
    clean_text = tolower(text), #converts all text in the 'text' column to lowercase --> eliminate case sensitivity and ensure consistent representation of words
    clean_text = gsub("[']", "", clean_text),  #remove all single quote characters (') from the text, ensuring that they don't interfere with subsequent processing
    clean_text = gsub("[[:punct:]]", " ", clean_text), #eliminate all punctuation marks, such as commas, periods, question marks etc, leaving only whitespace-separated words
    clean_text = gsub("[[:space:]]+", " ", clean_text), #replaces multiple consecutive whitespace characters (spaces/tabs/newlines) with single spaces, ensuring consistent spacing between words
    clean_text = trimws(clean_text)) #trims any leading or trailing whitespace from the beginning and end of each word, ensuring that words are represented in a consistent manner




#5.0 DATA PREPARATION: Split the data "review_data_small2"

##5.1 Create the TRAINING data

train <- sample(1:nrow(review_data_small2), 7*nrow(review_data_small2)/8) #split 7/8 and 1/8
review_train <- review_data_small2[train,]

###5.1.1 Predictor variables in training data 
review_x_train <- review_train[,3] #Create data frame containing only the predictor variables (clean_text) which is in column 3 in the UPDATED "review_data_small2"

###5.1.2 Output variable (stars) in training data
review_y_train <- review_train[,1] #Create vector containing ONLY the output (ie stars) which is in column 4 in "review_data_small2"



##5.2 Create the TEST data (ie the data not in the training data)
review_test<- review_data_small2[-train,]

###5.2.1 Predictor variables in test data 
review_x_test<- review_test[,3]

###5.2.2 Output variable (stars) in test data
review_y_test <- review_test[,1]

##5.3 Clear Memory
rm(review_test)
rm(review_train)
rm(train)
rm(review_data_small2)


#6.0 DATA PREPARATION: Create tokens from the words appearing in the input corpus --> use the tokens to construct a model
install.packages("tm")
library(tm)

##6.1 Creating tokens within TRAINING data

DTM_review_x_train <- DocumentTermMatrix(Corpus(VectorSource(review_x_train)), control = list(
  verbose=TRUE, #enables verbose output, providing detailed information about the preprocessing steps
  stem=TRUE, #convert words to their root form
  stopwords=TRUE, #removes common stopwords (ie words considered to be less informative for text analysis)
  asPlain=TRUE, #convert input text into plain text, removing html tags and other formatting)
  tolower=TRUE, #converts all tags into lower case
  removePunctuation=TRUE, #remove punctuation
  removeSeparators=TRUE, #remove whitespace separators (eg.tabs and newline characters) --> leave only single spaces between words
  stripWhitespace=TRUE, #removes leading and trailing whitespace from each word
  minWordLength=1, #includes words of any length in the vocabulary, including single-letter words
  bounds=list(global=c(0.1*length(review_x_train),Inf)))) #specifies that only words appearing in at least 12233 documents (ie repeated at least 12233 times, which is 1% of the training data) will be included, with no upper bound (Inf) on document frequency

install.packages("tidytext")
library(tidytext)
X_train <- tidy(DTM_review_x_train)

X_train <- as.matrix(DTM_review_x_train)

regr <- lm(stars~ . ,data = as.data.frame(cbind(text=X_train, stars=review_y_train)))
