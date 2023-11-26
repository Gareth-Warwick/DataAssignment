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





#2.0 DATA COLLECTION: Loading the data into computer

##2.1 Load the .RDA data for users and reviews (these are the smaller datasets as I couldn't load the big ones)

###2.1.1 Load the dataset "review_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_review_small.Rda")


##2.3 View all the data (note the capital letter for "View" command)
View(review_data_small)


##2.4 Remove duplicates and empty text
review_data_clean <- review_data_small %>%
  filter(!duplicated(text) & text != "")

##2.5 Clear Memory
rm(review_data_small)


#3.0 DATA UNDERSTANDING: View summary of star ratings 
install.packages("janitor")
library(janitor)
tabyl(review_data_clean$stars, sort=TRUE)
#Output: We observe that there is skewed result toward 1-star, 4-star, and 5-star --> (1star: 15.28% | 2star: 7.82% | 3star: 9.92% | 4 star: 20.78% | 5star: 46.21%)

summary(review_data_clean)




#4.0 DATA PREPARATION: Get a random sample from the population data (half of the population data which is around 699,028) --> due to machine constraints where the machine is unable to process too large datasets
randomsample <- sample(1:nrow(review_data_clean), 1*nrow(review_data_clean)/2)
review_data_small2 <- review_data_clean[randomsample,]


#4.1 Clear Memory
rm(randomsample)
rm(review_data_clean)






#5.0 DATA PREPARATION: Filter the data

##5.1 In review_data_small --> only extract relevant columns (1:review_id // 4:stars // 8:text)
review_data_small3 <- subset(review_data_small2, select = c(1,4,8) )
View(review_data_small3)

##5.2 Clearing Memory
rm(review_data_small2)






#6.0 DATA PREPARATION: clean the text
review_data_small3 <- review_data_small3 |>
  dplyr::mutate(
    clean_text = tolower(text), #converts all text in the 'text' column to lowercase --> eliminate case sensitivity and ensure consistent representation of words
    clean_text = gsub("[']", "", clean_text), #remove all single quote characters (') from the text, ensuring that they don't interfere with subsequent processing
    clean_text = gsub("[[:punct:]]", " ", clean_text), #eliminate all punctuation marks, such as commas, periods, question marks etc, leaving only whitespace-separated words
    clean_text = gsub("[[:space:]]+", " ", clean_text), #replaces multiple consecutive whitespace characters (spaces/tabs/newlines) with single spaces, ensuring consistent spacing between words
    clean_text = gsub("[[:digit:]]", "", clean_text), #removes digits
    clean_text = trimws(clean_text)) #trims any leading or trailing whitespace from the beginning and end of each word, ensuring that words are represented in a consistent manner

##6.1 Remove duplicate rows with duplicates in clean_text (again)
review_data_small3clean <- review_data_small3 %>%
  filter(!duplicated(clean_text) & clean_text != "")

##6.2 Clear memory
rm(review_data_small3)

##6.3 Extract the relevant columns
review_data_small4 <- subset(review_data_small3clean, select = c(1,2,4) ) #extract out column 1 (review_id), column 2 (stars) and column 4 (clean_text)
View(review_data_small4)

##6.4 Clear memory
rm(review_data_small3clean)









#7.0 DATA PREPARATION: SPLIT INTO TRAINING AND TEST
train <- sample(1:nrow(review_data_small4), 3*nrow(review_data_small4)/4) #split 3/4 and 1/4
review_train <- review_data_small4[train,] #Training Data
review_test<- review_data_small4[-train,] #Test Data

rm(train)
rm(review_data_small4)







#8.0 DATA UNDERSTANDING: Visualise the data
install.packages("tm")
library(tm)
install.packages("tidytext")
library(tidytext)



##8.1 TRAINING DATA -- Visualise


###8.1.1 TOKENIZE THE TEXT
#Consider filtering out redundant words
tokenized_review_train <- review_train %>%
  unnest_tokens(input = clean_text, output = word)

View(tokenized_review_train)


###8.1.2 Remove stopwords
install.packages("stopwords")
library(stopwords)
cleaned_tokenized_train <- tokenized_review_train %>%
  anti_join(get_stopwords())


###8.1.3 Clear memory
rm(tokenized_review_train)


###8.1.3 Count and sort most common words
cleaned_tokenized_train %>%
  count(word, sort = TRUE) 
  
###8.1.4 Count the occurrences of words with specific stars, sort based on frequency
train_stars_text_count <- cleaned_tokenized_train %>%
  count(word, stars, sort = TRUE)

###8.1.5 Visualise
train_stars_text_count %>%
  filter(n > 0.5*length(cleaned_tokenized_train)) %>%
  mutate(word = reorder(word, n)) %>%
  slice_max(order_by=n,n=100) %>% #Select the top 100 words 
  ggplot(aes(word, 0.5*length(cleaned_tokenized_train), fill = stars)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to stars rating")
#Finding: Based on the top 100 words that impact star ratings (ie frequency of word used), we expect these words to be useful indicators of a user's eventual star rating
  #--> In terms of tangible business attributes, "food", "service", "staff", "experience", friendly", "restaurant", "delicious", "fresh" are within the top 100 words that have the largest contribution to star rating (due to their highest usage)




##9.0 DATA PREPARATION: [Training Data] Create a Document Term Matrix (go back to "review_train", but remember to remove stop words here)

##9.1 Install required packages
install.packages("quanteda")
library(quanteda)

##9.2 Create a corpus
corpus_review_train <- corpus(review_train$clean_text, docnames = review_train$review_id)
summary(corpus_review_train)
  
##9.3 Token
token_review_train <- tokens(corpus_review_train)

##9.4 Create a Document Term Matrix (this will take a pretty long time)
DTM_review_train <- dfm(token_review_train) %>%
  dfm_remove(c(stopwords(source="stopwords-iso"))) #remove stopwords from the "stopwords-iso lexicon

##9.5 Remove stopwords
Clean_DTM_review_train <- dfm_remove(DTM_review_train)


##9.6 Trim DFM
Trim_DTM_review_train <- dfm_trim(Clean_DTM_review_train, min_docfreq = 0.05, docfreq_type = "prop") #remove features that appear in less than 5% of the reviews

View(Trim_DTM_review_train) #View entire DTM
Trim_DTM_review_train #view some features


##9.7 Clear Memory
rm(corpus_review_train)
rm(token_review_train)
rm(DTM_review_train)
rm(Clean_DTM_review_train)






#10.0 DATA PREPARATION: [Training Data] Create matrix 

##10.1 Convert DFM into a matrix containing only predictors (ie features and words)
Matrix_DTM_review_train <- as.matrix(Trim_DTM_review_train) #IT WORKSSSSSSSSS

##10.2 Acquire vector containing output (stars) from original training dataset "review_train"
stars_review_train <- subset(review_train, select = c(2))

##10.3 Join output with predictors
Matrix_review_train <- cbind(stars_review_train,Matrix_DTM_review_train)

##10.4 Clear Memory
rm(Matrix_DTM_review_train)
rm(stars_review_train)







#11.0 MODELLING 1: [Training Data] Unregularised Linear Regression

##11.1 Construct Linear Regression of "stars" against "features"
lm_unreg_train_stars <- lm(stars~ . , data = Matrix_review_train) #Create the linear regression

##11.2 Review results 
summary(lm_unreg_train_stars) 






#12.0 DATA PREPARATION: [Test Data] Create a Document Term Matrix (go back to "review_test", but remember to remove stop words here)


##12.1 Create a corpus
corpus_review_test <- corpus(review_test$clean_text, docnames = review_test$review_id)
summary(corpus_review_test)

##12.2 Token
token_review_test <- tokens(corpus_review_test)

##12.3 Create a Document Term Matrix (this will take a pretty long time)
DTM_review_test <- dfm(token_review_test) %>%
  dfm_remove(c(stopwords(source="stopwords-iso"))) #remove stopwords from the "stopwords-iso lexicon

##12.4 Remove stopwords
Clean_DTM_review_test <- dfm_remove(DTM_review_test)


##12.5 Trim DFM such that it has the same features as training DFM (ref step 9.6)
Trim_DTM_review_test <- dfm_match(Clean_DTM_review_test, featnames(Trim_DTM_review_train))


View(Trim_DTM_review_test) #View entire DTM
Trim_DTM_review_test #view some features


##12.6 Clear Memory
rm(corpus_review_test)
rm(token_review_test)
rm(DTM_review_test)
rm(Clean_DTM_review_test)






#13.0 DATA PREPARATION: [Test Data] Create matrix 

##13.1 Convert DFM into a matrix containing only predictors (ie features and words)
Matrix_DTM_review_test <- as.matrix(Trim_DTM_review_test) #IT WORKSSSSSSSSS

##13.2 Acquire vector containing output (stars) from original test dataset "review_test"
stars_review_test <- subset(review_test, select = c(2))

##13.3 Join output with predictors
Matrix_review_test <- cbind(stars_review_test,Matrix_DTM_review_test)

##13.4 Clear Memory
rm(Matrix_DTM_review_test)
rm(stars_review_test)
rm(Trim_DTM_review_test)









### Plot a unregularised linear regression



#Try sort by count word WITHIN 5 stars?

###8.1.4 do something about link word with stars then sort then plot? (https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html & https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html)














#SCRAPPED - This regression too big
UnRegLR <- lm(stars~word, data=tokenized_review_train)


#SCRAPPED - Check unique id
unique_review_ids_before <- unique(review_x_train$review_id)
unique_review_ids_after <- unique(tokenized_review_train$review_id)
setdiff(unique_review_ids_after, unique_review_ids_before)


#SCRAPPED - start crying
rm(unique_review_ids_before)
rm(unique_review_ids_after)


## SCRAPPEDCheck whether review_id are unique

review_ids <- gsub(".*(\\d+).*", "\\1", review_x_train$clean_text)

# SCRAPPED Count identifier occurrences
identifier_counts <- as.data.frame(table(review_ids))

# SCRAPPED Identify duplicate identifiers
duplicate_ids <- identifier_counts[identifier_counts$Freq > 1,]$review_id

# SCRAPPED Investigate duplicate identifiers
for (id in duplicate_ids) {
  duplicate_reviews <- review_x_train[review_x_train$review_id == id, ]
  print(paste("Review ID:", id))
  print(paste("Review Text:", duplicate_reviews$clean_text))
  # Manually examine the review text to determine if it represents a single review or multiple reviews with the same identifier
}

# SCRAPPED Validate identifier format
invalid_ids <- review_x_train[grepl("[^\\d]", review_ids), ]$review_id
# SCRAPPED Identify any identifiers that deviate from the expected format or appear nonsensical

# SCRAPPED Correct or remove invalid identifiers
# SCRAPPED Based on your investigation, correct any invalid identifiers or remove them from the data if they cannot be corrected reliably

# SCRAPPED Verify uniqueness
unique_ids <- length(unique(review_ids))
if (unique_ids != nrow(review_x_train)) {
  print("Review identifiers are not unique!")
}



### SCRAPPED 8.1.2 Calculate TF-IDF (Term frequency and inverse document frequency)
tfidf_review_train <- tokenized_review_train %>%
  bind_tf_idf(term=word, document=review_id, n=55016803) 


#Error: Error in tapply(n, documents, sum) : arguments must have same length
### Consider: Split tokenized_review_train --> review_id + stars // review_id + word --> TF-IDF the review_id + word --> join with review_id+stars by=review_id (ie join based on similarity in review_id))
#Above doesn't seem to work


###SCRAPPED 8.1.1 CONVERT TRAINING DATA TO BOW DATA

bowdata_x_train <- review_x_train %>%
  mutate(text = as.character(clean_text)) %>%
  mutate(tokens = token_ngrams(clean_text, n = 1)) %>%
  unnest(tokens) %>%
  count(review_id, name = "token_count")




###SCRAPPED 8.1.1 CONVERT TRAINING DATA TO DTM
DTM_review_train <- DocumentTermMatrix(Corpus(VectorSource(review_x_train$clean_text)), control = list(
  verbose=TRUE, #enables verbose output, providing detailed information about the preprocessing steps
  stem=TRUE, #convert words to their root form
  stopwords=TRUE, #removes common stopwords (ie words considered to be less informative for text analysis)
  asPlain=TRUE, #convert input text into plain text, removing html tags and other formatting)
  tolower=TRUE, #converts all tags into lower case
  removePunctuation=TRUE, #remove punctuations
  removeSeparators=TRUE, #remove whitespace separators (eg.tabs and newline characters) --> leave only single spaces between words
  stripWhitespace=TRUE, #removes leading and trailing whitespace from each word
  minWordLength=1, #includes words of any length in the vocabulary, including single-letter words
  bounds=list(global=c(0.1*length(review_train),Inf)))) #specifies that only words appearing in at least 10% of the time will be included, with no upper bound (Inf) on document frequency




tidy_review_train <- tidy(DTM_review_train)
merged_tibble_train <- mutate(tidy_review_train, stars = review_y_train)


matrix_review_train <- as.matrix(DTM_review_train)
  


tibble_review_train <- cbind(DTM_review_train, review_y_train) #merge stars with predictor
rm(merged_train)


###8.1.2 CONVERT TRAINING DTM TO TIBBLE
tibble_review_train <- tidy(merged_train)
rm(tibble_review_train)

###8.1.3 CLEAR MEMORY
rm(review_train)
rm(DTM_review_train)


##8.2 TEST DATA

###8.2.1 CONVERT TEST DATA TO DTM
DTM_review_test <- DocumentTermMatrix(Corpus(VectorSource(review_test)), control = list(
  verbose=TRUE, #enables verbose output, providing detailed information about the preprocessing steps
  stem=TRUE, #convert words to their root form
  stopwords=TRUE, #removes common stopwords (ie words considered to be less informative for text analysis)
  asPlain=TRUE, #convert input text into plain text, removing html tags and other formatting)
  tolower=TRUE, #converts all tags into lower case
  removePunctuation=TRUE, #remove punctuation
  removeSeparators=TRUE, #remove whitespace separators (eg.tabs and newline characters) --> leave only single spaces between words
  stripWhitespace=TRUE, #removes leading and trailing whitespace from each word
  minWordLength=1, #includes words of any length in the vocabulary, including single-letter words
  bounds=list(global=c(0.2*length(review_train),Inf)))) #specifies that only words appearing in at least 20% of the time will be included, with no upper bound (Inf) on document frequency


###8.2.2 CONVERT TEST DTM TO TIBBLE
tibble_review_test <- tidy(DTM_review_test)


###8.2.3 CLEAR MEMORY
rm(review_test)
rm(DTM_review_test)



#8.0 SCRAP I DONT NEED THIS - DATA PREPARATION: SPLIT DATA INTO PREDICTORS (text) AND OUTPUT (stars)


##8.1 TRAINING DATA
tibble_x_train <- tibble_review_train[ ,c(2,3)] #Predictors (text in column 2, frequency of repeat words in column 3)
tibble_y_train <- tibble_review_train [ ,1] #Output (stars)


##8.2 TEST DATA
tibble_x_test <- tibble_review_test[ ,c(2,3)] #Predictors (text in column 2, frequency of repeat words in column 3)
tibble_y_test <- tibble_review_test [ ,1] #Output (stars)





#9.0 MODELLING 1: Unregularised Linear Regression from training data
UnregLinRegr_train <- lm (stars~ ., data=merged_train)





#10.0 EVALUATION 1: Calculating mean squared error

##10.1 Predict target vfalues on the test set
Predictions1 <- predict(Unreg_LinRegr, newdata=tibble_review_test)

##10.2 Calculate Mean Squared Error (MSE)
MSE1 <- mean((Predictions1-tibble_review_test$stars)^2)

##10.3 Calculate R-Squared
Rsquared1 <- summary(Unreg_LinRegr)$rsq

##10.4 Print performance metrics
print(paste("MSE:", MSE1))
print(paste(2R-squared:", Rsquared1"))












###5.1.1 Predictor variables in training data 
review_x_train <- tibble_review[,2] #Create data frame containing only the predictor variables (clean_text) which is in column 2 in the UPDATED "review_data_small2"

###5.1.2 Output variable (stars) in training data
review_y_train <- tibble_review[,1] #Create vector containing ONLY the output (ie stars) which is in column 1 in "review_data_small2"



##5.2 Create the TEST data (ie the data not in the training data)
review_test<- tibble_review[-train,]

###5.2.1 Predictor variables in test data 
review_x_test<- tibble_review[,2]

###5.2.2 Output variable (stars) in test data
review_y_test <- tibble_review[,1]

##5.3 Clear Memory
rm(review_test)
rm(review_train)
rm(train)
rm(review_data_small3)





#6.0 SCRAPPED Create matrix of words in TRAINING data


##6.1 Create function to count frequency of words appearing in training data predictor values
base_loop <- function(clean_text){
  tokns_train <- strsplit(review_x_train, " ", fixed=TRUE)
  vocab_train <- sort(unique(unlist(tokns_train)))
  filtered_vocab_train <- vocab_train[which(table(tokns_train))>=122330]
  dtm <- matrix(data = 0L, 
                ncol = length(filtered_vocab_train), nrow = length(tokns_train),
                dimnames = list(filtered_vocab_train) )
  freqs <- lapply(tokns_train, table)
  for (i in 1:length(freqs) ){
    doc <- freqs[[i]]
    dtm[i, names(doc)] <- as.integer(doc)
  }
  return(dtm)}

##6.2 scrapped Apply function on training data predictor values
dtm_train <- base_loop(review_x_train)


rm(base_loop)



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
  bounds=list(global=c(34951,Inf)))) #specifies that only words appearing in at least 12233 documents (ie repeated at least 12233 times, which is 1% of the training data) will be included, with no upper bound (Inf) on document frequency


##RETRY but now keep the sparse matrix

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


X_train <- tidy(DTM_review_x_train)

regr <- lm(stars~ . ,data = as.data.frame(cbind(text=X_train, stars=review_y_train)))



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
  bounds=list(global=c(34951,Inf)))) #specifies that only words appearing in at least one document (lower bound) will be included, with no upper bound (Inf) on document frequency


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

