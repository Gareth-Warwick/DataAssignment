#1.0 Setting up

##1.1 Setting Working Directory
setwd("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/EC349 Assignment 3.0")

##1.2 Download and Load Tidyverse Package
install.packages("tidyverse") 
library(tidyverse)

#1.3 Clear
cat("\014")  
rm(list=ls())


##1.4 Set seed to control randomisation
set.seed(1)





#2.0 DATA COLLECTION: Loading the data into computer

##2.1 Load the .RDA data for users and reviews (these are the smaller datasets as I couldn't load the big ones)

###2.1.1 Load the dataset "review_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_review_small.Rda")

#2.1.2 DATA PREPARATION: Get a random sample from the population data (half of the population data which is around 699,028) --> due to machine constraints where the machine is unable to process too large datasets
randomsample <- sample(1:nrow(review_data_small), 1*nrow(review_data_small)/2)
review_data_small2 <- review_data_small[randomsample,]

#2.1.3 Clear Memory
rm(randomsample)
rm(review_data_small)

##2.2 View all the data (note the capital letter for "View" command)
View(review_data_small2)
summary(review_data_small2)



#3.0 In review_data_small --> only extract relevant columns (1:review_id // 4:stars // 8:text)
review_data_small3 <- subset(review_data_small2, select = c(1,4,8) )
View(review_data_small3)


#4.0 Clean Data

##4.1 Remove duplicates and empty text
review_data_clean <- review_data_small3 %>%
  filter(!duplicated(text) & text != "")


##4.2 Clear Memory
rm(review_data_small2)
rm(review_data_small3)


#5.0 DATA UNDERSTANDING: View summary of star ratings 
install.packages("janitor")
library(janitor)
tabyl(review_data_clean$stars, sort=TRUE)
#Output: We observe that there is skewed result toward 1-star, 4-star, and 5-star --> (1star: 15.31% | 2star: 7.83% | 3star: 9.91% | 4 star: 20.83% | 5star: 46.12%)

summary(review_data_clean)












#6.0 DATA PREPARATION: clean the text
review_data_clean <- review_data_clean |>
  dplyr::mutate(
    clean_text = tolower(text), #converts all text in the 'text' column to lowercase --> eliminate case sensitivity and ensure consistent representation of words
    clean_text = gsub("[']", "", clean_text), #remove all single quote characters (') from the text, ensuring that they don't interfere with subsequent processing
    clean_text = gsub("[[:punct:]]", " ", clean_text), #eliminate all punctuation marks, such as commas, periods, question marks etc, leaving only whitespace-separated words
    clean_text = gsub("[[:space:]]+", " ", clean_text), #replaces multiple consecutive whitespace characters (spaces/tabs/newlines) with single spaces, ensuring consistent spacing between words
    clean_text = gsub("[[:digit:]]", "", clean_text), #removes digits
    clean_text = trimws(clean_text)) #trims any leading or trailing whitespace from the beginning and end of each word, ensuring that words are represented in a consistent manner


##6.1 Remove duplicate rows with duplicates in clean_text (again)
review_data_small3clean <- review_data_clean %>%
  filter(!duplicated(clean_text) & clean_text != "")

##6.2 Clear memory
rm(review_data_clean)

##6.3 Extract the relevant columns
review_data_small4 <- subset(review_data_small3clean, select = c(1,2,4) ) #extract out column 1 (review_id), column 2 (stars) and column 4 (clean_text)
View(review_data_small4)

##6.4 Clear memory
rm(review_data_small3clean)



#7.0 DATA UNDERSTANDING

##7.1 Install required packages
install.packages("tm")
library(tm)
install.packages("tidytext")
library(tidytext)

##7.2 TOKENIZE THE TEXT
tokenized_review <- review_data_small4 %>%
  unnest_tokens(input = clean_text, output = word)

View(tokenized_review) #View the tokens

##7.3 Remove stopwords
install.packages("stopwords")
library(stopwords)
cleaned_tokenized_review <- tokenized_review %>%
  anti_join(get_stopwords()) %>%
  filter(!word %in% stopwords(source="stopwords-iso")) #remove stopwords, where stopwords are extracted from a pre-made lexicon called "stopwords-iso" 

##7.4 Clear memory
rm(tokenized_review)


##7.5 Count and sort most common words
cleaned_tokenized_review %>%
  count(word, sort = TRUE) 


##7.6 Count the occurrences of words with specific stars, sort based on frequency
overall_stars_text_count <- cleaned_tokenized_review %>%
  count(word, stars, sort = TRUE)

View(overall_stars_text_count)

##7.7 Visualise
overall_stars_text_count %>%
  filter(n > 0.5*length(cleaned_tokenized_review)) %>%
  mutate(word = reorder(word, n)) %>%
  slice_max(order_by=n,n=100) %>% #Select the top 100 words 
  ggplot(aes(word, 0.5*length(cleaned_tokenized_review), fill = stars)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to stars rating")
#Finding: Based on the top 100 words that impact star ratings (ie frequency of word used), we expect these words to be useful indicators of a user's eventual star rating
#--> In terms of tangible business attributes, "food", "service", "staff", friendly", "delicious", "restaurant", "experience" are within the top 100 words that have the largest contribution to star rating (due to their highest usage)


#8.0 DATA PREPARATION -- Create a Document Feature Matrix (go back to "review_train_small4", but remember to remove stop words here)


##8.1 Install required packages
install.packages("quanteda")
library(quanteda)

##8.2 Create a corpus
corpus_review <- corpus(review_data_small4$clean_text, docnames = review_data_small4$review_id)

summary(corpus_review)

##8.3 Tokenise the text
token_review <- tokens(corpus_review)

##8.4 Create a Document Feature Matrix (this will take a pretty long time)
DFM_review <- dfm(token_review) %>%
  dfm_remove(c(stopwords(source="stopwords-iso"))) #remove stopwords from the "stopwords-iso lexicon

##8.5 Trim DFM
Trim_DFM_review <- dfm_trim(DFM_review, min_docfreq = 0.03, docfreq_type = "prop") #remove features that appear in less than 5% of the reviews

View(Trim_DFM_review) #View entire DFM
Trim_DFM_review #view some features


##8.6 Clear Memory
rm(corpus_review)
rm(token_review)
rm(DFM_review)
#Keep Trim_DFM_review for now



#9.0 DATA PREPARATION -- Create Matrix 

##9.1 Convert DFM into a matrix containing only predictors (ie features and words)
Matrix_DFM_review <- as.matrix(Trim_DFM_review) #IT WORKSSSSSSSSS
#Output: A 698808 x 58 matrix

##9.2 Clean Matrix by deleting the column containing the predictor variable "stars" as this is unlikely to give significant information, and instead complicates things due to it bearing the same name as output variable
Matrix_DFM_review2 <- subset(Matrix_DFM_review, select = -stars)
View(Matrix_DFM_review2)
#Output: A 698808 x 57 matrix

##9.3 Acquire vector containing output (stars) from original training dataset "review_data_small4"
stars_review <- subset(review_data_small4, select = c(2))

##9.4 Join output with predictors
Matrix_review <- cbind(stars_review,Matrix_DFM_review2)
View(Matrix_review)
#Output: A 698808 x 58 matrix


##9.5 Clear Memory
rm(Matrix_DFM_review)
rm(Matrix_DFM_review2)
rm(stars_review)
rm(Trim_DFM_review)







#10.0 DATA PREPARATION: SPLIT INTO TRAINING AND TEST
train <- sample(1:nrow(Matrix_review), 3*nrow(Matrix_review)/4) #split 3/4 and 1/4
review_train <- Matrix_review[train,] #Training Data
review_test<- Matrix_review[-train,] #Test Data

##10.1 Clear Memory
rm(train)
rm(review_data_small4)
rm(Matrix_review)

##10.2 Within TRAINING data, split into predictors and output (stars)
review_train_predictors <- review_train[,-1]
review_train_stars <- review_train[,1]

##10.3 Within TEST data, split into predictors and output (stars)
review_test_predictors <- review_test[,-1]
review_test_stars <- review_test[,1]




#11.0 MODELLING 1: [TRAINING DATA] Ordinary Least Squares (OLS) Linear Regression

##11.1 Construct linear regression of "stars" against "features"
linreg_unreg_train <- lm(stars~ ., data=review_train) #create the linear regression

##11.2 Review results and coefficients
summary(linreg_unreg_train)
#Adjusted R-squared: 0.2687




#12.0 MODEL EVALUATION 1 [TEST DATA] Unregularised Linear Regression

##12.1 Fit model onto test data (exclude first column which is the outcome) to generate predicted value
linreg_unreg_predict <- predict(linreg_unreg_train, newdata=review_test[,-1]) 

##12.2 Calculate empirical Mean Squared Error in the TEST data
linreg_unreg_test_MSE <- mean((linreg_unreg_predict-review_test$stars)^2)
#Finding: Mean Squared Error = 1.58765071924771 --> greater than 1 --> too large --> there is a problem with this model --> proceed to new model




#13.0 MODELLING 2: [TRAINING DATA] Shrinkage Methods -- Ridge Linear Regression

##13.1 Install required packages
install.packages("glmnet")
library(glmnet)

##13.2 Conduct cross validation to find lambda that minimises empirical Mean Squared Error in training data
cv.out.ridge <- cv.glmnet(as.matrix(review_train_predictors), as.matrix(review_train_stars), alpha=0, nfolds=10)
plot(cv.out.ridge)
lambda_ridge_cv <- cv.out.ridge$lambda.min #choose lambda that minimises empirial MSE in training data set

##13.3 Estimate Ridge with lambda chosen by Cross Validation
ridge.mod <- glmnet(review_train_predictors, review_train_stars, alpha=0, lambda=lambda_ridge_cv, thresh=1e-12)

summary(ridge.mod)






#14.0 MODEL EVALUATION 2: [TEST DATA] Shrinkage Methods -- Ridge Linear Regression

##14.1 Fit on test data
ridge.pred <- predict(ridge.mod, s=lambda_ridge_cv, newx=as.matrix(review_test_predictors))
ridge_MSE <- mean((ridge.pred-review_test_stars)^2)
#Finding: Mean Squared Error = 1.58792508089288 --> greater than 1 --> too large --> there is a problem with this model





#15.0 MODELLING 3: [TRAINING DATA] Shrinkage Methods -- LASSO Linear Regression

#15.1 Conduct cross validation to find lambda that minimises empirical Mean Squared Error in training data
cv.out.LASSO <- cv.glmnet(as.matrix(review_train_predictors), as.matrix(review_train_stars), alpha=1, nfolds=10)
plot(cv.out.LASSO)
lambda_LASSO_cv <- cv.out.LASSO$lambda.min #choose lambda that minimises empirial MSE in training data set

##15.2 Estimate LASSO with lambda chosen by Cross Validation
LASSO.mod <- glmnet(review_train_predictors, review_train_stars, alpha=1, lambda=lambda_LASSO_cv, thresh=1e-12)

summary(LASSO.mod)







#16.0 MODEL EVALUATION 3: [TEST DATA] Shrinkage Methods -- LASSO Linear Regression

##16.1 Fit on test data
LASSO.pred <- predict(LASSO.mod, s=lambda_LASSO_cv, newx=as.matrix(review_test_predictors))
LASSO_MSE <- mean((LASSO.pred-review_test_stars)^2)
#Finding: Mean Squared Error = 1.60202550357658 --> greater than 1 --> too large --> there is a problem with this model
#Comparing MSE: OLS MSE=1.60205091245437 > LASSO MSE=1.60202550357658 > Ridge MSE=1.60188964913095 --> therefore ridge reduces variance to the greatest extent






#17.0 MODELLING 4: [TRAINING DATA] Regression Decision Tree 

##17.1 Install required packages
install.packages("tidymodels")
library(tidymodels)
install.packages("tree")
library(tree)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


##17.2 Construct tree using rpart package
rpart_tree <- rpart(stars ~ ., data=review_train)

##17.3 Construct graph
rpart.plot(rpart_tree, cex=0.6)


#18.0 MODEL EVALUATION 4: [TEST DATA] Regression Decision Tree

##18.1 Make predictions and add to the test data
regression_tree_prediction <- predict(rpart_tree,new_data=review_test) %>% 
  cbind(review_test)
View(regression_tree_prediction)

##18.2 Evaluating Performance

###18.2.1 Evaluating using mae (Mean Absolute Error)
regression_tree_performance_mae <- mae(regression_tree_prediction, estimate = ".", truth=stars)

View(regression_tree_performance_mae)
#Finding: estimate = 1.355557

###18.2.2 Evaluating using rmse (Root Mean Squared Error)
regression_tree_performance_rmse <- rmse(regression_tree_prediction, estimate = ".", truth=stars)

View(regression_tree_performance_rmse)
#Finding: estimate = 1.633423


