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

#2.0 Loading the data

##2.1 Load the .RDA data for users and reviews (these are the smaller datasets as I couldn't load the big ones)

###2.1.1 Load the dataset "review_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_review_small.Rda")

###2.1.2 Load the dataset "user_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_user_small.Rda")

##2.2 Load the .json data for business information "business_data"
install.packages("jsonlite")
library(jsonlite)
business_data <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_business.json"))

##2.3 View all the data (note the capital letter for "View" command)
View(review_data_small)
View(user_data_small)
View(business_data)

#3.0 View summary of star ratings 
install.packes("janitor")
library(janitor)
tabyl(review_data_small$stars, sort=TRUE)
#Output: We observe that there is skewed result toward 1-star, 4-star, and 5-star --> (1star: 15.3% | 2star: 7.82% | 3star: 9.91% | 4 star: 20.77% | 5star: 46.22%)

