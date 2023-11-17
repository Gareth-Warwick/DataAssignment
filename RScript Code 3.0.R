#1.0 Linking to Git

##1.1Creating an EC349 Assignment 2.0 to link to GitHub

##1.2Commit Again2


#2.0 Setting up

##2.1 Setting Working Directory
setwd("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/EC349 Assignment 3.0")

##2.2 Download and Load Tidyverse Package
install.packages("tidyverse") 
library(tidyverse)

##2.3 Set seed to control randomisation
set.seed(1)

##2.4 Loading the data

###2.4.1 Clear
cat("\014")  
rm(list=ls())

###2.4.2 Load the .RDA data (these are the smaller datasets as I couldn't load the big ones)

####2.4.2.1 Load & view the dataset "review_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_review_small.Rda")

####2.4.2.1 Load the dataset "user_data_small"
load(file="/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/Small Datasets/yelp_user_small.Rda")

###2.4.2 Load the .json data "business_data"
install.packages("jsonlite")
library(jsonlite)
business_data <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_business.json"))

###2.4.3 View the data
View(review_data_small)
View(user_data_small)
View(business_data)