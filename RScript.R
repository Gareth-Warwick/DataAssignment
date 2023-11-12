#1.0 Linking to Git

##1.1Creating an EC349 Assignment 2.0 to link to GitHub

##1.2Commit Again2


#2.0 Setting up

##2.1 Setting Working Directory
setwd("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/EC349 Assignment 2.0")

##2.2 Download and Load Tidyverse Package
install.packages("tidyverse")
library(tidyverse)

##2.3 Set seed to control randomisation
set.seed(1)

##2.4 Pre-Processing Yelp Academic Data for the Assignment (Code prepared by Love and Nathan)

###2.4.1 Install and load "jsonlite" package
install.packages("jsonlite")
library(jsonlite)

###2.4.2 Clear
cat("\014")  
rm(list=ls())


#3.0 Load Different Data (Code prepared by Love and Nathan) --> #note that stream_in reads the json lines (as the files are json lines, not json)
business_data <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_business.json"))
review_data  <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_review.json")) 
checkin_data  <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_checkin.json")) 
user_data <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_user.json")) 
tip_data  <- stream_in(file("/Users/gare.mac/Desktop/Warwick/Y3/EC349/Summative Assignment/Assignment/yelp_academic_dataset_tip.json")) 


#4.0 Create a test dataset containing 10,000 randomly drawn observations -- using the “caret” package in R (or the function “sample()”)

