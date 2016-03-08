# Problem Set 5
## March 10
## Danielle Korman

# Code given:

rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("/Users/drk/Desktop/R/PS5/anes_timeseries_2012_stata12.dta")

## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score
model1 <- lm(ft_dpc ~ ft_hclinton, anes)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))
## we would expect a Obama score of 71.7


## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness

# Split data into test and train
dim(anes)
indexes <- sample(1:nrow(anes), size=0.5*nrow(anes))
test <- anes[indexes,]
dim(test) 
train <- anes[-indexes,]
dim(train)
