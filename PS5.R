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

# statistical models
# First Model: Regress Obama Thermometer on Gender
# Use variable gender_respondent_x for gender, as this has the gender for both
# FTF and web modes
# 1 is male and 2 is female
# There does not appear to be any missing observations
str(anes$gender_respondent_x)
length(anes$gender_respondent_x)
class(anes$gender_respondent_x)
unique(anes$gender_respondent_x)
# It is currently a factor, so it must be made numeric
gender_num <- as.numeric(train$gender_respondent_x)
# Regress against Obama Thermometer variable
gen_model <- lm(ft_dpc ~ gender_num, train)

# Second Model:  Obama Thermometer on education
# use dem_edu, which has 18 unique outcomes
# While the codebook doesn't specify every outcome, it can be assumed from
# the information they do give that a higher value is more education
# change from factor to nuumeric
edu_num <- as.numeric(train$dem_edu)
# look at unique values: they are numbers 1-19, but there is no 2
table(edu_num)
# regress Obama thermometer on education
edu_model <- lm(ft_dpc ~ edu_num, train)

# Third model: Obama thermometer on vice president thermometer (Biden)
table(train$ft_dvpc)
# Has the same possible values as 



# among women
predict(gen_model, data.frame(gender_num=2))
# among people with bachelors degrees
predict(edu_model, data.frame(edu_num=13))

