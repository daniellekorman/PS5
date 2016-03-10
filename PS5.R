# Problem Set 5
## March 10
## Danielle Korman

# Code given to set up:

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

# First get rid of negative numbers in Obama thermometer variable, ft_dpc
table(anes$ft_dpc)
# change to NA
anes$ft_dpc[anes$ft_dpc==-9] <- NA
anes$ft_dpc[anes$ft_dpc==-8] <- NA
anes$ft_dpc[anes$ft_dpc==-2] <- NA

# Make table of variables using for regression
anes2 <- subset(anes, select=c("ft_dpc", "gender_respondent_x", "dem_edu", 
                               "ft_rpc", "ft_gwb", "ft_rep"))
colnames(anes2) <- c("Obama", "Gender", "Education", "Romney", "Bush", "Rep")

# Make all data that does not fit linearly NA
table(anes2$Gender)
anes2$Gender <- as.numeric(anes2$Gender)
# Gender has no NA and only 1's and 2's
table(as.numeric(anes2$Education))
anes2$Education <- as.numeric(anes2$Education)
# Eduction needs 1 and 19 to be NA because in the numeric form
# these are the same as -9 and 95
anes2$Education[anes2$Education==1] <- NA
anes2$Education[anes2$Education==19] <- NA
table(anes2$Education)
# The last 3 variables are also thermometers and will be the same as Obama's
anes2[anes2==-8] <- NA
anes2[anes2==-9] <- NA
anes2[anes2==-2] <- NA

# Split data into test and train
dim(anes2)
partition <- sample(1:nrow(anes2), size=0.5*nrow(anes2))
test <- anes2[partition,]
dim(test) 
train <- anes2[-partition,]
dim(train)

# Statistical Models
# First Model: Regress Obama Thermometer on Gender
# 1 is male and 2 is female
# Regress Obama Thermometer against gender
gen_model <- lm(Obama ~ Gender, train)
summary(gen_model)

# Second Model:  Obama Thermometer on education
# Higher education numbers means higher level of education, from first grade to 
# Doctorate
# regress Obama thermometer on education
edu_model <- lm(Obama ~ Education, train)
summary(edu_model)

# Third model: Obama thermometer on Republican opinions
# Obama Thermometer regressed on opinions on Romney, George W Bush, and 
# Republican Party
table(train$ft_dvpc)
# Has the same possible values as 



# among women
predict(gen_model, data.frame(gender_num=2))
# among people with bachelors degrees
predict(edu_model, data.frame(edu_num=13))

