## ---------------------------
##
## Script name: 2_Linear_regression_with_regularization
##
## Purpose of script: To determine predictors of physical activity.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-08
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: COVID-19 survey
##
## Notes: This analysis is for the publication Nightingale et al, 2021 published in XX. [add link here]
##   
#### ---------------------------

## set working directory

setwd("/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/")

## ---------------------------
## load up the packages we will need:  
library(partykit)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)



## ----------------------------
## Install packages needed:  (uncomment as required)

##if(!require(partykit)){install.packages("partykit")}
##if(!require(plyr)){install.packages("plyr")}
##if(!require(readr)){install.packages("readr")}
##if(!require(dplyr)){install.packages("dplyr")}
##if(!require(caret)){install.packages("caret")}
##if(!require(ggplot2)){install.packages("ggplot2")}
##if(!require(repr)){install.packages("repr")}


#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/'
outdir_tables='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original data
covid19.survey.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')

#Display all the variable names
names(covid19.survey.data)

#Take a glimpse at the data and its structure
glimpse(covid19.survey.data)

##---- 1. Data Partitioning ----
## We will build our model on the training set and evaluate its performance on the test set. This is called the holdout-validation approach for 
## evaluating model performance.

# The is line sets the random seed for reproducibility of results. 
set.seed(100) 

# The train set contains 70 percent of the data while the test set contains the remaining 30 percent.
index = sample(1:nrow(covid19.survey.data), 0.7*nrow(covid19.survey.data)) 

# Create the training data 
train = covid19.survey.data[index,] 

# Create the test data
test = covid19.survey.data[-index,] 

# Show dimension of train and test data set
dim(train)
dim(test)

##---- 2. Scaling the Numeric Features (variables) ----

# Create list containing the names of independent numeric variables. 
cols = c('Age', 'Duration_Corrected', 'Pain', 'Sedentary_Hrs_Per_Day', 'Sedentary_Hrs_Per_Day', 'Walking_wheeling_Hours_Per_Day', 'Walking_wheeling_SCORE',
         "Light_sport_Hours_Per_Day", "Light_sport_SCORE",  "Moderate_sport_Hours_Per_Day", "Moderate_sport_SCORE", "Strenous_sport_Hours_Per_Day",
         "Strenous_sport_SCORE", "Exercise_Hours_Per_Day", "Exercise_SCORE", "LTPA_SCORE", "Light_housework_Hours_Per_Day","Light_housework_SCORE", 
         "Heavy_housework_Hours_Per_Day", "Heavy.housework_SCORE", "Home_repairs_Hours_Per_Day", "Home_repairs_SCORE", "Yard_work_Hours_Per_Day",
         "Yard_work_SCORE", "Gardening_Hours_Per_Day", "Gardening_SCORE", "Caring_Hours_Per_Day", "Caring_SCORE",  "Household_activity_SCORE","Work_related_activity_Hours_Per_Day",  "Work_related_activity_SCORE",      
       "PASIDP_SCORE",  "Leaving_the_house_to_work_Hours_Per_Day", "Fear_of_COVID_19_SCORE", "UCLA_Loneliness_SCORE", "SVS_SCORE",                           
         "FSS_SCORE","Global_Fatigue","Anxiety_SCORE",  "Depression_SCORE", "GRSI")


# preProcess function from the caret package to complete the scaling task. 
# The pre-processing object is fit only to the training data.
pre_proc_val <- caret::preProcess(train[,cols], method = c("center", "scale"))

#The scaling is applied on both the train and test sets. 
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)


##---- 3. Linear Regression ----

lr = lm(unemploy ~ uempmed + psavert + pop + pce, data = train)
summary(lr)


##---- 4. Model Evaluation Metrics ----
#Step 4.1 - create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Step 4.2 - predicting and evaluating the model on train data
predictions = predict(lr, newdata = train)
eval_metrics(lr, train, predictions, target = 'unemploy')

# Step 4.3 - predicting and evaluating the model on test data
predictions = predict(lr, newdata = test)
eval_metrics(lr, test, predictions, target = 'unemploy')


##---- 5. Regularization----








#subset data to remove NA from the y variable
Anxiety_score_without_na <-subset(covid19.survey.data, (!is.na(Anxiety_SCORE)))


model1<-partykit::ctree(Anxiety_SCORE~Age+as.factor(Sex), data=Anxiety_score_without_na, na.action = na.pass)
plot(model1)


model2<-ctree(Anxiety_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Ethnicity)+as.factor(Condition)+Duration_Corrected+as.factor(Mobility_Aid)+PASIDP_SCORE, data=Anxiety_score_without_na, na.action = na.pass)
plot(model2)


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
