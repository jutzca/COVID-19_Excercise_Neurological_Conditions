## ---------------------------
##
## Script name: 2_URP-TREE_analysis
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


#subset data to remove NA from the y variable
Anxiety_score_without_na <-subset(covid19.survey.data, (!is.na(Anxiety_SCORE)))


model1<-partykit::ctree(Anxiety_SCORE~Age+as.factor(Sex), data=Anxiety_score_without_na, na.action = na.pass)
plot(model1)


model2<-ctree(Anxiety_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Ethnicity)+as.factor(Condition)+Duration_Corrected+as.factor(Mobility_Aid)+PASIDP_SCORE, data=Anxiety_score_without_na, na.action = na.pass)
plot(model2)


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
