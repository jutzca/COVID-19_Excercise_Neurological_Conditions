## ---------------------------
##
## Script name: 1_COVID-19_survey
##
## Purpose of script: To analyze and visualize the data from the COVID-19 survey assessing how the COVID-10 pamdemic has impacted the physical activity in patients
##                    suffering from neurological diseases. This code was written to create Table 1 and 2 of the manuscript.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-3
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
## ---------------------------
##
## load up the packages we will need:  
##
library(table1)
library(ggplot2)
library(likert)
library(HH)
library(dplyr)
library(Hmisc)
library(finalfit) 
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(table1)){install.packages("table1")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(likert)){install.packages("likert")}
#if(!require(HH)){install.packages("HH")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(Hmisc)){install.packages("Hmisc")}
#if(!require(finalfit)){install.packages("finalfit")}
##
#### ---------------------------
##
## R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
#### ---------------------------
##
## Set working directory and output directorypaths
##
setwd("/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/")
##
##
outdir_figures='/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/'
outdir_tables='/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original data
covid19.survey.data <- read.csv("~/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')
names(covid19.survey.data)


#---------- Data cleaning and assessment ---------- 

# 1. Count number of NA's in entire dataframe
sum(is.na(covid19.survey.data))

# 2. Count number of NA's per coloumn
na_count <-sapply(covid19.survey.data, function(y) sum(length(which(is.na(y))))) %>%
  as.data.frame()%>%sum()
na_count

#write.csv(na_count, '/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/number_of_NA.csv')

# 3. Count number of complete answers
full_count <-sapply(covid19.survey.data, function(y) sum(length(which(!(is.na(y)))))) %>%
  as.data.frame()%>%sum()
full_count

# 4. Assess mechanism of missingness

# Examine with ff_glimpse
covid19.survey.data %>%
  finalfit::ff_glimpse()

# Identify missing values in each variable
covid19.survey.data %>%
  finalfit::missing_plot()

# Look for patterns of missingness
missing.data.pattern.plot <- covid19.survey.data %>% 
  finalfit::missing_pattern()
missing.data.pattern.plot


#---------- Create Summary Table of Included Cohort ---------- 

# Formatting of table: Customize levels, labels, and units of listed variables

# 1. Change class of variables
covid19.survey.data$GRSI <- as.factor(covid19.survey.data$GRSI )

# 2. Change order of levels of specific factors

covid19.survey.data$Situation <- factor(covid19.survey.data$Situation, levels = c("self-imposed isolation", "goverment-issued isolation", 
                                                                                   "social distancing", "none", "other"))
covid19.survey.data$Mobility_Aid <- factor(covid19.survey.data$Mobility_Aid, levels = c("manual wheelchair","powered wheelchair", "mobility scooter", "zimmer frame","walking sticks", "crutches",
                                                                                        "none", "other"))
# 3. Change names of levels of variables
levels(covid19.survey.data$Sex) <- c("Female", "Male", "Prefer not to disclose")
levels(covid19.survey.data$Ethnicity) <- c("Asian/Asian British", "Black/African/Caribbean/Black British", "Caucasian/White", "Mixed/multiple ethnic groups", 'Other')
levels(covid19.survey.data$Situation) <- c("Self-imposed isolation/shielded (considered at-risk)", "Isolation due to government legislation", 
                                                                                   "Practising social distancing", "None of the above", "Other")
levels(covid19.survey.data$Condition) <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndrome, CRPS" , "Muscular dystrophy, neuromuscular diseases", 
                                           "Multiple Sclerosis", "Parkinson's disease", "Spinal Cord Injury", "Stroke, ataxia's, other (spina bifida, dystonia)")
levels(covid19.survey.data$Mobility_Aid) <- c("Manual wheelchair","Power wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                                                                                        "None", "Other")
# 4. Relable variables
Hmisc::label(covid19.survey.data$Sex) <- "Sex, n (%)"
Hmisc::label(covid19.survey.data$Age) <- "Age"
Hmisc::label(covid19.survey.data$Mobility_Aid) <- "Mobility Aid, n (%)"
Hmisc::label(covid19.survey.data$GRSI) <- "Government Response Stringency Index, n (%)"
Hmisc::label(covid19.survey.data$Condition) <- 'Condition, n (%)'
Hmisc::label(covid19.survey.data$Ethnicity) <- 'Ethnicity, n (%)'
Hmisc::label(covid19.survey.data$Situation) <- 'Situation, n (%)'
Hmisc::label(covid19.survey.data$HAQ_SDI_Mean) <- 'HAQ SDI'

# 5. Assign units to Age at Injury and Year of Injury
units(covid19.survey.data$Age) <- "years"

# 6. Print table
# Table 1: Demographics
table1::table1(~ Sex+Age+Ethnicity+Condition+Mobility_Aid+Situation+GRSI, data = covid19.survey.data, render.continuous=c(.="Median [Q1, Q3]"))

# Table 2: Physical Activity

# Create groups
Sedentary_Hrs_Per_Day_groups<-cut(covid19.survey.data$Sedentary_Hrs_Per_Day, c(-1,0,2,5),
                                  labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Walking_wheeling_Hours_Per_Day_groups<-cut(covid19.survey.data$Walking_wheeling_Hours_Per_Day, c(-1,0,2,5),
                                           labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Light_sport_Hours_Per_Day_groups<-cut(covid19.survey.data$Light_sport_Hours_Per_Day, c(-1,0,2,5),
                                      labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Moderate_sport_Hours_Per_Day_groups<-cut(covid19.survey.data$Moderate_sport_Hours_Per_Day, c(-1,0,2,5),
                                         labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Strenous_sport_Hours_Per_Day_groups<-cut(covid19.survey.data$Strenous_sport_Hours_Per_Day, c(-1,0,2,5),
                                         labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Exercise_Hours_Per_Day_groups<-cut(covid19.survey.data$Exercise_Hours_Per_Day, c(-1,0,2,5),
                                   labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Light_housework_Hours_Per_Day_groups<-cut(covid19.survey.data$Light_housework_Hours_Per_Day, c(-1,0,2,5),
                                          labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Heavy_housework_Hours_Per_Day_groups<-cut(covid19.survey.data$Heavy_housework_Hours_Per_Day, c(-1,0,2,5),
                                          labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Home_repairs_Hours_Per_Day_groups<-cut(covid19.survey.data$Home_repairs_Hours_Per_Day, c(-1,0,2,5),
                                       labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Yard_work_Hours_Per_Day_groups<-cut(covid19.survey.data$Yard_work_Hours_Per_Day, c(-1,0,2,5),
                                    labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Gardening_Hours_Per_Day_groups<-cut(covid19.survey.data$Gardening_Hours_Per_Day, c(-1,0,2,5),
                                    labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Caring_Hours_Per_Day_groups<-cut(covid19.survey.data$Caring_Hours_Per_Day, c(-1,0,2,5),
                                 labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Work_related_activity_Hours_Per_Day_groups<-cut(covid19.survey.data$Work_related_activity_Hours_Per_Day, c(-1,0,2,5),
                                                labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
LTPA_SCORE.grps<-cut(covid19.survey.data$LTPA_SCORE, c(-1,0,2,100),
                                                labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
PASIDP_SCORE.grps<-cut(covid19.survey.data$PASIDP_SCORE, c(-1,0,2,100),
                     labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Work_related_activity_SCORE.grps<-cut(covid19.survey.data$Work_related_activity_SCORE, c(-1,0,2,100),
                       labels=c("0 hrs", ">0-2 hrs",">2 hrs"))
Household_activity_SCORE.grps<-cut(covid19.survey.data$Household_activity_SCORE, c(-1,0,2,100),
                       labels=c("0 hrs", ">0-2 hrs",">2 hrs"))



table1::table1(~ Sedentary_Hrs_Per_Day+Sedentary_Hrs_Per_Day_groups+
                 Walking_wheeling_Hours_Per_Day+Walking_wheeling_Hours_Per_Day_groups+
                 Light_sport_Hours_Per_Day+Light_sport_Hours_Per_Day_groups+
                 Moderate_sport_Hours_Per_Day+Moderate_sport_Hours_Per_Day_groups+
                 Strenous_sport_Hours_Per_Day+Strenous_sport_Hours_Per_Day_groups+
                 Exercise_Hours_Per_Day+Exercise_Hours_Per_Day_groups+
                 Light_housework_Hours_Per_Day+Light_housework_Hours_Per_Day_groups+
               Heavy_housework_Hours_Per_Day+Heavy_housework_Hours_Per_Day_groups+
                 Home_repairs_Hours_Per_Day+Home_repairs_Hours_Per_Day_groups+
                 Yard_work_Hours_Per_Day+Yard_work_Hours_Per_Day_groups+
                 Gardening_Hours_Per_Day+Gardening_Hours_Per_Day_groups+
                 Caring_Hours_Per_Day+Caring_Hours_Per_Day_groups+
                 Work_related_activity_Hours_Per_Day+Work_related_activity_Hours_Per_Day_groups +
                 LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+
                 PASIDP_SCORE+LTPA_SCORE.grps+PASIDP_SCORE.grps+
                 Household_activity_SCORE.grps+Work_related_activity_SCORE.grps
                 , data = covid19.survey.data, render.continuous=c(.="Median [Q1, Q3]"))


# Table 3: Outcomes
Depression_SCORE.grp<-cut(covid19.survey.data$Depression_SCORE, c(-1,7,10,21),
                          labels=c("Normal", "Borderline abnormal","Abnormal"))

Anxiety_SCORE.grp<-cut(covid19.survey.data$Anxiety_SCORE, c(-1,7,10,21),
                       labels=c("Normal", "Borderline abnormal","Abnormal"))


table1::table1(~ HAQ_SDI_Mean+Pain+Fear_of_COVID_19_SCORE+UCLA_Loneliness_SCORE+SVS_SCORE+FSS_SCORE+Global_Fatigue+Anxiety_SCORE+Anxiety_SCORE.grp+Depression_SCORE+Depression_SCORE.grp| Condition, 
               data = covid19.survey.data, render.continuous=c(.="Median [Q1, Q3]"))




# 7. Draw historgrams and save plots

#----- HAQ_SDI_Mean by sex ----

# Create plot
histogram_HAQ_SDI_Mean.by_sex<-ggplot(covid19.survey.data, aes(x = HAQ_SDI_Mean, fill = Sex)) +                      
  geom_histogram(position = "identity", alpha = 0.5, bins = 25)+
  theme_bw()
histogram_HAQ_SDI_Mean.by_sex

# Save plot
ggsave(
  "histogram_HAQ_SDI_Mean.by_sex.pdf",
  plot = histogram_HAQ_SDI_Mean.by_sex,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---- By HAQ_SDI_Mean by Mobility Aid ----

# Create plot
histogram_HAQ_SDI_Mean.by_mobility_aid<-ggplot(covid19.survey.data, aes(x = HAQ_SDI_Mean, fill = Mobility_Aid)) +                      
  geom_histogram(position = "identity", alpha = 0.5, bins = 25)+
  theme_bw()
histogram_HAQ_SDI_Mean.by_mobility_aid

# Save plot
ggsave(
  "histogram_HAQ_SDI_Mean.by_mobility_aid.pdf",
  plot = histogram_HAQ_SDI_Mean.by_mobility_aid,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---- By HAQ_SDI_Mean by neurological conditions ----

# Create plot
histogram_HAQ_SDI_Mean.by_neurol_cond<-ggplot(covid19.survey.data, aes(x = HAQ_SDI_Mean, fill = Condition)) +                      
  geom_histogram(position = "identity", alpha = 0.5, bins = 25)+
  theme_bw()
histogram_HAQ_SDI_Mean.by_neurol_cond

# Save plot
ggsave(
  "histogram_HAQ_SDI_Mean.by_neurol_cond.pdf",
  plot = histogram_HAQ_SDI_Mean.by_neurol_cond,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
