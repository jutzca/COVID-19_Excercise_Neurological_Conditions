## ---------------------------
##
## Script name: 1_COVID-19_survey
##
## Purpose of script: To analyze and visualize the data from the COVID-19 survey assessing how the COVID-10 pamdemic has impacted the physical activity in patients
##                    suffering from neurological diseases. A cross-sectional study
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
#### ---------------------------

## set working directory

setwd("/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/")

## ---------------------------
## load up the packages we will need:  
library(table1)
library(ggplot2)
library(likert)
library(HH)
library(dplyr)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(table1)){install.packages("table1")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(likert)){install.packages("likert")}
#if(!require(HH)){install.packages("HH")}
#if(!require(dplyr)){install.packages("dplyr")}


#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/'
outdir_tables='/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original data
covid19.survey.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')
names(covid19.survey.data)


#----- Create Summary Table of Included Cohort -----

### Formatting of table: Customize levels, labels, and units of listed variables

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
levels(covid19.survey.data$Condition) <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndromee, CRPS" , "Muscular dystrophy, neuromuscular diseases", 
                                           "Multiple Sclerosis", "Parkinson's disease", "Spinal Cord Injury", "Stroke, ataxia's, other (spina bifida, dystonia)")
levels(covid19.survey.data$Mobility_Aid) <- c("Manual wheelchair","Power wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                                                                                        "None", "Other")

# 4. Relable variables
label(covid19.survey.data$Sex) <- "Sex, n (%)"
label(covid19.survey.data$Age) <- "Age"
label(covid19.survey.data$Mobility_Aid) <- "Mobility Aid, n (%)"
label(covid19.survey.data$GRSI) <- "Government Response Stringency Index, n (%)"
label(covid19.survey.data$Condition) <- 'Condition, n (%)'
label(covid19.survey.data$Ethnicity) <- 'Ethnicity, n (%)'
label(covid19.survey.data$Situation) <- 'Situation, n (%)'
label(covid19.survey.data$HAQ_SDI_Mean) <- 'HAQ SDI'



# 5. Assign units to Age at Injury and Year of Injury
units(covid19.survey.data$Age) <- "years"

# 6. Print table
table1::table1(~ Sex+Age+Ethnicity+Condition+Situation+Mobility_Aid+GRSI+HAQ_SDI_Mean , data = covid19.survey.data)


# 7. Draw historgrams and save plots if required

histogram_HAQ_SDI_Mean<-ggplot(covid19.survey.data, aes(x = HAQ_SDI_Mean, fill = Sex)) +                      
  geom_histogram(position = "identity", alpha = 0.5, bins = 25)+
  theme_bw()
histogram_HAQ_SDI_Mean


##Save plot
ggsave(
  "drug.prevalence.7-14-30-60days.plot.sygen.pdf",
  plot = prevalence.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 4,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



glimpse(covid19.survey.data)


http://rnotr.com/likert/ggplot/barometer/likert-plots/

covid19.survey.data2 <- subset(covid19.survey.data, (!(is.na(Change_in_PA))))
                               

##Calculate the proportions per group
covid19.survey.data.change.in.PA <- covid19.survey.data2 %>%
  group_by(Condition,Change_in_PA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  as.data.frame()
  
#Reformat dataframe from long to wide
covid19.survey.data.change.in.PA.wide <-reshape(data=covid19.survey.data.change.in.PA, timevar = "Change_in_PA", idvar = "Condition", direction = "wide")



ggplot() + geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(title=mytitle, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))



#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

