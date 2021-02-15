## ---------------------------
##
## Script name: 5_radar_charts_covid_survey
##
## Purpose of script: To create radar charts to visualize the results of the COVID-19 survey
##                    
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-01-03
##
## Copyright (c) Catherine Jutzeler, 2021
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: COVID-19 survey
##
## Notes: This analysis is for the publication Nightingale et al, 2021 published in XX. [add link here]
##        https://www.data-to-viz.com/caveat/spider.html
##
## ---------------------------
##
## load up the packages we will need:  
##
library(tidyverse)
library(table1)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(plyr)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(fmsb)
library(colormap)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(table1)){install.packages("table1")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(reshape2)){install.packages("reshape2")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(tidyverse)){install.packages("tidyverse")}
# if(!require(patchwork)){install.packages("patchwork")}
# if(!require(hrbrthemes)){install.packages("hrbrthemes")}
# if(!require(fmsb)){install.packages("fmsb")}
# if(!require(colormap)){install.packages("colormap")}
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
setwd("/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/")
##
##
outdir_figures='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/'
outdir_tables='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original data
covid19.survey.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')
names(covid19.survey.data)

# Relabel variable levels
levels(covid19.survey.data$Condition) <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndromee, CRPS" , "Muscular dystrophy, neuromuscular diseases", 
                                           "Multiple Sclerosis", "Parkinson's disease", "Spinal Cord Injury", "Stroke, ataxia's, other (spina bifida, dystonia)")

levels(covid19.survey.data$Mobility_Aid) <- c("Manual wheelchair","Power wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                                              "None", "Other")

levels(covid19.survey.data$Sex) <- c("Female", "Male", "Prefer not to disclose")

#---------- Radar plot for LTPDA stratified by neurological condition ---------- 
# Subset data
covid19.survey.data_scores.ltpa <- covid19.survey.data[,c(6,11,13,15,17,19,21)]

# Calculate mean of each column stratified by neurological condition
covid19.survey.data_neurol_cond <- covid19.survey.data_scores.ltpa %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(across(Sedentary_Hrs_Per_Day:Exercise_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_neurol_cond_subset<-covid19.survey.data_neurol_cond[,-c(1)]
#covid19.survey.data_neurol_cond_subset <- covid19.survey.data_neurol_cond[,-c(1, 16:24)]

# Replace NA's with 0
covid19.survey.data_neurol_cond_subset[is.na(covid19.survey.data_neurol_cond_subset)] <- 0

## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_neurol_cond_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_neurol_cond_subset)

# Prepare color: 7 shades one for each level of the variable neurological condition (from Cerebral Palsy to Stroke) 
colors_border=colormap(colormap=colormaps$viridis, nshades=7, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=7, alpha=0.3)

# Prepare title for plots
mytitle.neurol.cond <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndromee, CRPS", "Muscular dystrophy, neuromuscular diseases", "Multiple Sclerosis", 
             "Parkinson's disease", "Spinal Cord Injury", 'Stroke, ataxias, other (spina bifida, dystonia)')


# Split the screen in 7 parts
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

# Loop for each plot
for(i in 1:7){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_neurol_cond_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
              
              # custom polygon
              pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
              
              # custom the grid
              cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,4,1), cglwd=0.6,
              
              # custom labels
              vlcex=0.8, 
              vlabels = c("Sedentary Hours/ Day","Walking/Wheeling Score","Light Sport Score","Moderate Sport Score","Strenous Sport Score", "Exercise Score"), 
              
              # add title
              title=mytitle.neurol.cond[i]
  )
}

#---------- Radar plot for LTPDA stratified by sex ---------- 

# Subset data
covid19.survey.data_scores.ltpa <- covid19.survey.data[,c(3,11,13,15,17,19,21)]

# Calculate mean of each column stratified by sex
covid19.survey.data_sex <- covid19.survey.data_scores.ltpa %>%
  group_by(Sex) %>%
  dplyr::summarise(across(Sedentary_Hrs_Per_Day:Exercise_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_sex_subset<-covid19.survey.data_sex[,-c(1)]

# Replace NA's with 0
covid19.survey.data_sex_subset[is.na(covid19.survey.data_sex_subset)] <- 0


## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_sex_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_sex_subset)

# Prepare color: 3 shades one for each level of the variable sex (i.e., female, male, prefer not to disclose)
colors_border=colormap(colormap=colormaps$viridis, nshades=3, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=3, alpha=0.3)

# Prepare title for plots
mytitle.sex <-c("Female", "Male", "Prefer not to disclose")

# Split the screen in 3 parts to place the plots
par(mar=rep(0.8,4))
par(mfrow=c(2,2))

# Loop for each plot
for(i in 1:3){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_sex_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
             
             # custom polygon
             pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
             
             # custom the grid
             cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,5,1), cglwd=0.6,
             
             # custom labels
             vlcex=0.8, 
             vlabels = c("Sedentary Hours/ Day","Walking/Wheeling Score","Light Sport Score","Moderate Sport Score","Strenous Sport Score", "Exercise Score"), 
             
             # add title
             title=mytitle.sex[i]
  )
}


#---------- Radar plot for LTPDA stratified by mobility aid ---------- 

# Subset data
covid19.survey.data_scores.ltpa <- covid19.survey.data[,c(8,11,13,15,17,19,21)]

# Calculate mean of each column stratified by mobility aid
covid19.survey.data_mobility_aid <- covid19.survey.data_scores.ltpa %>%
  group_by(Mobility_Aid) %>%
  dplyr::summarise(across(Sedentary_Hrs_Per_Day:Exercise_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_mobility_aid_subset<-covid19.survey.data_mobility_aid[,-c(1)]

# Replace NA's with 0
covid19.survey.data_mobility_aid_subset[is.na(covid19.survey.data_mobility_aid_subset)] <- 0


## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_mobility_aid_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_mobility_aid_subset)

# Prepare color: 3 shades one for each level of the variable mobility aid (i.e., female, male, prefer not to disclose)
colors_border=colormap(colormap=colormaps$viridis, nshades=8, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=8, alpha=0.3)

# Prepare title for plots
mytitle.mobility_aid <- c("Manual wheelchair","Powered wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                                                                                                               "None", "Other")

# Split the screen in 8 parts to place the plots
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

# Loop for each plot
for(i in 1:8){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_mobility_aid_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
             
             # custom polygon
             pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
             
             # custom the grid
             cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,5,1), cglwd=0.6,
             
             # custom labels
             vlcex=0.8, 
             vlabels = c("Sedentary Hours/ Day","Walking/Wheeling Score","Light Sport Score","Moderate Sport Score","Strenous Sport Score", "Exercise Score"), 
             
             # add title
             title=mytitle.mobility_aid[i]
  )
}







#---------- Radar plot for Household Score stratified by neurological condition ---------- 

# Subset data
covid19.survey.data_scores.hh_score <- covid19.survey.data[,c(6,24,26,28,30,32,34)]

# Calculate mean of each column stratified by neurological condition
covid19.survey.data_neurol_cond <- covid19.survey.data_scores.hh_score %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(across(Light_housework_SCORE:Caring_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_neurol_cond_subset<-covid19.survey.data_neurol_cond[,-c(1)]
#covid19.survey.data_neurol_cond_subset <- covid19.survey.data_neurol_cond[,-c(1, 16:24)]

# Replace NA's with 0
covid19.survey.data_neurol_cond_subset[is.na(covid19.survey.data_neurol_cond_subset)] <- 0

## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_neurol_cond_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_neurol_cond_subset)

# Prepare color: 7 shades one for each level of the variable neurological condition (from Cerebral Palsy to Stroke) 
colors_border=colormap(colormap=colormaps$viridis, nshades=7, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=7, alpha=0.3)

# Prepare title for plots
mytitle.neurol.cond <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndromee, CRPS", "Muscular dystrophy, neuromuscular diseases", "Multiple Sclerosis", 
                         "Parkinson's disease", "Spinal Cord Injury", 'Stroke, ataxias, other (spina bifida, dystonia)')


# Split the screen in 7 parts
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

# Loop for each plot
for(i in 1:7){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_neurol_cond_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
             
             # custom polygon
             pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
             
             # custom the grid
             cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,4,1), cglwd=0.6,
             
             # custom labels
             vlcex=0.8, 
             vlabels = c("Light Household Work Score","Heavy Household Work Score","Home Repairs Score","Yard Work Score"," Gardening Score", "Caring Score"), 
             
             # add title
             title=mytitle.neurol.cond[i]
  )
}

#---------- Radar plot for Radar plot for Household Score stratified by sex ---------- 

# Subset data
covid19.survey.data_scores.hh_score <- covid19.survey.data[,c(3,24,26,28,30,32,34)]

# Calculate mean of each column stratified by sex
covid19.survey.data_sex <- covid19.survey.data_scores.hh_score %>%
  group_by(Sex) %>%
  dplyr::summarise(across(Light_housework_SCORE:Caring_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_sex_subset<-covid19.survey.data_sex[,-c(1)]

# Replace NA's with 0
covid19.survey.data_sex_subset[is.na(covid19.survey.data_sex_subset)] <- 0


## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_sex_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_sex_subset)

# Prepare color: 3 shades one for each level of the variable sex (i.e., female, male, prefer not to disclose)
colors_border=colormap(colormap=colormaps$viridis, nshades=3, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=3, alpha=0.3)

# Prepare title for plots
mytitle.sex <-c("Female", "Male", "Prefer not to disclose")

# Split the screen in 3 parts to place the plots
par(mar=rep(0.8,4))
par(mfrow=c(2,2))

# Loop for each plot
for(i in 1:3){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_sex_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
             
             # custom polygon
             pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
             
             # custom the grid
             cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,5,1), cglwd=0.6,
             
             # custom labels
             vlcex=0.8, 
             vlabels = c("Light Household Work Score","Heavy Household Work Score","Home Repairs Score","Yard Work Score"," Gardening Score", "Caring Score"), 
             
             # add title
             title=mytitle.sex[i]
  )
}


#---------- Radar plot for Radar plot for Household Score stratified by mobility aid ---------- 

# Subset data
covid19.survey.data_scores.hh_score <- covid19.survey.data[,c(8,24,26,28,30,32,34)]

# Calculate mean of each column stratified by mobility aid
covid19.survey.data_mobility_aid <- covid19.survey.data_scores.hh_score %>%
  group_by(Mobility_Aid) %>%
  dplyr::summarise(across(Light_housework_SCORE:Caring_SCORE, mean)) %>%
  as.data.frame()

# Subset data
covid19.survey.data_mobility_aid_subset<-covid19.survey.data_mobility_aid[,-c(1)]

# Replace NA's with 0
covid19.survey.data_mobility_aid_subset[is.na(covid19.survey.data_mobility_aid_subset)] <- 0


## Radar plot
# To use the fmsb package, two lines have to be added to the dataframe: the max and min of each topic to show on the plot!
covid19.survey.data_mobility_aid_subset_for_radarplot <-rbind(rep(4,10) , rep(0,10) , covid19.survey.data_mobility_aid_subset)

# Prepare color: 3 shades one for each level of the variable mobility aid (i.e., female, male, prefer not to disclose)
colors_border=colormap(colormap=colormaps$viridis, nshades=8, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=8, alpha=0.3)

# Prepare title for plots
mytitle.mobility_aid <- c("Manual wheelchair","Powered wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                          "None", "Other")

# Split the screen in 8 parts to place the plots
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

# Loop for each plot
for(i in 1:8){
  
  # Custom the radarChart
  radarchart(covid19.survey.data_mobility_aid_subset_for_radarplot[c(1,2,i+2),], axistype=1, 
             
             # custom polygon
             pcol=colors_border[i] , pfcol=colors_in[i] , plwd=2, plty=1 , 
             
             # custom the grid
             cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,5,1), cglwd=0.6,
             
             # custom labels
             vlcex=0.8, 
             vlabels = c("Light Household Work Score","Heavy Household Work Score","Home Repairs Score","Yard Work Score"," Gardening Score", "Caring Score"), 
             
             # add title
             title=mytitle.mobility_aid[i]
  )
}










