## ---------------------------
##
## Script name: 7_Qualitative_Analysis
##
## Purpose of script: To analyze and visualize the qualitative data from the COVID-19 survey assessing how the COVID-10 pamdemic has impacted the physical activity in patients
##                    suffering from neurological diseases.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-3-6
##
## Copyright (c) Catherine Jutzeler, 2021
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
library(tidyr)
library(ggplot2)
library(dplyr)
library(waffle)
library(scales)
library(RColorBrewer)

##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(waffle)){install.packages("waffle")}
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
setwd("/Users/jutzelec/jutzca/Github/COVID-19_Excercise_Neurological_Conditions/")
##
##
outdir_figures='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/'
outdir_tables='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original data
covid19.qual.data.facilitator <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid_survey_qual_data_facilitator.csv", header = T, sep = ',')
names(covid19.qual.data.facilitator)

covid19.qual.data.challenges<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid_survey_qual_data_challenges.csv", header = T, sep = ',')
names(covid19.qual.data.challenges)

covid19.qual.data.barrier<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid_survey_qual_data_barrier.csv", header = T, sep = ',')
names(covid19.qual.data.barrier)


# Reformat data from wide to long
covid19.qual.data.facilitator_long <- tidyr::gather(covid19.qual.data.facilitator, category, response, Social.contact.fun:Misc, factor_key=TRUE)
covid19.qual.data.facilitator_long

covid19.qual.data.challenges_long <- tidyr::gather(covid19.qual.data.challenges, category, response, Family:Misc, factor_key=TRUE)
covid19.qual.data.challenges_long

covid19.qual.data.barrier_long <- tidyr::gather(covid19.qual.data.barrier, category, response, Motivation:Misc, factor_key=TRUE)
covid19.qual.data.barrier_long


#---------- Count number of responses per categories of facilitator ---------- 
covid19.qual.data.facilitator_long.count <- covid19.qual.data.facilitator_long%>%
  filter(!is.na(response))%>%
  dplyr::count(category, response)%>%
  mutate(percentage=(n/192*100))%>%
  mutate(value=n)
covid19.qual.data.facilitator_long.count


#---------- Count number of responses per categories of challenges ---------- 
covid19.qual.data.challenges_long.count <- covid19.qual.data.challenges_long%>%
  filter(!is.na(response))%>%
  dplyr::count(category, response)%>%
  mutate(percentage=(n/197*100))%>%
  mutate(value=n)
covid19.qual.data.challenges_long.count


#---------- Count number of responses per categories of challenges ---------- 
covid19.qual.data.barrier_long.count <- covid19.qual.data.barrier_long%>%
  filter(!is.na(response))%>%
  dplyr::count(category, response)%>%
  mutate(percentage=(n/196*100))%>%
  mutate(value=n)
covid19.qual.data.barrier_long.count




#---------- Create plots ---------- 
# 1. Create waffle plot
parts <- c(`Social contact/fun`=3.0,
           `Nothing/Same as ever`=17.1, 
           `Work related`=1.5,
           `Health mental and physical weight`=30.2,
           `Online classes, support`=14.1, 
           `Leaving the house/ Fresh air/ Garden/ Dog/ Horse/ Weather`=13.6,
           `Relieve boredom`=2.0,
           `Having time`=4.0)
           # `Family HCP support`=15.1, 
           # `Misc`=6.5)

covid19.qual.data.waffle.plot <- waffle::waffle(parts, rows=5, size=1, 
                                  title="Facilitator", 
                                  xlab="One square == 1%")+theme(plot.title = element_text(size=12, family = 'Times', face = 'bold'), 
                                                                 axis.title = element_text(family = "Times"),
                                                                 legend.text = element_text(family = "Times"))
covid19.qual.data.waffle.plot

# 2. Create pie chart

# Define the number of colors you want
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# Create blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#---------- Pie chart for facilitator ---------- 
pie.covid19.facilitator <-covid19.qual.data.facilitator_long.count%>%dplyr::select(c(1,4,5))%>%
ggplot2::ggplot(aes(x="", y=percentage, fill=category)) + geom_bar(stat="identity", width=2) +   
  geom_text(aes(label = percent(percentage/100)), size=4, position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y", start=0)+
  scale_fill_manual(values = c25) +
  blank_theme +ggtitle('Facilitator')+
  theme(axis.text.x=element_blank(), plot.title  = element_text(hjust=0.5))
pie.covid19.facilitator

# Save plot
ggsave(
  "pie.covid19.facilitator.pdf",
  plot = pie.covid19.facilitator,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Pie chart for challenges ---------- 
pie.covid19.challenges <-covid19.qual.data.challenges_long.count%>%dplyr::select(c(1,4,5))%>%
  ggplot2::ggplot(aes(x="", y=percentage, fill=category)) + geom_bar(stat="identity", width=1) +   
  geom_text(aes(label = percent(percentage/100)), size=4, position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y", start=0)+
  scale_fill_manual(values = c25) +
  blank_theme +ggtitle('Challenges')+
  theme(axis.text.x=element_blank(), plot.title  = element_text(hjust=0.5))
pie.covid19.challenges

# Save plot
ggsave(
  "pie.covid19.challenges.pdf",
  plot = pie.covid19.challenges,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Pie chart for barrier ---------- 
pie.covid19.barrier<-covid19.qual.data.barrier_long.count%>%dplyr::select(c(1,4,5))%>%
  ggplot2::ggplot(aes(x="", y=percentage, fill=category)) + geom_bar(stat="identity", width=1) +   
  geom_text(aes(label = percent(percentage/100)), size=4, position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y", start=0)+
  scale_fill_manual(values = c25) +
  blank_theme +
  blank_theme +ggtitle('Barrier')+
  theme(axis.text.x=element_blank(), plot.title  = element_text(hjust=0.5))
pie.covid19.barrier

# Save plot
ggsave(
  "pie.covid19.barrier.pdf",
  plot = pie.covid19.barrier,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


