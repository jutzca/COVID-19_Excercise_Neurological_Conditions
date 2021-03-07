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
covid19.qual.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid_survey_qual_data_facilitator.csv", header = T, sep = ',')
names(covid19.qual.data)

# Reformat data from wide to long
covid19.qual.data_long <- tidyr::gather(covid19.qual.data, category, response, Social.contact.fun:Misc, factor_key=TRUE)
covid19.qual.data_long

#---------- Count number of responses per categories ---------- 
covid19.qual.data_long.count <- covid19.qual.data_long%>%
  filter(!is.na(response))%>%
  dplyr::count(category, response)%>%
  mutate(percentage=(n/199*100))
covid19.qual.data_long.count

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
                                  title="Blood Brain Barrier Permeability", 
                                  xlab="One square == 1%")+theme(plot.title = element_text(size=12, family = 'Times', face = 'bold'), 
                                                                 axis.title = element_text(family = "Times"),
                                                                 legend.text = element_text(family = "Times"))
covid19.qual.data.waffle.plot

# 2. Create pie chart

# Basic bar
pie.covid19 <-covid19.qual.data_long.count%>%dplyr::select(c(1,4))%>%
ggplot2::ggplot(aes(x="", y="n", fill=category)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie.covid19 = pie.covid19 + coord_polar("y", start=0) + geom_text(aes(label = paste0("n"), "%"), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie.covid19 = pie.covid19 + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Phones - Market Share")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))


