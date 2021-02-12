## ---------------------------
##
## Script name: 2_Normality_testing
##
## Purpose of script: To test if the data and its residuals are normally distributed.
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
library("ggplot2")
library("dplyr")
library("ggpubr")
library("tidyr")
library("broom")
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(broom)){install.packages("broom")}
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

# Check data
set.seed(1234)
dplyr::sample_n(covid19.survey.data, 10)

# Note: The central limit theorem tells us that no matter what distribution things have, the sampling distribution tends to be normal if the sample is large enough (n > 30).

#---------- Visual methods  ---------- 

# Subset data to numeric variables
covid19.survey.data.subset <- covid19.survey.data%>%
  dplyr::select_if(is.numeric)

# Extract column names
col_names <- colnames(covid19.survey.data.subset)

# 1. Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped. Loop through column names and generata density plots.
for (i in col_names){
  # Create plot for all variables i
  myplot1 <- ggpubr::ggdensity(covid19.survey.data.subset, i, 
                            main = paste("Density plot of", i),
                            xlab =paste("Density plot of", i))
  # Save plot 
  ggsave(myplot1,filename=paste('Density_plots_',i,".pdf",sep=""),path='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/Density_plots')
  
}


# 2. Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.Loop through column names and generate Q-Q plots.

# Loop through column names and generata density plots
for (i in col_names){
  
  # Create plot for all variables i
  myplot1 <- ggpubr::ggqqplot(covid19.survey.data.subset, i, 
                               main = paste("QQ plot of", i),
                               xlab =paste("QQ plot of", i))
  
  # Save plot
  ggsave(myplot1,filename=paste('QQ_plots',i,".pdf",sep=""),path='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/QQ_plots')
}


#---------- Normality test  ---------- 

# Shapiro-Wilk test of normality for one variable (univariate):
sw_test_results <- covid19.survey.data.subset %>% dplyr::select_if(is.numeric)%>%
  tidyr::gather(key = "variable_name", value = "value", HAQ_SDI_Mean:GRSI) %>% 
  dplyr::group_by(variable_name)  %>% 
  dplyr::do(broom::tidy(shapiro.test(.$value))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-method)%>%
  as.data.frame()%>%
  dplyr::mutate_if(is.numeric, round, digits = 3)

# %>%mutate(variable_name = as.character(variable_name),
    # variable_name= sub("_", " ", variable_name, ignore.case = FALSE))
sw_test_results

write.csv(sw_test_results, "/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/Table_Shapiro_Wilk_test.csv", row.names=FALSE) 


# If p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
# If p-value > 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we can NOT assume the normality.

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

