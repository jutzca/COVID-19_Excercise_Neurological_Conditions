## ---------------------------
##
## Script name: 3_GLM_and_URP
##
## Purpose of script: To determine predictors of physical activity using GLMs and URP.
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
## https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r
##
## ---------------------------
##
## load up the packages we will need:  
##
library(partykit)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)
library(reshape2)
library(sjPlot)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(partykit)){install.packages("partykit")}
# if(!require(plyr)){install.packages("plyr")}
# if(!require(readr)){install.packages("readr")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(caret)){install.packages("caret")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(repr)){install.packages("repr")}
# if(!require(glmnet)){install.packages("glmnet")}
# if(!require(sjPlot)){install.packages("sjPlot")}
# if(!require(reshape2)){install.packages("reshape2")}
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

# Display all the variable names
names(covid19.survey.data)

# Take a glimpse at the data and its structure
dplyr::glimpse(covid19.survey.data)

#--------- Non-linear GLM: Depression ----------

# Subset data
covid19.survey.data.depression <-subset(covid19.survey.data, Depression_SCORE> 0)

# Create model
glm.depression <- glm(Depression_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+PASIDP_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.depression)
summary(glm.depression)
sjPlot::tab_model(glm.depression)

  # Plot model output
plot(glm.depression)

#--------- Non-linear GLM: Anxiety ----------

# Subset data
covid19.survey.data.anxiety <-subset(covid19.survey.data, Anxiety_SCORE> 0)

# Create model
glm.anxiety <- glm(Anxiety_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+PASIDP_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.anxiety)
summary(glm.anxiety)
sjPlot::tab_model(glm.anxiety)

# Plot model output
plot(glm.anxiety)

#--------- Non-linear GLM: HAQ_SDI_Mean ----------

# Subset data
covid19.survey.data.HAQ_SDI_Mean <-subset(covid19.survey.data, HAQ_SDI_Mean> 0)

# Create model
glm.HAQ_SDI_Mean <- glm(HAQ_SDI_Mean~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.HAQ_SDI_Mean)
summary(glm.HAQ_SDI_Mean)
sjPlot::tab_model(glm.HAQ_SDI_Mean)

# Plot model output
plot(glm.HAQ_SDI_Mean)

#--------- Non-linear GLM: Pain ----------

# Subset data
covid19.survey.data.Pain <-subset(covid19.survey.data, Pain> 0)

# Create model
glm.Pain <- glm(Pain~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.Pain)
summary(glm.Pain)
sjPlot::tab_model(glm.Pain)

# Plot model output
plot(glm.Pain)

#--------- Non-linear GLM: Fatigue ----------

# Subset data
covid19.survey.data.FSS_SCORE <-subset(covid19.survey.data, FSS_SCORE> 0)

# Create model
glm.FSS_SCORE <- glm(FSS_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.FSS_SCORE)
summary(glm.FSS_SCORE)
sjPlot::tab_model(glm.FSS_SCORE)

# Plot model output
plot(glm.FSS_SCORE)


#--------- Non-linear GLM: Loneliness ----------

# Subset data
covid19.survey.data.UCLA_Loneliness_SCORE <-subset(covid19.survey.data, UCLA_Loneliness_SCORE> 0)

# Create model
glm.UCLA_Loneliness_SCORE <- glm(UCLA_Loneliness_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, family = Gamma(), data = covid19.survey.data.UCLA_Loneliness_SCORE)
summary(glm.UCLA_Loneliness_SCORE)
sjPlot::tab_model(glm.UCLA_Loneliness_SCORE)

# Plot model output
plot(glm.UCLA_Loneliness_SCORE)



#------------------- Unbiased recursive partitioning ---------------------------

#--------- URP: Depression ----------
urp.model.depression<-partykit::ctree(Depression_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.depression
plot(urp.model.depression)

info_node(node_party(urp.model.depression))

#--------- URP: Anxiety ----------
#subset data to remove NA from the y variable
Anxiety_score_without_na <-subset(covid19.survey.data, (!is.na(Anxiety_SCORE)))

urp.model.anxiety<-partykit::ctree(Anxiety_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=Anxiety_score_without_na, na.action = na.pass)
urp.model.anxiety
plot(urp.model.anxiety)

info_node(node_party(urp.model.anxiety))

#--------- URP: HAQ_SDI_Mean ----------
urp.model.HAQ_SDI_Mean<-partykit::ctree(HAQ_SDI_Mean~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.HAQ_SDI_Mean
plot(urp.model.HAQ_SDI_Mean)

info_node(node_party(urp.model.HAQ_SDI_Mean))


#--------- URP: Pain ----------
urp.model.pain<-partykit::ctree(Pain~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.pain
plot(urp.model.pain)

info_node(node_party(urp.model.pain))

#--------- URP: Change in PA ----------
urp.model.change_in_PA<-partykit::ctree(Change_in_PA~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.change_in_PA
plot(urp.model.change_in_PA)

info_node(node_party(urp.model.change_in_PA))


#--------- URP: Fatigue ----------
urp.model.fatigue<-partykit::ctree(FSS_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.fatigue
plot(urp.model.fatigue)

info_node(node_party(urp.model.fatigue))


#--------- URP: Vitality ----------
urp.model.vitality<-partykit::ctree(SVS_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.vitality
plot(urp.model.vitality)

info_node(node_party(urp.model.vitality))


#--------- URP: UCLA_Loneliness_SCORE ----------
urp.model.loneliness<-partykit::ctree(UCLA_Loneliness_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+LTPA_SCORE+Household_activity_SCORE+Work_related_activity_SCORE+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.loneliness
plot(urp.model.loneliness)

info_node(node_party(urp.model.loneliness))



#--------- URP: PASIDP_SCORE ----------
urp.model.PASIDP_SCORE<-partykit::ctree(PASIDP_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Condition)+as.factor(Mobility_Aid)+Sedentary_Hrs_Per_Day, data=covid19.survey.data, na.action = na.pass)
urp.model.PASIDP_SCORE
plot(urp.model.PASIDP_SCORE)

info_node(node_party(urp.model.PASIDP_SCORE))



#---- Heatmap ----

covid19.survey.data_scores.overall <- covid19.survey.data[,c(3,6,8,24,26,28,30,32,34,11,13,15,17,19,21)]

neurological.condition <-unique(covid19.survey.data_scores.overall$Condition)
neurological.condition<-subset(neurological.condition, (!(neurological.condition=="MD" |neurological.condition=="FM"  )))

sex<-unique(covid19.survey.data_scores.overall$Sex)
mobility.aid<-unique(covid19.survey.data_scores.overall$Mobility_Aid)
mobility.aid<-subset(mobility.aid, (!(mobility.aid=="mobility scooter"| mobility.aid=="crutches"| mobility.aid=="other"| mobility.aid=="zimmer frame")))


for (i in neurological.condition) {
  
  covid19.survey.data_scores.overall_subset <-subset(covid19.survey.data_scores.overall, Condition ==i)
  
  # 1. Change to all variables to numeric
  covid19.survey.data.numeric <- lapply(covid19.survey.data_scores.overall_subset, as.numeric)
  covid19.survey.data.numeric.df<-as.data.frame(covid19.survey.data_scores.overall_subset)
  
  # 2. Prepare data
  mydata <- covid19.survey.data.numeric.df[, -c(1,2,3)] #remove first column
  head(mydata)
  
  # 3. Create correlation matrix using the R function cor() :
  cormat <- round(cor(mydata, method =  "spearman", use = "pairwise.complete.obs"),2)
  head(cormat)
  
  # 4. Create the correlation heatmap with ggplot2
  melted_cormat <- reshape2::melt(cormat)
  head(melted_cormat)
  
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  
  # 5. Get the lower and upper triangles of the correlation matrix
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  
  # 5. Finished correlation matrix heatmap
  # Melt the correlation matrix
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot2::ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1), 
          axis.text.y = element_text(size=10))+
    coord_fixed()
  # Print the heatmap
  print(ggheatmap)
  
  # Add correlation coefficients on the heatmap
  
  myplot<- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + ggtitle(i)+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  ggsave(myplot,
         filename=paste('Neurological_condition_',i,".pdf",sep=""),
         path='/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Figures/Heat_plots/',
         device = 'pdf',
         scale = 1,
         width = 9,
         height = 8,
         units = "in",
         dpi = 300
  )

}



#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
