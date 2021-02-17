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
##if(!require(partykit)){install.packages("partykit")}
##if(!require(plyr)){install.packages("plyr")}
##if(!require(readr)){install.packages("readr")}
##if(!require(dplyr)){install.packages("dplyr")}
##if(!require(caret)){install.packages("caret")}
##if(!require(ggplot2)){install.packages("ggplot2")}
##if(!require(repr)){install.packages("repr")}
##if(!require(glmnet)){install.packages("glmnet")}
##if(!require(reshape2)){install.packages("reshape2")}
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
covid19.survey.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')

# Display all the variable names
names(covid19.survey.data)

# Take a glimpse at the data and its structure
dplyr::glimpse(covid19.survey.data)

# Create model matrix
x_vars <- model.matrix(Anxiety_SCORE ~ Ethnicity+Age+Sex+PASIDP_SCORE+Mobility_Aid+Condition+Sedentary_Hrs_Per_Day+Situation, data = covid19.survey.data)
y_var <- covid19.survey.data$HAQ_SDI_Mean
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)

# Identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

final <- cbind(y_var[test], pred)

# Checking the first six obs
head(final)

actual <- test$actual
preds <- test$predicted
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

# Inspecting beta coefficients: getting the list with the most important variables
coef(lasso_best)

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
