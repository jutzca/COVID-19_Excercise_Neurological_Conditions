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

##---- 1. Data Partitioning ----
# We will build our model on the training set and evaluate its performance on the test set. This is called the holdout-validation approach for 
# evaluating model performance.

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
cols = c('Age', 'Sex', 'Condition', 'Mobility_Aid', "PASIDP_SCORE",  'Duration_Corrected', 'Sedentary_Hrs_Per_Day',
         "Leaving_the_house_to_work_Hours_Per_Day", "Fear_of_COVID_19_SCORE", "UCLA_Loneliness_SCORE"                         
         ,"Global_Fatigue",  "Depression_SCORE", "GRSI")


# "Household_activity_SCORE"
# Anxiety_SCORE
# Work_related_activity_SCORE
#PASIDP_SCORE

# preProcess function from the caret package to complete the scaling task. 
# The pre-processing object is fit only to the training data.
pre_proc_val <- caret::preProcess(train[,cols], method = c("center", "scale"))

#The scaling is applied on both the train and test sets. 
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)


##---- 3. Linear Regression ----

lr = lm(HAQ_SDI_Mean ~ Age+Sex+PASIDP_SCORE+Mobility_Aid+Condition+Sedentary_Hrs_Per_Day, data = covid19.survey.data)
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
eval_metrics(lr, train, predictions, target = 'HAQ_SDI_Mean')

# Step 4.3 - predicting and evaluating the model on test data --> first numer is R2 and second RMSE
predictions = predict(lr, newdata = test)
eval_metrics(lr, test, predictions, target = 'HAQ_SDI_Mean')



##---- 5. Regularization: Ridge ----

cols_reg = c('LTPA_SCORE', "Anxiety_SCORE" )

dummies <- caret::dummyVars(Anxiety_SCORE ~ ., data = covid19.survey.data[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))


x = as.matrix(train_dummies)
y_train = train$Anxiety_SCORE

x_test = as.matrix(test_dummies)
y_test = test$Anxiety_SCORE

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet::glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)



##---- 6. Regularization: Lasso ----

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)


##---- 7. Regularization: ELastic Net ----
# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(unemploy ~ .,
                     data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)



# Best tuning parameter
elastic_reg$bestTune



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

###https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r


#subset data to remove NA from the y variable
Anxiety_score_without_na <-subset(covid19.survey.data, (!is.na(Anxiety_SCORE)))


model1<-partykit::ctree(Anxiety_SCORE~Age+as.factor(Sex), data=Anxiety_score_without_na, na.action = na.pass)
plot(model1)


model2<-ctree(Anxiety_SCORE~Age+as.factor(Sex)+as.factor(Situation)+as.factor(Ethnicity)+as.factor(Condition)+Duration_Corrected+as.factor(Mobility_Aid)+PASIDP_SCORE, data=Anxiety_score_without_na, na.action = na.pass)
plot(model2)


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
