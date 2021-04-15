## ---------------------------
##
## Script name: 4_Likert_graph_covid_survey
##
## Purpose of script: To create likert graphs to visualize the physicial activity question in the COVID-19 survey
##                    
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-28
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: COVID-19 survey
##
## Notes: This analysis is for the publication Nightingale et al, 2021 published in XX. [add link here]
##        http://rnotr.com/likert/ggplot/barometer/likert-plots/
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
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(stringr)
library(Hmisc)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(table1)){install.packages("table1")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(likert)){install.packages("likert")}
# if(!require(HH)){install.packages("HH")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(reshape2)){install.packages("reshape2")}
# if(!require(RColorBrewer)){install.packages("RColorBrewer")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(stringr)){install.packages("stringr")}
# if(!require(Hmisc)){install.packages("Hmisc")}
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
covid19.survey.data <- read.csv("~/19_COVID_Survey/covid19_data_survey.csv", header = T, sep = ',')
names(covid19.survey.data)

# Relabel variable levels
levels(covid19.survey.data$Condition) <- c("Cerebral Palsy", "Fibromyalgia, Chronic fatigue syndrome, CRPS" , "Muscular dystrophy, neuromuscular diseases", 
                                           "Multiple Sclerosis", "Parkinson's disease", "Spinal Cord Injury", "Stroke, ataxia's, other (spina bifida, dystonia)")

levels(covid19.survey.data$Mobility_Aid) <- c("Manual wheelchair","Power wheelchair", "Mobility scooter", "Zimmer frame","Walking sticks", "Crutches",
                                              "None", "Other")

levels(covid19.survey.data$Sex) <- c("Female", "Male", "Prefer not to disclose")

# Subset data to remove NAs from Change_in_PA variable
covid19.survey.data2 <- subset(covid19.survey.data, (!(is.na(Change_in_PA))))

#---------- Changes in physical activity stratified by neurological conditions ---------- 

# Calculate the proportions per neurological condition
covid19.survey.data.change.in.PA.neurol.cond <- covid19.survey.data2 %>%
  group_by(Condition, Change_in_PA) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))%>%
  as.data.frame()
covid19.survey.data.change.in.PA.neurol.cond

# Save table
write.csv(covid19.survey.data.change.in.PA.neurol.cond,"/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/Likert_scale_change_in_PA_neurol.condition.csv", row.names = F)

# Reformat dataframe from long to wide
covid19.survey.data.change.in.PA.wide <-reshape(data=covid19.survey.data.change.in.PA.neurol.cond, timevar = "Change_in_PA", idvar = "Condition", direction = "wide")

# Subset data
covid19.survey.data.change.in.PA.wide_subset <-covid19.survey.data.change.in.PA.wide[, -c(2,4,6,8,10)]

# Reorder data to follow the levels: "considerably less", "slightly less", "about the same", "slightly more",  "considerably more"
likert.graph.data <- covid19.survey.data.change.in.PA.wide_subset[,c(1,3,5,2,6,4)]

# Replace NA's with 0
likert.graph.data[is.na(likert.graph.data)] <- 0

# Create labels
mylevels<-c("considerably less", "slightly less","about the same","slightly more","considerably more")

## To handle the center category of 'about the same' this estimate is divided by two, and then include it twice. 
## That way,half of this category can be plotted below the center line and the other half above this line. The indvidual steps
## are described.

# Count number of levels (-1 removes the column 'condition' from the count)
numlevels<-length(likert.graph.data[1,])-1  #count on the first line

# Determine center of levels
numcenter<-ceiling(numlevels/2)+1

# Create column with center values divided by to
likert.graph.data$midvalues<-likert.graph.data[,numcenter]/2

tab2<-cbind(likert.graph.data[,1],likert.graph.data[,2:ceiling(numlevels/2)],
            likert.graph.data$midvalues,likert.graph.data$midvalues,likert.graph.data[,numcenter:numlevels+1])
colnames(tab2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                  "midhigh",mylevels[numcenter:numlevels])


## To avoid too much blank space, find the closest 25% break below the minimum value and above the maximum value. Then,
## pass these to ggplot???s limits option.

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100


# For applying colors, the data set has six columns of estimates, but only have five levels on the factor (from 'considerably less' to 'considerablly more'). 
# Thus, two palettes hava to ebe created: one to color the plot and one to make the legend. 

numlevels<-length(likert.graph.data[1,])-1
temp.rows<-length(tab2[,1])
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF" ## replace the default value with a darker grey 
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])


# Create new dataframe by reformating from wide to long
tab3<-melt(tab2,id="outcome")

# Manually assign colors using the above-created palette with length six
tab3$col<-rep(pal,each=temp.rows)

# Multiply everything by 100 to get into a percent rather than decimal format
tab3$value<-tab3$value*100

# Wrap long labels 
tab3$outcome<-str_wrap(tab3$outcome, width = 100)

# To order the plot from least to most 
tab3$outcome<-factor(tab3$outcome, levels = tab2$outcome[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])

# Split data frame into two equal halves: highs and lows
highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
lows<-na.omit(tab3[1:(length(tab3[,1])/2),])

# Reverse order of 'lows'
lows <- lows[rev(rownames(lows)),]

# Order the color of the 'lows' according to labels
lows$col <-factor(lows$col, levels = c("#CA0020", "#F4A582","#DFDFDF"))

# Create plot
likert.plot_neurol_condition <- ggplot() + geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + #choose theme
  coord_flip() + # flip coordinate system. Note: the y-axis will still be the y-axis for modifiction in ggplot
  labs(title='Change in Physical Activity \n by Neurological Condition', y="",x="") +   # add title to plot
  theme(plot.title = element_text(size=12, hjust=0.5)) + # change font size of title
  theme(axis.text.y = element_text(hjust=0), # change position of y axis text
        legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.text = element_text(size=10)) +   # add legend at the bottom of the graph
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) # customize the limits of the y-axis
likert.plot_neurol_condition

# Save plot
ggsave(
  "likert.plot.change.in.PA_neurol_condition.pdf",
  plot = likert.plot_neurol_condition,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Changes in physical activity stratified by sex (female, male, prefer not to disclose) ---------- 

# Calculate the proportions per sex
covid19.survey.data.change.in.PA.sex <- covid19.survey.data2 %>%
  group_by(Sex, Change_in_PA) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  as.data.frame()
covid19.survey.data.change.in.PA.sex

# Save table
write.csv(covid19.survey.data.change.in.PA.sex,"/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/Likert_scale_change_in_PA_sex.csv", row.names = F)


# Reformat dataframe from long to wide
covid19.survey.data.change.in.PA.wide <-reshape(data=covid19.survey.data.change.in.PA.sex, timevar = "Change_in_PA", idvar = "Sex", direction = "wide")

# Subset data
covid19.survey.data.change.in.PA.wide_subset <-covid19.survey.data.change.in.PA.wide[, -c(2,4,6,8,10)]

# Reorder data to follow the levels: "considerably less", "slightly less", "about the same", "slightly more",  "considerably more"
likert.graph.data <- covid19.survey.data.change.in.PA.wide_subset[,c(1,3,5,2,6,4)]

# Replace NA's with 0
likert.graph.data[is.na(likert.graph.data)] <- 0

# Create labels
mylevels<-c("considerably less", "slightly less","about the same","slightly more","considerably more")

## To handle the center category of ???about the same,??? this estimate is divided by two, and then include it twice. 
## That way,half of this category can be plotted below the center line and the other half above this line. The indvidual steps
## are described.

# Count number of levels (-1 removes the column 'condition' from the count)
numlevels<-length(likert.graph.data[1,])-1  #count on the first line

# Determine center of levels
numcenter<-ceiling(numlevels/2)+1

# Create column with center values divided by to
likert.graph.data$midvalues<-likert.graph.data[,numcenter]/2

tab2<-cbind(likert.graph.data[,1],likert.graph.data[,2:ceiling(numlevels/2)],
            likert.graph.data$midvalues,likert.graph.data$midvalues,likert.graph.data[,numcenter:numlevels+1])
colnames(tab2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                  "midhigh",mylevels[numcenter:numlevels])


## To avoid too much blank space, find the closest 25% break below the minimum value and above the maximum value. Then,
## pass these to ggplot???s limits option.

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100


# For applying colors, the data set has six columns of estimates, but only have five levels on the factor (from 'considerably less' to 'considerablly more'). 
# Thus, two palettes hava to ebe created: one to color the plot and one to make the legend. 

numlevels<-length(likert.graph.data[1,])-1
temp.rows<-length(tab2[,1])
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF" ## replace the default value with a darker grey 
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])


# Create new dataframe by reformating from wide to long
tab3<-melt(tab2,id="outcome")

# Manually assign colors using the above-created palette with length six
tab3$col<-rep(pal,each=temp.rows)

# Multiply everything by 100 to get into a percent rather than decimal format
tab3$value<-tab3$value*100

# Wrap long labels 
tab3$outcome<-str_wrap(tab3$outcome, width = 100)

# To order the plot from least to most 
tab3$outcome<-factor(tab3$outcome, levels = tab2$outcome[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])

# Split data frame into two equal halves: highs and lows
highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
lows<-na.omit(tab3[1:(length(tab3[,1])/2),])

# Reverse order of 'lows'
lows <- lows[rev(rownames(lows)),]

# Order the color of the 'lows' according to labels
lows$col <-factor(lows$col, levels = c("#CA0020", "#F4A582","#DFDFDF"))

# Create plot
likert.plot_sex <- ggplot() + geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + #choose theme
  coord_flip() + # flip coordinate system. Note: the y-axis will still be the y-axis for modifiction in ggplot
  labs(title='Change in Physical Activity \n by Sex', y="",x="") +   # add title to plot
  theme(plot.title = element_text(size=12, hjust=0.5)) + # change font size of title
  theme(axis.text.y = element_text(hjust=0), # change position of y axis text
        legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.text = element_text(size=10)) +   # add legend at the bottom of the graph
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) # customize the limits of the y-axis
likert.plot_sex

# Save plot
ggsave(
  "likert.plot.change.in.PA_sex.pdf",
  plot = likert.plot_sex,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Changes in physical activity stratified by mobility aid used ---------- 

# Calculate the proportions per mobility aid
covid19.survey.data.change.in.PA.mob.aid <- covid19.survey.data2 %>%
  group_by(Mobility_Aid, Change_in_PA) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  as.data.frame()
covid19.survey.data.change.in.PA.mob.aid

# Save Table
write.csv(covid19.survey.data.change.in.PA.mob.aid,"/Users/jutzca/Documents/Github/COVID-19_Excercise_Neurological_Conditions/Tables/Likert_scale_change_in_PA_mob.aid.csv", row.names = F)

# Reformat dataframe from long to wide
covid19.survey.data.change.in.PA.wide <-reshape(data=covid19.survey.data.change.in.PA.mob.aid, timevar = "Change_in_PA", idvar = "Mobility_Aid", direction = "wide")

# Subset data
covid19.survey.data.change.in.PA.wide_subset <-covid19.survey.data.change.in.PA.wide[, -c(2,4,6,8,10)]

# Reorder data to follow the levels: "considerably less", "slightly less", "about the same", "slightly more",  "considerably more"
likert.graph.data <- covid19.survey.data.change.in.PA.wide_subset[,c(1,3,5,2,6,4)]

# Replace NA's with 0
likert.graph.data[is.na(likert.graph.data)] <- 0

# Create labels
mylevels<-c("considerably less", "slightly less","about the same","slightly more","considerably more")

## To handle the center category of ???about the same,??? this estimate is divided by two, and then include it twice. 
## That way,half of this category can be plotted below the center line and the other half above this line. The indvidual steps
## are described.

# Count number of levels (-1 removes the column 'condition' from the count)
numlevels<-length(likert.graph.data[1,])-1  #count on the first line

# Determine center of levels
numcenter<-ceiling(numlevels/2)+1

# Create column with center values divided by to
likert.graph.data$midvalues<-likert.graph.data[,numcenter]/2

tab2<-cbind(likert.graph.data[,1],likert.graph.data[,2:ceiling(numlevels/2)],
            likert.graph.data$midvalues,likert.graph.data$midvalues,likert.graph.data[,numcenter:numlevels+1])
colnames(tab2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                  "midhigh",mylevels[numcenter:numlevels])


## To avoid too much blank space, find the closest 25% break below the minimum value and above the maximum value. Then,
## pass these to ggplot???s limits option.

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100


# For applying colors, the data set has six columns of estimates, but only have five levels on the factor (from 'considerably less' to 'considerablly more'). 
# Thus, two palettes hava to ebe created: one to color the plot and one to make the legend. 

numlevels<-length(likert.graph.data[1,])-1
temp.rows<-length(tab2[,1])
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF" ## replace the default value with a darker grey 
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])


# Create new dataframe by reformating from wide to long
tab3<-melt(tab2,id="outcome")

# Manually assign colors using the above-created palette with length six
tab3$col<-rep(pal,each=temp.rows)

# Multiply everything by 100 to get into a percent rather than decimal format
tab3$value<-tab3$value*100

# Wrap long labels 
tab3$outcome<-str_wrap(tab3$outcome, width = 100)

# To order the plot from least to most 
tab3$outcome<-factor(tab3$outcome, levels = tab2$outcome[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])

# Split data frame into two equal halves: highs and lows
highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
lows<-na.omit(tab3[1:(length(tab3[,1])/2),])

# Reverse order of 'lows'
lows <- lows[rev(rownames(lows)),]

# Order the color of the 'lows' according to labels
lows$col <-factor(lows$col, levels = c("#CA0020", "#F4A582","#DFDFDF"))

# Create plot
likert.plot_mobility_aid <- ggplot() + geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + #choose theme
  coord_flip() + # flip coordinate system. Note: the y-axis will still be the y-axis for modifiction in ggplot
  labs(title='Change in Physical Activity \n by Mobility Aid', y="",x="") +   # add title to plot
  theme(plot.title = element_text(size=12, hjust=0.5)) + # change font size of title
  theme(axis.text.y = element_text(hjust=0), # change position of y axis text
        legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.text = element_text(size=10)) +   # add legend at the bottom of the graph
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) # customize the limits of the y-axis
likert.plot_mobility_aid

# Save plot
ggsave(
  "likert.plot.change.in.PA_mobility_aid.pdf",
  plot = likert.plot_mobility_aid,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()











#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####








