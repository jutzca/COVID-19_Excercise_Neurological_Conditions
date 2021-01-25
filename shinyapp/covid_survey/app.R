## ---------------------------
##
## Script name: 1_shiny_web_application
##
## Purpose of script: Code underlying the shiny web application
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
## Notes: This app accompanies the publication of Nightingale et al, 2021 published in XX. [add link here]
##      
##   
#### ---------------------------

## set working directory

setwd("/Users/jutzelec/Documents/Github/COVID-19_Excercise_Neurological_Conditions/shinyapp/")  #replace with your working directory

## ---------------------------
## load up the packages we will need:  
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(DT)
library(shinyWidgets)
library(png)
library(plotly)
library(splitstackshape)
library(RColorBrewer)
library(stringr)
library(ggnetwork)
library(networkD3)
library(igraph)
library(intergraph)
library(sna)
library(shinyjs)


## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(shiny)){install.packages("shiny")}
#if(!require(shinydashboard)){install.packages("shinydashboard")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(stats)){install.packages("stats")}
#if(!require(ggthemes)){install.packages("ggthemes")}


#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set local system
Sys.setlocale('LC_ALL','C') 

#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Default

ui <- dashboardPage(

  #----Dashboard header----
  title = "PACorona: Data Visualization",
  dashboardHeader(title=span(icon("react"), "COVID-19 Physical Activity Survey"),
                  titleWidth = 350), #HTML(paste(icon("virus"), "PsyCorona Data Tool"))
 
   #Select 'skin' color: blue, black, green, purple, red, yellow
  skin = "purple",
  
  #----Dashboard sidebar----
  dashboardSidebar(width = 350,
    sidebarMenu(id = "sidebarMenu",
                menuItem("Cohort", tabName = "cohort", icon = icon("users")),
                #menuItem("Psychological Variables", tabName = "Variables", icon = icon("fas fa-pencil-ruler")),
                #menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line"), badgeLabel = "new", badgeColor = "blue"),
                #menuItem("Data", tabName = "data", icon = icon("fas fa-share-square")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
                #menuItem(HTML(paste0("Take the survey now ", icon("external-link"))), icon=icon("fas fa-file-signature"), href = "https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", newtab = T),
                #uiOutput("dynamic_content")
                ),
    
    shinyjs::useShinyjs(),
    tags$footer(HTML("<strong>Copyright &copy; 2020 <a href=\"Dr. Catherine Jutzeler\" target=\"_blank\">PhysActivitySurvey</a>.</strong> 
                     <br>This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
                     <br><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png\" /></a>
                     "), 
                #latest.DateTime,
                id = "sideFooter",
                align = "left",
                style = "
                position:absolute;
                bottom:0;
                width:100%;
                padding: 10px;
                "
    )
  ),
  
  #----Dashboard body----
  dashboardBody(
    
    
    shinyjs::useShinyjs(),
    tabItems(
    tabItem(tabName = "cohort",
            h3("Description of Cohort")
            ),
    
    
    tabItem(tabName = "about",
            h3("Welcome to the Physical Activity Project"),
            br(),
            fluidRow(
              box(#title = "Explore The Data", 
                width = 8, 
                heigth = "500px",
                solidHeader = TRUE,
                
                h4("Goal of the study"),
                "COVID-19 can result in severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) and has widespread effects on the body. Governments
                around the world have imposed strict restrictions in order to reduce the spread of the disease and flatten the curve to avoid overwhelming
                healthcare services. These restrictions include isolation and the practice of social distancing, which could unintentionally impact our exercise
                behaviours. Thus, the purpose of this study is to understand the effect of the COVID-19 pandemic on physical activity levels and health-related
                quality of life in adults with a neurologically-related mobility disability. We plan to capture changes over time as the UK Government alters their
                procedures to manage this pandemic. Information gathered will help us to better support individuals with physical disabilities, not only during future
                pandemics or serious global events, but during their everyday lives to engage in health promoting behaviours.",
                br(),
                br(),
                
                h4("Confidentiality/anonymity and data security"),
                "All of your personal identifying information will be pseudonymised using a keycode. Any data presented in reports will be anonymous, that is
                information used in any journal article or presentation will not allow identification to any specific individual.",
                br(),
                "Data will be stored securely on a password protected computer to ensure confidentiality. Only the research team who are directly involved in the
                study will have access to the data.",
                br(),
                "We will follow ethical and legal practices in accordance with the Data Protection Act (2018), and the General Data Practice Regulations (GDPR)
                (2018).",
                br(),
                br(),
                h4("Study team"),
                "Principal Investigator: Dr. Tom Nightingale, Lecturer in Exercise Physiology, School of Sport, Exercise and Rehabilitation Sciences,
                University of Birmingham ", 
                tags$a(href="mailto:T.E.Nightingale@bham.ac.uk", 
                                                   target="_blank",
                                                   icon("envelope")),
                ".",
                br(),
                "Co-investigators: Dr. Sally Fenton, Dr. Jet Veldhuijzen van Zaten & Dr. Nicola Heneghan, School of Sport, Exercise and
                Rehabilitation Sciences, University of Birmingham.",
                br(),
                br(),
                h4("Ethics statement"),
                "Ethical approval for this study has been given following review by the Science, Technology, Mathematics and Engineering (STEM)
                Ethical Review Committee at the University of Birmingham.
                If you have any questions or concerns regarding the study please do not hesitate to contact the Principal
                Investigator, Dr. Tom Nightingale",
                tags$a(href="mailto:T.E.Nightingale@bham.ac.uk", 
                       target="_blank",
                       icon("envelope")),
                ".",
                br(),
                br(),
                br(),
                "More information ",
                tags$a(href="https://www.birmingham.ac.uk/schools/sport-exercise/staff/profile.aspx?ReferenceId=178584&Name=dr-tom-e.-nightingale", 
                       target="_blank",
                       icon("id-card")),
                HTML("&nbsp"),
                tags$a(href="https://github.com/jutzca/COVID-19_Excercise_Neurological_Conditions", 
                       target="_blank", 
                       icon("github")),
                br(),
                br(),
              box(width = 4,
                  HTML("<a class=\"twitter-timeline\" data-height=\"600\" href=\"https://twitter.com/Tnightingale10\">A Twitter List by FortuneMagazine</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
              )
                  ),
            fluidRow(
              valueBox(prettyNum(200, big.mark=" ", scientific=FALSE), "Participants", icon = icon("user-edit"), width = 3, color = "purple"),
              valueBox(prettyNum(199, big.mark=" ", scientific=FALSE), "Total Survey Responses", icon = icon("edit"), width = 3,  color = "purple"),
              valueBox(prettyNum(7, big.mark=" ", scientific=FALSE), "Neurological Conditions", icon = icon("heartbeat"), width = 3,  color = "purple"),
              valueBox("XX", "Researchers", icon = icon("user-graduate"), width = 3,  color = "purple")#,
              #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
            )

    
            )
      )
    )
  )
)



server <- function(input, output) {
  output$cohort <- renderMenu({
    sidebarMenu(
      menuItem("Cohort description", icon = icon("users"))
    )
  })
  

}

shinyApp(ui, server)
