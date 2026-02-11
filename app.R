#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(bslib)
library(ICCbin)
library(statip)
library(geepack)
library(nlme)
library(lme4)
library(ggplot2)
library(sjstats)
library(tidyverse)
library(stringr)
library(R2jags)
library(bayesmeta)
library(fishmethods)
library(performance)
library(reshape2)
library(readxl)
library(plotly)
library(ggpp)
library(gridExtra)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(iccCounts)
library(TMB)
library(remotes)
library(glmmTMB)
library(GLMMadaptive)
library(insight)
library(DT)
library(data.table)
library(RSQLite)
library(writexl)
library(RMySQL)
library(ordinal)
library(survival)
library(RColorBrewer)
library(redcapAPI) 


####CODE for the ICC database in the app######################################
#Connect to redcap database and display in shiny app

load_icc_database <- function() {
  
  if (nzchar(Sys.getenv("REDCAP_API")) && nzchar(Sys.getenv("REDCAP_URL"))) {
    
    message("Using LIVE REDCap database")
    
    rcon <- redcapConnection(
      url   = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_API")
    )
    
    data <- exportRecords(rcon)
    attr(data, "is_dummy") <- FALSE
    return(data)
  }
  
  message("Using DUMMY database")
  
  data <- read_csv("TheICCDatabaseTEST_DATA_2025-06-26_1053.csv")
  attr(data, "is_dummy") <- TRUE
  return(data)
}

# ui
ui<-navbarPage(
  id = 'navbarID', useShinyjs(),
  tags$style(
    '.box:hover {
    cursor: pointer;
       background-color: lightblue;
    }'),
  theme=shinytheme("flatly"),title = "The ICC Shiny app",
  # Welcome tab
  tabPanel("Welcome page",
           fluidPage(
             br(),
             
             # Title
             h1("Welcome to the ICC Shiny App!"),
             
             # Intro text
             p("This app helps you estimate, understand, and explore 
                intra-cluster correlation coefficients (ICCs) for your studies. It can also assist you in choosing an ICC value 
for the sample size calculation of a Cluster Randomised Trial and access an open source database of ICC estimates added by users. You can click on the links below to get a first understanding of what is the ICC in lay terms and learn more on how to use this Shiny app."),
             br(),
             
             # Video links
             tags$a(href = "https://www.youtube.com/watch?v=FWWQ_tW4iuQ", target = "_blank", "Learn more about the ICC"),
             br(),
             tags$a(href = "https://www.youtube.com/watch?v=gee7zBjL4Ss", target = "_blank", "ICC Shiny app tutorial"),
             br(), br(),
             
             # Contact
             p("Contact: ",
               tags$a(href = "mailto:olivierquintinpro@gmail.com", "olivierquintinpro@gmail.com")),
             # Logo 
             img(src = "logo.jpg", height = "200px", style = "float: right;"),
             br(), br()
           )
  ),
  tabPanel("Previous estimates",fluidPage(em("Derive a posterior distribution from relevant ICC estimates selected on sources."),br(),
                                          h5("You can download and complete the template before loading the file. Please see the example in the second sheet of the template.",a("For more information on the models, check the article of Turner et. al 2005", href = "https://doi.org/10.1191/1740774505cn072oa",
                                                                                                                                                                                 target = "_blank")),
                                          downloadButton("downloadOP", label = "Download template"),
                                          tags$hr(),
                                          sidebarLayout(
                                            sidebarPanel(
                                              fileInput('file3', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                              actionButton("Load3", "Load the File"),width = 3),
                                            tags$hr()
                                          )),
           mainPanel(width = 9,
                     uiOutput("tb3"),
                     tags$hr()
           )),
  tabPanel(title = "Uncertainty",
           fluidPage(em("Incorporate uncertainty in your single ICC estimate."),br(),
                     h5(a("For more information on the methods, check the article of Ukoumunne et. al 2002", href = "https://doi.org/10.1002/sim.1330",
                          target = "_blank")),br(),
                     # Sidebar with numeric input
                     sidebarLayout(
                       sidebarPanel(
                         numericInput('ICC', 'ICC estimate',value=0,min=0,max=1),
                         numericInput('N', 'Total number of individual',value=0,min=2,step=1),
                         numericInput('K', 'Number of cluster',value=0,min=2,step=1)
                       ),
                       
                       # Show results of successive calculations
                       conditionalPanel(condition = c("input.ICC != '0' && input.N != '0' && input.K != '0'"),
                                        plotOutput("Uncertainty_plot"),
                                        tags$hr()
                       )
                     ))),
  tabPanel("Data",fluidPage(em("Estimate the ICC with your own dataset by answering the questions below..."),
                            p("Please note that in longitudinal CRT designs (e.g., stepped-wedge or crossover), the correlation structure can be complex and may vary over time. We therefore recommend importing data from a single time period only, as combining multiple periods may lead to unreliable ICC estimates."),
                            a("See article for more information on ICC estimates for longitudinal designs (incuding an R Shiny app)", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC10555741/", target = "_blank"),br(),br(),
                            
                            radioButtons("format", "What type is your file?", choices= c("Excel"="xl", "Other type (csv or txt)"="other"),selected = character(0)),
                            conditionalPanel(
                              condition = "input.format !== null",
                              radioButtons("type", "What type of data?",
                                           c("Continuous outcome" = "C","Binary outcome" = "Bin","Categorical outcome" = "Cat",
                                             "Count outcome" = "Count","Time to event" = "TTE","Proportions" = "Prop"),selected = character(0))),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'Prop'",
                              h5("Your proportions must be disposed in a vertical manner with each row corresponding to the proportion of the outcome (or prevalence) in one cluster. See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file11', 'Choose file to upload'),
                                  helpText("If the first row in your file is a variable name, please tick the Header box before loading."),
                                  checkboxInput('header11', 'Header', F),
                                  actionButton("Load11", "Load the File"),width = 3),
                                tags$hr()
                                
                              )),conditionalPanel(
                                condition = "input.format == 'xl' && input.type == 'Prop'",
                                mainPanel(width = 12,
                                          uiOutput("tb1"),
                                          tags$hr()
                                )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Prop'",
                              h5("Your proportions must be disposed in a vertical manner with each row corresponding to the proportion of the outcome (or prevalence) in one cluster. See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file1', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  helpText("If the first row in your file is a variable name, please tick the Header box before loading."),
                                  checkboxInput('header1', 'Header', F),
                                  actionButton("Load1", "Load the File"),width = 3),
                                tags$hr()
                                
                              )),conditionalPanel(
                                condition = "input.format == 'other' && input.type == 'Prop'",
                                mainPanel(width = 12,
                                          uiOutput("tb"),
                                          tags$hr()
                                )),
                            conditionalPanel(
                              condition = "input.type == 'C'|| input.type == 'Bin'|| input.type == 'Cat'|| input.type == 'Count'|| input.type == 'TTE'",
                              radioButtons("intervention", "All data in same condition?",
                                           c("Yes" = "Yes",
                                             "No" = "No"),selected = character(0))),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Bin' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with 0 or 1 values (missing values are also accepted). See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("See ICCbin package article for more information on ICC estimates", href = "https://doi.org/10.1016/j.cmpb.2017.10.023",
                                                                                                                                                                                                                                                                                                                                                            target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file2', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep2", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header2', 'Header', F),
                                  actionButton("Load2", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Bin' && input.intervention == 'Yes'",
                              mainPanel(width = 12,
                                        uiOutput("tb2"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'Bin' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with 0 or 1 values (missing values are also accepted). See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("See ICCbin package article for more information on ICC estimates", href = "https://doi.org/10.1016/j.cmpb.2017.10.023",
                                                                                                                                                                                                                                                                                                                                                            target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file22', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header22', 'Header', F),
                                  actionButton("Load22", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'Bin' && input.intervention == 'Yes'",
                              mainPanel(width = 12,
                                        uiOutput("tb22"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'C' && input.intervention == 'Yes'"),
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with continuous values (missing values are also accepted). See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file4', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep4", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header4', 'Header', F),
                                  actionButton("Load4", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'C' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb4"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'C' && input.intervention == 'Yes'"),
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with continuous values (missing values are also accepted). See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file44', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header44', 'Header', F),
                                  actionButton("Load44", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'C' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb44"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'C' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second one the outcome with continuous values (missing values are also accepted) and the third the condition status. See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file5', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep5", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header5', 'Header', F),
                                  actionButton("Load5", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'C' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb5"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'C' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second one the outcome with continuous values (missing values are also accepted) and the third the condition status. See image below for example."),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file55', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header55', 'Header', F),
                                  actionButton("Load55", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'C' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb55"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Bin' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second one the outcome with 0 or 1 values (missing values are also accepted) and the third the condition status. See image below for example.",a("See article for more information on ICC estimates", href = "https://doi.org/10.1177/1740774510392256",
                                                                                                                                                                                                                                                                                                                    target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file6', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep6", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header6', 'Header', F),
                                  actionButton("Load6", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Bin' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb6"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Bin' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second one the outcome with 0 or 1 values (missing values are also accepted) and the third the condition status. See image below for example.",a("See article for more information on ICC estimates", href = "https://doi.org/10.1177/1740774510392256", target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file66', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header66', 'Header', F),
                                  actionButton("Load66", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Bin' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb66"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Count' && input.intervention == 'Yes'"),
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with count values (missing values are also accepted). See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("Check Carrasco et al. 2022", href = "https://doi.org/10.32614/rj-2022-034",
                                                                                                                                                                                                                                                                                                                                                           target = "_blank"), a("AND Nakagawa et al. 2017 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5636267/",
                                                                                                                                                                                                                                                                                                                                                                                 target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file7', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep7", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header7', 'Header', F),
                                  actionButton("Load7", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Count' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb7"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Count' && input.intervention == 'Yes'"),
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second one the outcome with count values (missing values are also accepted). See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("Check Carrasco et al. 2022", href = "https://doi.org/10.32614/rj-2022-034",
                                                                                                                                                                                                                                                                                                                                                           target = "_blank"), a("AND Nakagawa et al. 2017 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5636267/",
                                                                                                                                                                                                                                                                                                                                                                                 target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file77', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header77', 'Header', F),
                                  actionButton("Load77", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Count' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb77"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Count' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers,the second one the outcome with count values (missing values are also accepted) and the condition status in the third. See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("Check Carrasco et al. 2022", href = "https://doi.org/10.32614/rj-2022-034",
                                                                                                                                                                                                                                                                                                                                                                                               target = "_blank"), a("AND Nakagawa et al. 2017 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5636267/",
                                                                                                                                                                                                                                                                                                                                                                                                                     target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file8', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep8", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header8', 'Header', F),
                                  actionButton("Load8", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'other' && input.type == 'Count' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb8"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Count' && input.intervention == 'No'"),
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers,the second one the outcome with count values (missing values are also accepted) and the condition status in the third. See image below for example. Please wait a moment after loading your file, it will only take a minute.",a("Check Carrasco et al. 2022", href = "https://doi.org/10.32614/rj-2022-034",
                                                                                                                                                                                                                                                                                                                                                                                               target = "_blank"), a("AND Nakagawa et al. 2017 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5636267/",
                                                                                                                                                                                                                                                                                                                                                                                                                     target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file88', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header88', 'Header', F),
                                  actionButton("Load88", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Count' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb88"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Cat' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second the outcome with the different categories (missing values are also accepted). See image below for example. ",a("Check Chakrabortya et al. 2021 for more information", href = "https://doi.org/10.1080/03610926.2021.1914660",
                                                                                                                                                                                                                                                                                          target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file9', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep9", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header9', 'Header', F),
                                  actionButton("Load9", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Cat' && input.intervention == 'Yes'",
                              mainPanel(width = 12,
                                        uiOutput("tb9"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'Cat' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with two columns. The first column should contain the cluster identifiers and the second the outcome with the different categories (missing values are also accepted). See image below for example. ",a("Check Chakrabortya et al. 2021 for more information", href = "https://doi.org/10.1080/03610926.2021.1914660",
                                                                                                                                                                                                                                                                                          target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file99', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header99', 'Header', F),
                                  actionButton("Load99", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Cat' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb99"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Cat' && input.intervention == 'No'",
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second the outcome with the different categories (missing values are also accepted) and the third the condition status. See image below for example. ",a("Check Langworthy et al. 2023 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC11164036/",
                                                                                                                                                                                                                                                                                                                            target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file10', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep10", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header10', 'Header', F),
                                  actionButton("Load10", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'Cat' && input.intervention == 'No'",
                              mainPanel(width = 12,
                                        uiOutput("tb10"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'Cat' && input.intervention == 'No'",
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second the outcome with the different categories (missing values are also accepted) and the third the condition status. See image below for example. ",a("Check Langworthy et al. 2023 for more information", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC11164036/",
                                                                                                                                                                                                                                                                                                                            target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file1010', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header1010', 'Header', F),
                                  actionButton("Load1010", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'Cat' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb1010"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'TTE' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second the time to event and the third the censoring indicator (missing values are also accepted). See image below for example.",a("Check Kalia et al. 2016 for more information", href = " https://doi.org/10.1002/sim.7145",
                                                                                                                                                                                                                                                                                                      target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file11b', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep11b", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header11b', 'Header', F),
                                  actionButton("Load11b", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'TTE' && input.intervention == 'Yes'",
                              mainPanel(width = 12,
                                        uiOutput("tb11b"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'TTE' && input.intervention == 'Yes'",
                              h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second the time to event and the third the censoring indicator (missing values are also accepted). See image below for example.",a("Check Kalia et al. 2016 for more information", href = " https://doi.org/10.1002/sim.7145",
                                                                                                                                                                                                                                                                                                      target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file1111', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header1111', 'Header', F),
                                  actionButton("Load1111", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'TTE' && input.intervention == 'Yes'"),
                              mainPanel(width = 12,
                                        uiOutput("tb1111"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'TTE' && input.intervention == 'No'",
                              h5("Your data must be disposed in a vertical manner with four columns. The first column should contain the cluster identifiers, the second the time to event (missing values are also accepted), the third the censoring indicator and the fourth the condition status. See image below for example.",a("Check Jahn-Eimermacher et al. 2012 for more information", href = " https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.5548",
                                                                                                                                                                                                                                                                                                                                      target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file12', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
                                  radioButtons("sep12", "Choose the separator in your file", choices= c("Comma (,)"=",", "Semi-colon (;)"=";", "Whitespace ( )"=" ")),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header12', 'Header', F),
                                  actionButton("Load12", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'other' && input.type == 'TTE' && input.intervention == 'No'",
                              mainPanel(width = 12,
                                        uiOutput("tb12"),
                                        tags$hr()
                              )),
                            conditionalPanel(
                              condition = "input.format == 'xl' && input.type == 'TTE' && input.intervention == 'No'",
                              h5("Your data must be disposed in a vertical manner with four columns. The first column should contain the cluster identifiers, the second the time to event (missing values are also accepted), the third the censoring indicator and the fourth the condition status. See image below for example.",a("Check Jahn-Eimermacher et al. 2012 for more information", href = " https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.5548",
                                                                                                                                                                                                                                                                                                                                      target = "_blank")),
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput('file1212', 'Choose file to upload'),
                                  helpText("If the first rows in your file are variable names, please tick the Header box before loading."),
                                  checkboxInput('header1212', 'Header', F),
                                  actionButton("Load1212", "Load the File"),width = 3),
                                tags$hr()
                              )),
                            conditionalPanel(
                              condition = c("input.format == 'xl' && input.type == 'TTE' && input.intervention == 'No'"),
                              mainPanel(width = 12,
                                        uiOutput("tb1212"),
                                        tags$hr()
                              ))
                            
  )),tabPanel("The ICC database",fluidPage(em("This is a database of ICC estimates added by users. You can add your own estimate by clicking on 'Add ICC estimate'. If your data come from a longitudinal design (e.g. stepped wedge), please only report within-period ICCs unless an exchangeable correlation structure is assumed. Thank you!"),br(),br(),
                                         tags$a(
                                           href = "https://trials2.pctu.qmul.ac.uk/surveys/?s=JLFXPRK74DT3RNJW",
                                           target = "_blank",   # opens in new tab
                                           class = "btn btn-default",  # Bootstrap button styling
                                           "Add ICC estimate"
                                         ),tags$hr(),
                                         uiOutput("dummy_message"),
                                         DTOutput("redcap_table"), tags$hr())
  ))


server = function(input, output,session) {
  
  # Connect to REDCap
  redcap_data <- reactive({
    load_icc_database()
  })
  
  output$dummy_message <- renderUI({
    data <- redcap_data()
    if (isTRUE(attr(data, "is_dummy"))) {
      tags$div(
        style = "background-color:#fff3cd; padding:12px; border-radius:6px; margin-bottom:15px;",
        tags$b("This is a dummy database. "),
        "To visualise the real ICC database, please use this link: ",
        tags$a(
          href = "https://olivierquintin.shinyapps.io/The_ICC_Shiny_app/",
          "https://olivierquintin.shinyapps.io/The_ICC_Shiny_app/",
          target = "_blank"
        )
      )
    }
  })
  
  
  # Render data table
  output$redcap_table <- renderDT({
    
    data <- redcap_data()
    is_dummy_db <- attr(data, "is_dummy")
    
    subset_data <- data[, setdiff(names(data), 
                                  c("record_id", "redcap_survey_identifier", "the_icc_form_timestamp",
                                    "outcome_description","outcome_description_text",
                                    "the_icc_form_complete"))]
    
    colnames(subset_data) <- c("ICC value", "Estimation method", "N individuals",
                               "Individual level", "N clusters", "Cluster level",
                               "Coefficient of variation of cluster sizes", "Outcome description",
                               "Primary outcome (Yes/No)", "Outcome type", "Other outcome type",
                               "Study ID", "Study design", "Other study design",
                               "Publication link/doi/PMID")
    
    datatable(subset_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  #####################Uncertainty
  rv <- reactive({
    tibble::tibble(ICC = input$ICC,N = input$N,K = input$K, 
                   #Swiger
                   Var1= (2*(N-1)*(1-ICC)^2*(1+((N/K)-1)*ICC)^2)/((N/K)^2*(N-K)*(K-1)),
                   lower1=ICC-1.96*sqrt(Var1),
                   upper1=ICC+1.96*sqrt(Var1),
                   #Fisher's tranformation
                   ZF=(1/2)*log((1+((N/K)-1)*ICC)/1-ICC),
                   VZ=(1/2)*((K-1)^(-1)+(N-K)^(-1)),
                   lowerF=ZF-1.96*sqrt(VZ),
                   upperF=ZF+1.96*sqrt(VZ),
                   lower2=(exp(2*ZF-1.96*sqrt(VZ))-1)/((N/K)-1+(exp(2*ZF-1.96*sqrt(VZ)))),
                   upper2=(exp(2*ZF+1.96*sqrt(VZ))-1)/((N/K)-1+(exp(2*ZF+1.96*sqrt(VZ)))),
                   #Searle's method
                   FS=(ICC*((N/K)-1)+1)/(1-ICC),
                   lower3=((FS/qf(.95, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.95, df1=K-1, df2=N-1)-1))),
                   Q13=((FS/qf(.75, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.75, df1=K-1, df2=N-1)-1))),
                   Median3=((FS/qf(.5, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.5, df1=K-1, df2=N-1)-1))),
                   Q33=((FS/qf(.25, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.25, df1=K-1, df2=N-1)-1))),
                   upper3=((FS/qf(.05, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.05, df1=K-1, df2=N-1)-1)))
    )
    
  })
  
  output$Uncertainty_plot<- renderPlot({
    Swiger<-ggplot() + 
      stat_function(fun = function(x){qnorm(x,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96)},linewidth=1.5,colour="blue") +
      theme_bw() +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower1,3),";",round(rv()$upper1,3),"]","\n",
                                                                              "Q1=",round(qnorm(0.25,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3),"\n",
                                                                              "Median=",round(qnorm(0.5,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3),"\n",
                                                                              "Q3=",round(qnorm(0.75,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3))))+
      ggtitle("Swiger's method")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    Fisher<-ggplot() + 
      stat_function(fun = function(x){qnorm(x,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96)},linewidth=1.5,colour="green") +
      theme_bw() +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower2,3),";",round(rv()$upper2,3),"]","\n",
                                                                              "Q1=",round(qnorm(0.25,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3),"\n",
                                                                              "Median=",round(qnorm(0.5,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3),"\n",
                                                                              "Q3=",round(qnorm(0.75,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3))))+
      ggtitle("Fisher's transformation")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    Searle<-ggplot() + 
      stat_function(fun = function(x){(rv()$FS/qf(1-x, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-x, df1=rv()$K-1, df2=rv()$N-1)-1))},size=1.5,colour="red") +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      theme_bw() +
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower3,3),";",round(rv()$upper3,3),"]","\n",
                                                                              "Q1=",round((rv()$FS/qf(1-0.25, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.25, df1=rv()$K-1, df2=rv()$N-1)-1)),3),"\n",
                                                                              "Median=",round((rv()$FS/qf(1-0.5, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.5, df1=rv()$K-1, df2=rv()$N-1)-1)),3),"\n",
                                                                              "Q3=",round((rv()$FS/qf(1-0.75, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.75, df1=rv()$K-1, df2=rv()$N-1)-1)),3))))+
      ggtitle("Searle's method")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    grid.arrange(Swiger, Fisher,Searle, ncol = 3)
  })
  
  
  
  ###Template to download
  output$downloadOP <- downloadHandler(
    filename = "Previous ICC templates.xlsx",
    content = function(file) {
      file.copy("Previous_estimates_template.xlsx", file)
    }
  )
  
  
  ################
  ##################Bayesian Approach
  
  data3 <- reactive({
    if(input$Load3 == 0){return()}
    inFile <- input$file3
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load3
      my_data3 <- read_excel(inFile$datapath)
      
      my_data3$`Outcome ID` <-as.factor(my_data3$`Outcome ID`)
      my_data3$`Study ID` <-as.factor(my_data3$`Study ID`)
      my_data3$`Outcome weight num` <- ifelse(my_data3$`Outcome weight` =="Same outcome", 1, 
                                              ifelse(my_data3$`Outcome weight` =="Similar outcome", 0.7,
                                                     ifelse(my_data3$`Outcome weight` =="Outcome somewhat similar", 0.5, 0.2)))
      my_data3$`Study weight num` <- ifelse(my_data3$`Study weight` =="Very similar study", 1, 
                                            ifelse(my_data3$`Study weight` =="Similar study", 0.7,
                                                   ifelse(my_data3$`Study weight` =="Study somewhat similar", 0.5, 0.2)))
      
      my_data3$ICC <- as.numeric(my_data3$ICC)
      my_data3$N <- as.numeric(my_data3$N)
      my_data3$K <- as.numeric(my_data3$K)
      my_data3$`Outcome weight num` <- as.numeric(my_data3$`Outcome weight num`)
      my_data3$`Study weight num` <- as.numeric(my_data3$`Study weight num`)
      
      my_data3$Var <- (2*(my_data3$N-1)*(1-my_data3$ICC)^(2)*(1+((my_data3$N/my_data3$K)-1)*my_data3$ICC)^(2))/((my_data3$N/my_data3$K)^(2)*(my_data3$N-my_data3$K)*(my_data3$K-1))
      my_data3$Precision <- 1/my_data3$Var
      
      
      datax1 <- list(N=length(my_data3$ICC), ICC=my_data3$ICC, Precision=my_data3$Precision)
      
      model1 <- function() {
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu,Precision2)
        }
        
        # priors for the parameters
        mu~dnorm(0,0.001)
        sigma~dunif(0,5)
        Precision2<-pow(sigma,-2)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu=-2.95)
      init2 <-list(mu=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      model_1 <- jags(data=datax1,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=model1,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med1 <- exp(model_1$BUGSoutput[13]$median$mu)/(1+exp(model_1$BUGSoutput[13]$median$mu))
      Inf1 <- exp(model_1$BUGSoutput$summary[2,3])/(1+exp(model_1$BUGSoutput$summary[2,3]))
      Sup1 <- exp(model_1$BUGSoutput$summary[2,7])/(1+exp(model_1$BUGSoutput$summary[2,7]))
      
      
      res <- as.mcmc(model_1)
      res <- rbind(res[[1]],res[[2]])
      ICC <- exp(res[,"mu"])/(1+exp(res[,"mu"]))   
      
      p1<- ggplot() +
        geom_histogram(aes(x=ICC),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf1,"\n","Median =",Med1,"\n","97.5% =",Sup1)))+
        ggtitle("Model 1 (Regardless of outcome and study type)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      
      datax2 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`))
      
      Model2 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2)
          mu[i]<- a[s[i]] 
        }
        
        # priors for the parameters
        Precision2<-pow(sigma,-2)
        sigma~dunif(0,5)
        
        #likelihood 2
        for (j in 1:S)
        {
          a[j]~dnorm(mu.a,tau.a)
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a <- pow(sigma.a,-2)
        sigma.a~dunif(0,5)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_2 <- jags(data=datax2,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model2,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med2 <- exp(Model_2$BUGSoutput[13]$median$mu)/(1+exp(Model_2$BUGSoutput[13]$median$mu))
      Inf2 <- exp(Model_2$BUGSoutput$summary[2,3])/(1+exp(Model_2$BUGSoutput$summary[2,3]))
      Sup2 <- exp(Model_2$BUGSoutput$summary[2,7])/(1+exp(Model_2$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_2)
      res <- rbind(res[[1]],res[[2]])
      
      ICC2 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p2<-ggplot() +
        geom_histogram(aes(x=ICC2),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf2,"\n","Median =",Med2,"\n","97.5% =",Sup2)))+
        ggtitle("Model 2 (Exchangeability between and within studies)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      datax3 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`),wl=my_data3$`Outcome weight num`,ws=my_data3$`Study weight num`)
      
      Model3 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2[i]*wl[i])
          mu[i]<- a[s[i]] 
          
          # priors for the parameters
          Precision2[i]<-pow(sigma[i],-2)
          sigma[i]~dunif(0,5)
        }
        
        #likelihood 2
        for (j in 1:S)
        {
          a[j]~dnorm(mu.a,tau.a*ws[j])
          
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a<-pow(sigma.a,-2)
        sigma.a~dunif(0,5)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_3 <- jags(data=datax3,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model3,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med3 <- exp(Model_3$BUGSoutput[13]$median$mu)/(1+exp(Model_3$BUGSoutput[13]$median$mu))
      Inf3 <- exp(Model_3$BUGSoutput$summary[2,3])/(1+exp(Model_3$BUGSoutput$summary[2,3]))
      Sup3 <- exp(Model_3$BUGSoutput$summary[2,7])/(1+exp(Model_3$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_3)
      res <- rbind(res[[1]],res[[2]])
      
      ICC3 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p3<-ggplot() +
        geom_histogram(aes(x=ICC3),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf3,"\n","Median =",Med3,"\n","97.5% =",Sup3)))+
        ggtitle("Model 3 (Multiplicative weighting)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      datax4 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`),wl=my_data3$`Outcome weight num`,ws=my_data3$`Study weight num`)
      
      Model4 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2[i]*wl[i]/(wl[i]+lambda.l[i]*(1-wl[i])))
          mu[i]<- a[s[i]] 
          
          # priors for the parameters
          Precision2[i]<-pow(sigma[i],-2)
          sigma[i]~dunif(0,5)
          log.l[i]~dnorm(0,1)
          lambda.l[i]<- exp(log.l[i])
          
        }
        
        #likelihood 2
        for (j in 1:S)
        {
          
          a[j]~dnorm(mu.a,tau.a*ws[j]/(ws[j]+lambda.s*(1-ws[j])))
          
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a<-pow(sigma.a,-2)
        sigma.a~dunif(0,5)
        log.s~dnorm(0,1)
        lambda.s<- exp(log.s)
        
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_4 <- jags(data=datax4,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model4,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med4 <- exp(Model_4$BUGSoutput[13]$median$mu)/(1+exp(Model_4$BUGSoutput[13]$median$mu))
      Inf4 <- exp(Model_4$BUGSoutput$summary[2,3])/(1+exp(Model_4$BUGSoutput$summary[2,3]))
      Sup4 <- exp(Model_4$BUGSoutput$summary[2,7])/(1+exp(Model_4$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_4)
      res <- rbind(res[[1]],res[[2]])
      
      ICC4 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p4<-ggplot() +
        geom_histogram(aes(x=ICC4),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf4,"\n","Median =",Med4,"\n","97.5% =",Sup4)))+
        ggtitle("Model 4 (Additive weighting and uncertainty)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      output$plotM1_4 <- renderPlot({
        
        grid.arrange(p1, p2,p3, p4, ncol = 2)
      })
      
      output$plotM11_44 <- renderPlotly({
        
        p11 <- ggplotly(ggplot() +
                          geom_line(aes(ICC),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 1")+
                          theme_bw()+
                          ggtitle("Model 1 (Regardless of outcome and study type)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p22 <- ggplotly(ggplot() +
                          geom_line(aes(ICC2),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 2")+
                          theme_bw()+
                          ggtitle("Model 2 (Exchangeability between and within studies)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p33 <- ggplotly(ggplot() +
                          geom_line(aes(ICC3),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 3")+
                          theme_bw()+
                          ggtitle("Model 3 (Multiplicative weighting)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p44 <- ggplotly(ggplot() +
                          geom_line(aes(ICC4),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 4")+
                          theme_bw()+
                          ggtitle("Model 4 (Additive weighting and uncertainty)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        subplot(p11,p22,p33,p44,nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.1)%>% 
          layout(title = 'Empirical cumulative distribution (ecdf)')
        
      })
      
    })
    my_data3
  })
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE, colnames =T)
  
  
  #11) Bayesian
  output$tb3 <- renderUI({
    if (is.null(data3()))
      h5("Please, wait for a  moment after loading your file, it will only take a minute.")
    else
      tabsetPanel(tabPanel("Results", plotOutput("plotM1_4"), plotlyOutput("plotM11_44")),tabPanel("Data", tableOutput("my_output_data3")))
  })
  
  
  ##########################Data tab
  ##########################/proportions    
  #/Other format
  
  data1 <- reactive({
    if(input$Load1 == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load1
      my_data <- read.table(inFile$datapath, header = input$header1)
      colnames(my_data) <-c("Proportion")
      v=(mean(my_data$Proportion)*(1-mean(my_data$Proportion)))/var(my_data$Proportion)-1
      a=mean(my_data$Proportion)*v
      b=(1-mean(my_data$Proportion))*v
      ICC=round(1/(a+b+1),3)
      ICCl=round(var(log(my_data$Proportion/(1-my_data$Proportion)))/(var(log(my_data$Proportion/(1-my_data$Proportion)))+(pi^2/3)),3)
      p = seq(0,1, length=1000)
      x<-my_data$Proportion
      expr <- vector("expression", 4)
      expr[[1]] <- bquote(a==.(a))
      expr[[2]] <- bquote(b==.(b))
      expr[[3]] <- bquote(mean==.(mean(my_data$Proportion)))
      expr[[4]] <- bquote(var==.(var(my_data$Proportion)))
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("ICC estimate (proportion scale):", (ICC))})
      ICC2 <- renderText({paste("ICC estimate (log-odds scale):", (ICCl))})
      
      output$PrintICC <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      
      output$plot1 <- renderPlot({
        plot(p, dbeta(p, a, b), xlab='Proportion', ylab='Density', type ='l', lwd=2.0,main="Theoritical beta-binomial distribution")
        legend("topright",legend=expr,.7, 4)
      })
      
      output$plot2 <- renderPlot({
        hist(x, main="Histogram of proportions", xlab='Actual proportion',col="#99CCFF",xlim=c(0,1))
      })
      
    })
    my_data
  })
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  #Excel format
  
  data11 <- reactive({
    if(input$Load11 == 0){return()}
    inFile <- input$file11
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load11
      my_data <- read_excel(inFile$datapath, col_names = input$header11)
      colnames(my_data) <-c("Proportion")
      my_data$Proportion<-as.numeric(my_data$Proportion)
      v=(mean(my_data$Proportion)*(1-mean(my_data$Proportion)))/var(my_data$Proportion)-1
      a=mean(my_data$Proportion)*v
      b=(1-mean(my_data$Proportion))*v
      ICC=round(1/(a+b+1),3)
      ICCl=round(var(log(my_data$Proportion/(1-my_data$Proportion)))/(var(log(my_data$Proportion/(1-my_data$Proportion)))+(pi^2/3)),3)
      p = seq(0,1, length=1000)
      x<-my_data$Proportion
      expr <- vector("expression", 4)
      expr[[1]] <- bquote(a==.(a))
      expr[[2]] <- bquote(b==.(b))
      expr[[3]] <- bquote(mean==.(mean(my_data$Proportion)))
      expr[[4]] <- bquote(var==.(var(my_data$Proportion)))
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("ICC estimate (proportion scale):", (ICC))})
      ICC2 <- renderText({paste("ICC estimate (log-odds scale):", (ICCl))})
      
      output$PrintICC11 <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$plot11 <- renderPlot({
        plot(p, dbeta(p, a, b), xlab='Proportion', ylab='Density', type ='l', lwd=2.0,main="Theoritical beta-binomial distribution")
        legend("topright",legend=expr,.7, 4)
      })
      
      output$plot21 <- renderPlot({
        hist(x, main="Histogram of proportions", xlab='Actual proportion',col="#99CCFF",xlim=c(0,1))
      })
      
    })
    my_data
  })
  
  output$my_output_data11 <- renderTable({data11()},include.rownames=FALSE)
  
  #1) Proportions csv
  output$tb <- renderUI({
    if (!is.null(data1())) 
      tabsetPanel(tabPanel("Results",tableOutput("PrintICC"),plotOutput("plot2"), plotOutput("plot1")),tabPanel("Data", tableOutput("my_output_data"))) 
    else tags$img(src="proportions xlsx.png")
  })
  #2) Proportions excel 
  output$tb1 <- renderUI({
    if (!is.null(data11())) 
      tabsetPanel(tabPanel("Results", tableOutput("PrintICC11"),plotOutput("plot21"), plotOutput("plot11")),tabPanel("Data", tableOutput("my_output_data11"))) 
    else tags$img(src="proportions xlsx.png")
  })
  
  
  ########Binary
  ######/Same condition
  ######/csv
  
  data2 <- reactive({
    if(input$Load2 == 0){return()}
    inFile <- input$file2
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load2
      my_data2 <- read.table(inFile$datapath, header = input$header2, sep = input$sep2)
      
      colnames(my_data2)[1] <- "Cluster" 
      colnames(my_data2)[2] <- "Outcome" 
      
      my_data2$Outcome <- as.numeric(my_data2$Outcome)
      my_data2$Cluster <- factor(my_data2$Cluster,levels=unique(my_data2$Cluster))
      
      my_data2b<-na.omit(my_data2)
      
      colnames(my_data2b)[1] <- "cid" 
      colnames(my_data2b)[2] <- "y"     
      
      # Cluster ID
      cid <- my_data2b$cid
      # Response variable
      y <- my_data2b$y
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      # ::: ANOVA method :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: Modified ANOVA (Fleiss, 1981) :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msbs <- (1/(k))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aovs <- (msbs - msw)/(msbs + (n0 - 1)*msw)
      
      # ::: Moment estimators :::
      pii <- yi/ni
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.keq <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.kpr <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.keqs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.kprs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      kappa = 0.45
      p <- sum(yi)/sum(ni)
      wi <- ni/N
      sw <- sum(wi*(pii - piw)^2)
      rho.stab <- (1/(n0 - 1))*((N*sw)/((k - 1)*p*(1 - p)) + kappa - 1)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.ub <- 1 - (N*n0*(k - 1)*msw)/(sum(yi)*(n0*(k - 1) - sum(yi)) + sum(yisq))
      
      # ::: Estimators based on a direct probabilistic method :::
      # Fleiss and Cuzick (1979) Method 
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      piio <- sum(yi)/sum(ni)
      rho.fc <- 1 - (1/((N - k)*piio*(1 - piio)))*sum(yi*(ni - yi)/ni)
      
      # Mak (1988) method
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      ni <- as.vector(table(cid))
      rho.mak <- 1 - (k - 1)*sum((yi*(ni - yi))/(ni*(ni - 1)))/(sum(yisq/ni^2) + sum(yi/ni)*(k - 1 - sum(yi/ni)))
      
      # ::: Estimators based on direct calculation of correlation within each group :::
      # wi = constant and equal weight for every pair of observations; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.peq <- sum((ni - 1)*yi)/sum((ni - 1)*ni)
      rho.peq <- (1/(mu.peq*(1 - mu.peq)))*(sum(yi*(yi - 1))/sum(ni*(ni - 1)) - mu.peq^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.pgp <- sum(yi/ni)/k
      rho.pgp <- (1/(mu.pgp*(1 - mu.pgp)))*(sum((yi*(yi - 1))/(ni*(ni - 1)))/k - mu.pgp^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.ppr <- sum(yi)/N
      rho.ppr <- (1/(mu.ppr*(1 - mu.ppr)))*(sum(yi*(yi - 1)/(ni - 1))/N - mu.ppr^2)
      
      # ::: Estimators using ressampling method; Chakraborty & Sen (2016) :::
      # U ststistics
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      n <- sum(ni)
      u1 <- sum(yi)/N
      alp <- u1
      # Within cluster pairwise probabilities
      ucid <- sort(unique(cid))
      nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
      for(i in 1:k){
        dti <- my_data2b[cid == ucid[i], ]
        wsamp1 <- c()
        wsamp2 <- c()
        for(m in 1:(nrow(dti) -1)){
          wsamp1 <- c(wsamp1, rep(dti$y[m], length((m + 1):nrow(dti))))
          wsamp2 <- c(wsamp2, dti$y[(m + 1):nrow(dti)])
        }
        wsamp <- rbind(wsamp1, wsamp2)
        for(j in 1:ncol(wsamp)){
          if(all(wsamp[ , j] == c(0, 0)) == TRUE){
            nw00 <- nw00 + 1}
          else if(all(wsamp[ , j] == c(0, 1)) == TRUE){
            nw01 <- nw01 + 1}
          else if(all(wsamp[ , j] == c(1, 0)) == TRUE){
            nw10 <- nw10 + 1}
          else{nw11 <- nw11 + 1}
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nw11n <- nw11*2; nw00n <- nw00*2
      nw10n <- nw10 + nw01; nw01n <- nw01 + nw10
      uw11 <- nw11n/sum(ni*(ni - 1))
      uw10 <- nw10n/sum(ni*(ni - 1))
      uw01 <- nw01n/sum(ni*(ni - 1))
      uw00 <- nw00n/sum(ni*(ni - 1))
      tw <- uw11 + uw00 - uw10 - uw01
      
      # Between cluster pairwise probabilities
      nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
      for(i in 1:(k - 1)){
        dti <- my_data2b[cid == ucid[i], ]
        for(m in (i + 1):k){
          dtm <- my_data2b[cid == ucid[m], ]
          bsamp1 <- rep(dti$y, each = nrow(dtm))
          bsamp2 <-rep(dtm$y, times = nrow(dti))
          bsamp <- rbind(bsamp1, bsamp2)
          for(j in 1:ncol(bsamp)){
            if(all(bsamp[ , j] == c(0, 0)) == TRUE){
              nb00 <- nb00 + 1}
            else if(all(bsamp[ , j] == c(0, 1)) == TRUE){
              nb01 <- nb01 + 1}
            else if(all(bsamp[ , j] == c(1, 0)) == TRUE){
              nb10 <- nb10 + 1}
            else{nb11 <- nb11 + 1}
          }
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nb11n <- nb11*2; nb00n <- nb00*2
      nb10n <- nb10 + nb01; nb01n <- nb01 + nb10
      ub11 <- nb11n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub10 <- nb10n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub01 <- nb01n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub00 <- nb00n/(N*(N - 1) - sum(ni*(ni - 1)))
      tb <- ub11 + ub00 - ub10 - ub01
      
      rho.rm <- (tw - tb)/(4*u1*(1 - u1))
      
      # Model Linearization and Monte Carlo Simulation Methods; Goldstein et al. (2002)
      mmod <- lme4::glmer(Outcome ~ 1 + (1 | Cluster), family = binomial, data = my_data2, nAGQ = 1)
      fint <- lme4::fixef(mmod)
      re_var <- as.vector(lme4::VarCorr(mmod)[[1]])
      pr <- exp(fint)/(1 + exp(fint))
      sig1 <- pr*(1 - pr)
      sig2 <- re_var*pr^2*(1 + exp(fint))^(-2)
      rho.lin <- sig2/(sig1 + sig2)
      
      z <- rnorm(n = 1000, mean = 0, sd = sqrt(re_var))
      pr <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(pr*(1 - pr))
      sig2 <- var(pr)
      rho.sim <- sig2/(sig1 + sig2)
      
      
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data2,corstr="exch",family=binomial ,scale.fix = TRUE))
      glmm_mod  <- glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCfo <-bv/(bv+p*(1-p))
      
      colnames(my_data2b)[1] <- "Cluster" 
      colnames(my_data2b)[2] <- "Outcome"  
      my_data2b$Outcome <- as.factor(my_data2b$Outcome)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("ANOVA Estimate:", round(rho.aov,3))})
      ICC2 <- renderText({paste("Modified ANOVA Estimate:", round(rho.aovs,3))})
      ICC3 <- renderText({paste("Moment Estimate with Equal Weights:", round(rho.keq,3))})
      ICC4 <- renderText({paste("Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kpr,3))})
      ICC5 <- renderText({paste("Modified Moment Estimate with Equal Weights:", round(rho.keqs,3))})
      ICC6 <- renderText({paste("Modified Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kprs,3))})
      ICC7 <- renderText({paste("Stabilized Moment Estimate:", round(rho.stab,3))})
      ICC8 <- renderText({paste("Modified Moment Estimate from Unbiased Estimating Equation:", round(rho.ub,3))})
      ICC9 <- renderText({paste("Fleiss-Cuzick Kappa Type Estimate:", round(rho.fc,3))})
      ICC10 <- renderText({paste("Mak's Unweighted Average Estimate:", round(rho.mak,3))})
      ICC11 <- renderText({paste("Correlation Estimate with Equal Weight to Every Pair of Observations:", round(rho.peq,3))})
      ICC12 <- renderText({paste("Correlation Estimate with Equal Weight to Each Cluster Irrespective of Size:", round(rho.pgp,3))})
      ICC13 <- renderText({paste("Correlation Estimate with Weighting Each Pair According to Number of Pairs individuals Appear:", round(rho.ppr,3))})
      ICC14 <- renderText({paste("Resampling Estimate:", round(rho.rm,3))})
      ICC15 <- renderText({paste("First-order Model Linearized Estimate:", round(rho.lin,3))})
      ICC16 <- renderText({paste("Monte Carlo Simulation Estimate:", round(rho.sim,3))})
      ICC17 <-  renderText({paste("Mixed Model with logit link (log-odds scale):", round(icc(glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2, family = binomial))[1],3))})
      #ICC18 <-  renderText({paste("First-order Taylor extension with logit link:", round(ICCfo,3))})
      #ICC19 <-  renderText({paste("Estimate via Generalized Estimating Equations:", round(gee_mod$correlation[1,1],3))})
      
      output$PrintICC2 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5(), ICC6(), ICC7(), ICC8(), ICC9(), ICC10(), ICC11(), ICC12(), ICC13(), ICC14(), ICC15(), ICC16(), ICC17())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data2))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data2$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data2$Cluster)))/mean(as.vector(table(my_data2$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details2 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot4 <- renderPlotly({
        ggplotly(ggplot(my_data2b, aes(x = Cluster,
                                       fill = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data2
  })
  
  output$my_output_data2 <- renderTable({data2()},include.rownames=FALSE, colnames =T)
  
  ########Data/ Binary
  ######/Same condition
  #######/Excel 
  
  data22 <- reactive({
    if(input$Load22 == 0){return()}
    inFile <- input$file22
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load22
      my_data2 <- read_excel(inFile$datapath, col_names = input$header22)
      
      colnames(my_data2)[1] <- "Cluster" 
      colnames(my_data2)[2] <- "Outcome" 
      
      my_data2$Outcome <- as.numeric(my_data2$Outcome)
      my_data2$Cluster <- factor(my_data2$Cluster,levels=unique(my_data2$Cluster))
      
      my_data2b<-na.omit(my_data2)
      
      colnames(my_data2b)[1] <- "cid" 
      colnames(my_data2b)[2] <- "y"     
      
      # Cluster ID
      cid <- my_data2b$cid
      # Response variable
      y <- my_data2b$y
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      # ::: ANOVA method :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: Modified ANOVA (Fleiss, 1981) :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msbs <- (1/(k))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aovs <- (msbs - msw)/(msbs + (n0 - 1)*msw)
      
      # ::: Moment estimators :::
      pii <- yi/ni
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.keq <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.kpr <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.keqs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.kprs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      kappa = 0.45
      p <- sum(yi)/sum(ni)
      wi <- ni/N
      sw <- sum(wi*(pii - piw)^2)
      rho.stab <- (1/(n0 - 1))*((N*sw)/((k - 1)*p*(1 - p)) + kappa - 1)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.ub <- 1 - (N*n0*(k - 1)*msw)/(sum(yi)*(n0*(k - 1) - sum(yi)) + sum(yisq))
      
      # ::: Estimators based on a direct probabilistic method :::
      # Fleiss and Cuzick (1979) Method 
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      piio <- sum(yi)/sum(ni)
      rho.fc <- 1 - (1/((N - k)*piio*(1 - piio)))*sum(yi*(ni - yi)/ni)
      
      # Mak (1988) method
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      ni <- as.vector(table(cid))
      rho.mak <- 1 - (k - 1)*sum((yi*(ni - yi))/(ni*(ni - 1)))/(sum(yisq/ni^2) + sum(yi/ni)*(k - 1 - sum(yi/ni)))
      
      # ::: Estimators based on direct calculation of correlation within each group :::
      # wi = constant and equal weight for every pair of observations; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.peq <- sum((ni - 1)*yi)/sum((ni - 1)*ni)
      rho.peq <- (1/(mu.peq*(1 - mu.peq)))*(sum(yi*(yi - 1))/sum(ni*(ni - 1)) - mu.peq^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.pgp <- sum(yi/ni)/k
      rho.pgp <- (1/(mu.pgp*(1 - mu.pgp)))*(sum((yi*(yi - 1))/(ni*(ni - 1)))/k - mu.pgp^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.ppr <- sum(yi)/N
      rho.ppr <- (1/(mu.ppr*(1 - mu.ppr)))*(sum(yi*(yi - 1)/(ni - 1))/N - mu.ppr^2)
      
      # ::: Estimators using ressampling method; Chakraborty & Sen (2016) :::
      # U ststistics
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      n <- sum(ni)
      u1 <- sum(yi)/N
      alp <- u1
      # Within cluster pairwise probabilities
      ucid <- sort(unique(cid))
      nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
      for(i in 1:k){
        dti <- my_data2b[cid == ucid[i], ]
        wsamp1 <- c()
        wsamp2 <- c()
        for(m in 1:(nrow(dti) -1)){
          wsamp1 <- c(wsamp1, rep(dti$y[m], length((m + 1):nrow(dti))))
          wsamp2 <- c(wsamp2, dti$y[(m + 1):nrow(dti)])
        }
        wsamp <- rbind(wsamp1, wsamp2)
        for(j in 1:ncol(wsamp)){
          if(all(wsamp[ , j] == c(0, 0)) == TRUE){
            nw00 <- nw00 + 1}
          else if(all(wsamp[ , j] == c(0, 1)) == TRUE){
            nw01 <- nw01 + 1}
          else if(all(wsamp[ , j] == c(1, 0)) == TRUE){
            nw10 <- nw10 + 1}
          else{nw11 <- nw11 + 1}
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nw11n <- nw11*2; nw00n <- nw00*2
      nw10n <- nw10 + nw01; nw01n <- nw01 + nw10
      uw11 <- nw11n/sum(ni*(ni - 1))
      uw10 <- nw10n/sum(ni*(ni - 1))
      uw01 <- nw01n/sum(ni*(ni - 1))
      uw00 <- nw00n/sum(ni*(ni - 1))
      tw <- uw11 + uw00 - uw10 - uw01
      
      # Between cluster pairwise probabilities
      nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
      for(i in 1:(k - 1)){
        dti <- my_data2b[cid == ucid[i], ]
        for(m in (i + 1):k){
          dtm <- my_data2b[cid == ucid[m], ]
          bsamp1 <- rep(dti$y, each = nrow(dtm))
          bsamp2 <-rep(dtm$y, times = nrow(dti))
          bsamp <- rbind(bsamp1, bsamp2)
          for(j in 1:ncol(bsamp)){
            if(all(bsamp[ , j] == c(0, 0)) == TRUE){
              nb00 <- nb00 + 1}
            else if(all(bsamp[ , j] == c(0, 1)) == TRUE){
              nb01 <- nb01 + 1}
            else if(all(bsamp[ , j] == c(1, 0)) == TRUE){
              nb10 <- nb10 + 1}
            else{nb11 <- nb11 + 1}
          }
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nb11n <- nb11*2; nb00n <- nb00*2
      nb10n <- nb10 + nb01; nb01n <- nb01 + nb10
      ub11 <- nb11n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub10 <- nb10n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub01 <- nb01n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub00 <- nb00n/(N*(N - 1) - sum(ni*(ni - 1)))
      tb <- ub11 + ub00 - ub10 - ub01
      
      rho.rm <- (tw - tb)/(4*u1*(1 - u1))
      
      # Model Linearization and Monte Carlo Simulation Methods; Goldstein et al. (2002)
      mmod <- lme4::glmer(Outcome ~ 1 + (1 | Cluster), family = binomial, data = my_data2, nAGQ = 1)
      fint <- lme4::fixef(mmod)
      re_var <- as.vector(lme4::VarCorr(mmod)[[1]])
      pr <- exp(fint)/(1 + exp(fint))
      sig1 <- pr*(1 - pr)
      sig2 <- re_var*pr^2*(1 + exp(fint))^(-2)
      rho.lin <- sig2/(sig1 + sig2)
      
      z <- rnorm(n = 1000, mean = 0, sd = sqrt(re_var))
      pr <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(pr*(1 - pr))
      sig2 <- var(pr)
      rho.sim <- sig2/(sig1 + sig2)
      
      
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data2,corstr="exch",family=binomial ,scale.fix = TRUE))
      glmm_mod  <- glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCfo <-bv/(bv+p*(1-p))
      
      colnames(my_data2b)[1] <- "Cluster" 
      colnames(my_data2b)[2] <- "Outcome"  
      my_data2b$Outcome <- as.factor(my_data2b$Outcome)
      
      #Output the ICC          
      ICC1 <- renderText({paste("ANOVA Estimate:", round(rho.aov,3))})
      ICC2 <- renderText({paste("Modified ANOVA Estimate:", round(rho.aovs,3))})
      ICC3 <- renderText({paste("Moment Estimate with Equal Weights:", round(rho.keq,3))})
      ICC4 <- renderText({paste("Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kpr,3))})
      ICC5 <- renderText({paste("Modified Moment Estimate with Equal Weights:", round(rho.keqs,3))})
      ICC6 <- renderText({paste("Modified Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kprs,3))})
      ICC7 <- renderText({paste("Stabilized Moment Estimate:", round(rho.stab,3))})
      ICC8 <- renderText({paste("Modified Moment Estimate from Unbiased Estimating Equation:", round(rho.ub,3))})
      ICC9 <- renderText({paste("Fleiss-Cuzick Kappa Type Estimate:", round(rho.fc,3))})
      ICC10 <- renderText({paste("Mak's Unweighted Average Estimate:", round(rho.mak,3))})
      ICC11 <- renderText({paste("Correlation Estimate with Equal Weight to Every Pair of Observations:", round(rho.peq,3))})
      ICC12 <- renderText({paste("Correlation Estimate with Equal Weight to Each Cluster Irrespective of Size:", round(rho.pgp,3))})
      ICC13 <- renderText({paste("Correlation Estimate with Weighting Each Pair According to Number of Pairs individuals Appear:", round(rho.ppr,3))})
      ICC14 <- renderText({paste("Resampling Estimate:", round(rho.rm,3))})
      ICC15 <- renderText({paste("First-order Model Linearized Estimate:", round(rho.lin,3))})
      ICC16 <- renderText({paste("Monte Carlo Simulation Estimate:", round(rho.sim,3))})
      ICC17 <-  renderText({paste("Mixed Model with logit link (log-odds scale):", round(icc(glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2, family = binomial))[1],3))})
      #ICC18 <-  renderText({paste("First-order Taylor extension with logit link:", round(ICCfo,3))})
      #ICC19 <-  renderText({paste("Estimate via Generalized Estimating Equations:", round(gee_mod$correlation[1,1],3))})
      output$PrintICC22 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5(), ICC6(), ICC7(), ICC8(), ICC9(), ICC10(), ICC11(), ICC12(), ICC13(), ICC14(), ICC15(), ICC16(), ICC17())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data2))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data2$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data2$Cluster)))/mean(as.vector(table(my_data2$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details22 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot44 <- renderPlotly({
        ggplotly(ggplot(my_data2b, aes(x = Cluster,
                                       fill = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data2
  })
  
  output$my_output_data22 <- renderTable({data22()},include.rownames=FALSE, colnames =T)
  
  #3) Binary outcome csv
  output$tb2 <- renderUI({
    if (!is.null(data2())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC2"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot4")),tabPanel("Data", tableOutput("Details2"),tableOutput("my_output_data2")))
    else 
      tags$img(src="bin_xlsx.png")
  })
  #4) Binary outcome xl
  output$tb22 <- renderUI({
    if (!is.null(data22())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC22"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot44")),tabPanel("Data", tableOutput("Details22"), tableOutput("my_output_data22")))
    else 
      tags$img(src="bin_xlsx.png")
  })
  
  
  ########Data/ Binary
  ######/by condition
  #Other type of file
  
  data6 <- reactive({
    if(input$Load6 == 0){return()}
    inFile <- input$file6
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load6
      my_data6 <- read.table(inFile$datapath,sep = input$sep6,header=input$header6)
      
      colnames(my_data6)[1] <- "Cluster" 
      colnames(my_data6)[2] <- "Outcome" 
      colnames(my_data6)[3] <- "Condition"
      
      my_data6$Outcome<-as.numeric(my_data6$Outcome)
      my_data6$Condition<-as.factor(my_data6$Condition)
      my_data6$Cluster<-as.factor(my_data6$Cluster)
      
      gee_mod<-summary(geese(Outcome~Condition,id=Cluster,data=my_data6,corstr="exch",family=binomial))
      glmm_mod  <- glmer(Outcome ~ Condition + (1 | Cluster), data = my_data6,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCa <-bv/(bv+p*(1-p))
      
      #Monte-carlo estimate
      z <- rnorm(n = sum(as.vector(table(my_data6$Cluster))), mean = 0, sd = sqrt(get_variance(glmm_mod)$var.random))
      pMC <- exp(fixef(glmm_mod)[[1]] + z)/(1 + exp(fixef(glmm_mod)[[1]] + z))
      sig1 <- mean(pMC*(1 - pMC))
      sig2 <- var(pMC)
      ICCaMC <- sig2/(sig1 + sig2)
      
      my_data6b<-na.omit(my_data6)
      my_data6b$Outcome<-as.factor(my_data6b$Outcome)
      
      #Ouput the ICC          
      
      ICC1 <- renderText({paste("Generalized Estimating Equation :", round(gee_mod$correlation[1,1],3))})
      ICC2 <-  renderText({paste("Generalized Linear Mixed Model (proportion scale):", round((ICCa),3))})
      ICC3 <-  renderText({paste("Monte-Carlo Estimate:", round((ICCaMC),3))})
      ICC4 <-  renderText({paste("Generalized Linear Mixed Model (log-odds scale):", round(icc(glmm_mod)[2],3))})
      
      
      output$PrintICC6 <- renderUI({
        mylist <- c(ICC1(), ICC2(),ICC3(),ICC4())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data6))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data6$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data6$Cluster)))/mean(as.vector(table(my_data6$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details6 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$barplot6 <- renderPlotly({
        ggplotly(ggplot(my_data6b, aes(x = Cluster,fill = Outcome)) +
                   geom_bar(position = "fill",stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster by Condition")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
    })
    my_data6
  })
  
  output$my_output_data6 <- renderTable({data6()},include.rownames=FALSE, colnames =T)
  
  ########Data/ Binary
  ######/by condition
  #Excel file
  
  data66 <- reactive({
    if(input$Load66 == 0){return()}
    inFile <- input$file66
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load66
      my_data6 <- read_excel(inFile$datapath, col_names = input$header66)
      
      colnames(my_data6)[1] <- "Cluster" 
      colnames(my_data6)[2] <- "Outcome" 
      colnames(my_data6)[3] <- "Condition"
      
      my_data6$Outcome<-as.numeric(my_data6$Outcome)
      my_data6$Condition<-as.factor(my_data6$Condition)
      my_data6$Cluster<-as.factor(my_data6$Cluster)
      
      gee_mod<-summary(geese(Outcome~Condition,id=Cluster,data=my_data6,corstr="exch",family=binomial))
      glmm_mod  <- glmer(Outcome ~ Condition + (1 | Cluster), data = my_data6,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCa <-bv/(bv+p*(1-p))
      
      #Monte-carlo estimate
      z <- rnorm(n = sum(as.vector(table(my_data6$Cluster))), mean = 0, sd = sqrt(get_variance(glmm_mod)$var.random))
      pMC <- exp(fixef(glmm_mod)[[1]] + z)/(1 + exp(fixef(glmm_mod)[[1]] + z))
      sig1 <- mean(pMC*(1 - pMC))
      sig2 <- var(pMC)
      ICCaMC <- sig2/(sig1 + sig2)
      
      my_data6b<-na.omit(my_data6)
      my_data6b$Outcome<-as.factor(my_data6b$Outcome)
      
      #Ouput the ICC          
      
      ICC1 <- renderText({paste("Generalized Estimating Equation :", round(gee_mod$correlation[1,1],3))})
      ICC2 <-  renderText({paste("Generalized Linear Mixed Model (proportion scale):", round((ICCa),3))})
      ICC3 <-  renderText({paste("Monte-Carlo Estimate:", round((ICCaMC),3))})
      ICC4 <-  renderText({paste("Generalized Linear Mixed Model (log-odds scale):", round(icc(glmm_mod)[2],3))})
      
      
      output$PrintICC66 <- renderUI({
        mylist <- c(ICC1(), ICC2(),ICC3(),ICC4())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data6))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data6$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data6$Cluster)))/mean(as.vector(table(my_data6$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details66 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$barplot66 <- renderPlotly({
        ggplotly(ggplot(my_data6b, aes(x = Cluster,fill = Outcome)) +
                   geom_bar(position = "fill",stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster by Condition")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
    })
    my_data6
  })
  
  output$my_output_data66 <- renderTable({data66()},include.rownames=FALSE, colnames =T)
  
  
  #9) Binary outcome by condition csv
  output$tb6 <- renderUI({
    if (is.null(data6()))
      tags$img(src="Bin int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC6"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("barplot6")),tabPanel("Data", tableOutput("Details6"), tableOutput("my_output_data6")))
  })
  #10) Binary outcome by condition xl
  output$tb66 <- renderUI({
    if (is.null(data66()))
      tags$img(src="Bin int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC66"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("barplot66")),tabPanel("Data", tableOutput("Details66"), tableOutput("my_output_data66")))
  })
  
  
  ########Binary
  ######/Same condition
  ######/csv
  
  data9 <- reactive({
    if(input$Load9 == 0){return()}
    inFile <- input$file9
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load9
      my_data9 <- read.table(inFile$datapath, header = input$header9, sep = input$sep9)
      
      colnames(my_data9)[1] <- "Cluster" 
      colnames(my_data9)[2] <- "Outcome" 
      
      my_data9$Outcome <- factor(my_data9$Outcome,levels=unique(my_data9$Outcome))
      my_data9$Cluster <- factor(my_data9$Cluster,levels=unique(my_data9$Cluster))
      
      #####Continuation Ratio Model 
      
      cr_vals <- cr_setup(as.factor(my_data9$Outcome))
      cr_data <- my_data9[cr_vals$subs, ]
      cr_data$y_new <- cr_vals$y
      cr_data$cohort <- cr_vals$cohort
      
      CR <- mixed_model(y_new ~ cohort, random = ~ 1 | Cluster, 
                        data = cr_data, family = binomial())
      
      #First order linearization
      p<-exp(fixef(CR)[[1]])/(1+exp(fixef(CR)[[1]]))
      bv<-(attr(CR$D, which = "L"))^2*(p*(1-p))^2
      ICCcr<-bv/(bv+p*(1-p))
      
      #Monte-carlo 
      fint <- CR$coefficients[[1]]
      re_var <- as.vector(attr(CR$D, which = "L")^2)
      N <- sum(as.vector(table(my_data9$Cluster)))
      z <- rnorm(n = N, mean = 0, sd = sqrt(re_var))
      p <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(p*(1 - p))
      sig2 <- var(p)
      ICCmc <- sig2/(sig1 + sig2)
      
      #####CLMM
      my_data9$y<-as.numeric(my_data9$Outcome)
      fm1<-clmm2(as.factor(y) ~ 1,random=Cluster,data=my_data9,link = "logistic")
      ICClogit <- fm1$stDev[[1]]^2 / (fm1$stDev[[1]]^2 + (pi^2 / 3))
      
      fm2<-clmm2(as.factor(y) ~ 1,random=Cluster,data=my_data9,link = "probit")
      ICCprobit<-fm2$stDev[[1]]^2/(fm2$stDev[[1]]^2+1)
      
      #Resampling estimator
      values<-unique(my_data9$y)
      x<-1*(as.data.frame(outer(my_data9$Outcome, sort(unique(my_data9$Outcome)), `==`)))
      my_data9<-cbind(my_data9,x)
      ni <- as.vector(table(my_data9$Cluster))
      k <- length(unique(my_data9$Cluster))
      N <- sum(ni)
      
      # Within cluster pairwise probabilities
      ucid <- sort(unique(my_data9$Cluster))
      my_vars<-colnames(x)
      
      for (V in my_vars){  
        nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
        
        for(i in 1:k){
          dti <- my_data9[my_data9$Cluster == ucid[i], ]
          wsamp1 <- c()
          wsamp2 <- c()
          for(m in 1:(nrow(dti) -1)){
            wsamp1 <- c(wsamp1, rep(dti[[V]][m], length((m + 1):nrow(dti))))
            wsamp2 <- c(wsamp2, dti[[V]][(m + 1):nrow(dti)])
          }
          
          wsamp12 <- rbind(wsamp1, wsamp2)
          for(j in 1:ncol(wsamp12)){
            if(all(wsamp12[ , j] == c(0, 0)) == TRUE){
              nw00 <- nw00 + 1}
            else if(all(wsamp12[ , j] == c(0, 1)) == TRUE){
              nw01 <- nw01 + 1}
            else if(all(wsamp12[ , j] == c(1, 0)) == TRUE){
              nw10 <- nw10 + 1}
            else{nw11 <- nw11 + 1}
          }
        }
        uw11<-nw11*2/sum(ni*(ni - 1))
        uw00<-nw00*2/sum(ni*(ni - 1))
        uw01<-(nw01 + nw10)/sum(ni*(ni - 1))
        uw10<-(nw01 + nw10)/sum(ni*(ni - 1))
        tw <- uw11 + uw00 - uw10 - uw01
        
        # Between cluster pairwise probabilities
        nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
        
        for(i in 1:(k - 1)){
          dti <- my_data9[my_data9$Cluster== ucid[i], ]
          for(m in (i + 1):k){
            dtm <- my_data9[my_data9$Cluster == ucid[m], ]
            bsamp1 <- rep(dti[[V]], each = nrow(dtm))
            bsamp2 <-rep(dtm[[V]], times = nrow(dti))
            
            bsamp12 <- rbind(bsamp1, bsamp2)
            for(j in 1:ncol(bsamp12)){
              if(all(bsamp12[ , j] == c(0, 0)) == TRUE){
                nb00 <- nb00 + 1}
              else if(all(bsamp12[ , j] == c(0, 1)) == TRUE){
                nb01 <- nb01 + 1}
              else if(all(bsamp12[ , j] == c(1, 0)) == TRUE){
                nb10 <- nb10 + 1}
              else{nb11 <- nb11 + 1}
            }
          }
        }
        
        ub11<-nb11*2/(N*(N - 1) - sum(ni*(ni - 1)))
        ub00<-nb00*2/(N*(N - 1) - sum(ni*(ni - 1)))
        ub01<-(nb01 + nb10)/(N*(N - 1) - sum(ni*(ni - 1)))
        ub10<-(nb01 + nb10)/(N*(N - 1) - sum(ni*(ni - 1)))
        tb <- ub11 + ub00 - ub10 - ub01
        
        assign(paste0("rho.",V,sep=""),(tw - tb)/(4*sum(my_data9[[V]]/N)*(1 - sum(my_data9[[V]]/N))))
        
      }
      
      Pattern2<-ls(pattern="rho", envir=environment())
      
      rho.list<-vector()
      for(i in values){
        rho.list[i] <- as.numeric(get(Pattern2[i]))
      }
      
      ICCre<-mean(rho.list)
      
      my_data9<-my_data9 %>% select(1:2)
      my_data9b<-na.omit(my_data9)
      my_data9b$Outcome <- as.factor(my_data9b$Outcome)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Cumulative Link Mixed Model with logit link (Latent scale):", round(ICClogit,3))})
      ICC2 <- renderText({paste("Cumulative Link Mixed Model with probit link (Latent scale):", round(ICCprobit,3))})
      ICC3 <- renderText({paste("Resampling Estimate:", round(ICCre,3))})
      #ICC4 <- renderText({paste("Continuation Ratio Model with first order linearization Estimate:", round(ICCcr,3))})
      #ICC5 <- renderText({paste("Continuation Ratio Model with Monte-Carlo simulations Estimate:", round(ICCmc,3))})
      
      output$PrintICC9 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data9))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data9$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data9$Cluster)))/mean(as.vector(table(my_data9$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details9 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot9 <- renderPlotly({
        ggplotly(ggplot(my_data9b, aes(x = Cluster,
                                       fill = Outcome,
                                       group = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="")+
                   scale_fill_brewer()+
                   theme_minimal()+
                   ggtitle("Outcome category by cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data9
  })
  
  output$my_output_data9 <- renderTable({data9()},include.rownames=FALSE, colnames =T)
  
  ########Data/ Binary
  ######/Same condition
  #######/Excel 
  
  data99 <- reactive({
    if(input$Load99 == 0){return()}
    inFile <- input$file99
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load99
      my_data9 <- read_excel(inFile$datapath, col_names = input$header99)
      
      colnames(my_data9)[1] <- "Cluster" 
      colnames(my_data9)[2] <- "Outcome" 
      
      my_data9$Outcome <- factor(my_data9$Outcome,levels=unique(my_data9$Outcome))
      my_data9$Cluster <- factor(my_data9$Cluster,levels=unique(my_data9$Cluster))
      
      #####Continuation Ratio Model 
      
      cr_vals <- cr_setup(as.factor(my_data9$Outcome))
      cr_data <- my_data9[cr_vals$subs, ]
      cr_data$y_new <- cr_vals$y
      cr_data$cohort <- cr_vals$cohort
      
      CR <- mixed_model(y_new ~ cohort, random = ~ 1 | Cluster, 
                        data = cr_data, family = binomial())
      
      #First order linearization
      p<-exp(fixef(CR)[[1]])/(1+exp(fixef(CR)[[1]]))
      bv<-(attr(CR$D, which = "L"))^2*(p*(1-p))^2
      ICCcr<-bv/(bv+p*(1-p))
      
      #Monte-carlo 
      fint <- CR$coefficients[[1]]
      re_var <- as.vector(attr(CR$D, which = "L")^2)
      N <- sum(as.vector(table(my_data9$Cluster)))
      z <- rnorm(n = N, mean = 0, sd = sqrt(re_var))
      p <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(p*(1 - p))
      sig2 <- var(p)
      ICCmc <- sig2/(sig1 + sig2)
      
      #####CLMM
      my_data9$y<-as.numeric(my_data9$Outcome)
      fm1<-clmm2(as.factor(y) ~ 1,random=Cluster,data=my_data9,link = "logistic")
      ICClogit <- fm1$stDev[[1]]^2 / (fm1$stDev[[1]]^2 + (pi^2 / 3))
      
      fm2<-clmm2(as.factor(y) ~ 1,random=Cluster,data=my_data9,link = "probit")
      ICCprobit<-fm2$stDev[[1]]^2/(fm2$stDev[[1]]^2+1)
      
      #Resampling estimator
      values<-unique(my_data9$y)
      x<-1*(as.data.frame(outer(my_data9$Outcome, sort(unique(my_data9$Outcome)), `==`)))
      my_data9<-cbind(my_data9,x)
      ni <- as.vector(table(my_data9$Cluster))
      k <- length(unique(my_data9$Cluster))
      N <- sum(ni)
      
      # Within cluster pairwise probabilities
      ucid <- sort(unique(my_data9$Cluster))
      my_vars<-colnames(x)
      
      for (V in my_vars){  
        nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
        
        for(i in 1:k){
          dti <- my_data9[my_data9$Cluster == ucid[i], ]
          wsamp1 <- c()
          wsamp2 <- c()
          for(m in 1:(nrow(dti) -1)){
            wsamp1 <- c(wsamp1, rep(dti[[V]][m], length((m + 1):nrow(dti))))
            wsamp2 <- c(wsamp2, dti[[V]][(m + 1):nrow(dti)])
          }
          
          wsamp12 <- rbind(wsamp1, wsamp2)
          for(j in 1:ncol(wsamp12)){
            if(all(wsamp12[ , j] == c(0, 0)) == TRUE){
              nw00 <- nw00 + 1}
            else if(all(wsamp12[ , j] == c(0, 1)) == TRUE){
              nw01 <- nw01 + 1}
            else if(all(wsamp12[ , j] == c(1, 0)) == TRUE){
              nw10 <- nw10 + 1}
            else{nw11 <- nw11 + 1}
          }
        }
        uw11<-nw11*2/sum(ni*(ni - 1))
        uw00<-nw00*2/sum(ni*(ni - 1))
        uw01<-(nw01 + nw10)/sum(ni*(ni - 1))
        uw10<-(nw01 + nw10)/sum(ni*(ni - 1))
        tw <- uw11 + uw00 - uw10 - uw01
        
        # Between cluster pairwise probabilities
        nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
        
        for(i in 1:(k - 1)){
          dti <- my_data9[my_data9$Cluster== ucid[i], ]
          for(m in (i + 1):k){
            dtm <- my_data9[my_data9$Cluster == ucid[m], ]
            bsamp1 <- rep(dti[[V]], each = nrow(dtm))
            bsamp2 <-rep(dtm[[V]], times = nrow(dti))
            
            bsamp12 <- rbind(bsamp1, bsamp2)
            for(j in 1:ncol(bsamp12)){
              if(all(bsamp12[ , j] == c(0, 0)) == TRUE){
                nb00 <- nb00 + 1}
              else if(all(bsamp12[ , j] == c(0, 1)) == TRUE){
                nb01 <- nb01 + 1}
              else if(all(bsamp12[ , j] == c(1, 0)) == TRUE){
                nb10 <- nb10 + 1}
              else{nb11 <- nb11 + 1}
            }
          }
        }
        
        ub11<-nb11*2/(N*(N - 1) - sum(ni*(ni - 1)))
        ub00<-nb00*2/(N*(N - 1) - sum(ni*(ni - 1)))
        ub01<-(nb01 + nb10)/(N*(N - 1) - sum(ni*(ni - 1)))
        ub10<-(nb01 + nb10)/(N*(N - 1) - sum(ni*(ni - 1)))
        tb <- ub11 + ub00 - ub10 - ub01
        
        assign(paste0("rho.",V,sep=""),(tw - tb)/(4*sum(my_data9[[V]]/N)*(1 - sum(my_data9[[V]]/N))))
        
      }
      
      Pattern2<-ls(pattern="rho", envir=environment())
      
      rho.list<-vector()
      for(i in values){
        rho.list[i] <- as.numeric(get(Pattern2[i]))
      }
      
      ICCre<-mean(rho.list)
      
      my_data9<-my_data9 %>% select(1:2)
      my_data9b<-na.omit(my_data9)
      my_data9b$Outcome <- as.factor(my_data9b$Outcome)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Cumulative Link Mixed Model with logit link (Latent scale):", round(ICClogit,3))})
      ICC2 <- renderText({paste("Cumulative Link Mixed Model with probit link (Latent scale):", round(ICCprobit,3))})
      ICC3 <- renderText({paste("Resampling Estimate:", round(ICCre,3))})
      #ICC4 <- renderText({paste("Continuation Ratio Model with first order linearization Estimate:", round(ICCcr,3))})
      #ICC5 <- renderText({paste("Continuation Ratio Model with Monte-Carlo simulations Estimate:", round(ICCmc,3))})
      
      output$PrintICC99 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data9))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data9$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data9$Cluster)))/mean(as.vector(table(my_data9$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details99 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot99 <- renderPlotly({
        ggplotly(ggplot(my_data9b, aes(x = Cluster,
                                       fill = Outcome,
                                       group = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="")+
                   scale_fill_brewer()+
                   theme_minimal()+
                   ggtitle("Outcome category by cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data9
  })
  
  output$my_output_data99 <- renderTable({data99()},include.rownames=FALSE, colnames =T)
  
  #3) Binary outcome csv
  output$tb9 <- renderUI({
    if (!is.null(data9())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC9"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot9")),tabPanel("Data", tableOutput("Details9"),tableOutput("my_output_data9")))
    else 
      tags$img(src="cat xlsx.png")
  })
  #4) Binary outcome xl
  output$tb99 <- renderUI({
    if (!is.null(data99())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC99"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot99")),tabPanel("Data", tableOutput("Details99"), tableOutput("my_output_data99")))
    else 
      tags$img(src="cat xlsx.png")
  })
  
  
  ########Categorical 
  ######/By condition
  ######/csv
  
  data10 <- reactive({
    if(input$Load10 == 0){return()}
    inFile <- input$file10
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load10
      my_data10 <- read.table(inFile$datapath, header = input$header10, sep = input$sep10)
      
      colnames(my_data10)[1] <- "Cluster" 
      colnames(my_data10)[2] <- "Outcome" 
      colnames(my_data10)[3] <- "Condition"
      
      my_data10$Outcome <- factor(my_data10$Outcome,levels=unique(my_data10$Outcome))
      my_data10$Cluster <- factor(my_data10$Cluster,levels=unique(my_data10$Cluster))
      my_data10$Condition<-factor(my_data10$Condition)
      
      #####Continuation Ratio Model 
      
      cr_vals <- cr_setup(as.factor(my_data10$Outcome))
      cr_data <- my_data10[cr_vals$subs, ]
      cr_data$y_new <- cr_vals$y
      cr_data$cohort <- cr_vals$cohort
      
      CR <- mixed_model(y_new ~ cohort + Condition, random = ~ 1 | Cluster, 
                        data = cr_data, family = binomial())
      
      #First order linearization
      p<-exp(fixef(CR)[[1]])/(1+exp(fixef(CR)[[1]]))
      bv<-(attr(CR$D, which = "L"))^2*(p*(1-p))^2
      ICCcr<-bv/(bv+p*(1-p))
      
      #Monte-carlo 
      fint <- CR$coefficients[[1]]
      re_var <- as.vector(attr(CR$D, which = "L")^2)
      N <- sum(as.vector(table(my_data10$Cluster)))
      z <- rnorm(n = N, mean = 0, sd = sqrt(re_var))
      p <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(p*(1 - p))
      sig2 <- var(p)
      ICCmc <- sig2/(sig1 + sig2)
      
      #####CLMM
      my_data10$y<-as.numeric(my_data10$Outcome)
      fm1<-clmm2(as.factor(y) ~ Condition,random=Cluster,data=my_data10,link="logistic")
      ICClogit <- fm1$stDev[[1]]^2 / (fm1$stDev[[1]]^2 + (pi^2 / 3))
      
      fm2<-clmm2(as.factor(y) ~ Condition,random=Cluster,data=my_data10,link="probit")
      ICCprobit<-fm2$stDev[[1]]^2/(fm2$stDev[[1]]^2+1)
      
      my_data10b<-na.omit(my_data10)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Cumulative Link Mixed Model with logit link (Latent scale):", round(ICClogit,3))})
      ICC2 <- renderText({paste("Cumulative Link Mixed Model with probit link (Latent scale):", round(ICCprobit,3))})
      #ICC3 <- renderText({paste("Continuation Ratio Model with first order linearization Estimate:", ICCcr)})
      #ICC4 <- renderText({paste("Continuation Ratio Model with Monte-Carlo simulations Estimate:", ICCmc)})
      
      output$PrintICC10 <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data10))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data10$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data10$Cluster)))/mean(as.vector(table(my_data10$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details10 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot10 <- renderPlotly({
        ggplotly(ggplot(my_data10b, aes(x = Cluster,
                                        fill = Outcome,
                                        group = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="")+
                   scale_fill_brewer()+
                   theme_minimal()+
                   ggtitle("Outcome category by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
    })
    my_data10
  })
  
  output$my_output_data10 <- renderTable({data10()},include.rownames=FALSE, colnames =T)
  
  ########Data/Categorical
  ######/By condition
  #######/Excel 
  
  data1010 <- reactive({
    if(input$Load1010 == 0){return()}
    inFile <- input$file1010
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load1010
      my_data10 <- read_excel(inFile$datapath, col_names = input$header1010)
      
      colnames(my_data10)[1] <- "Cluster" 
      colnames(my_data10)[2] <- "Outcome" 
      colnames(my_data10)[3] <- "Condition"
      
      my_data10$Outcome <- factor(my_data10$Outcome,levels=unique(my_data10$Outcome))
      my_data10$Cluster <- factor(my_data10$Cluster,levels=unique(my_data10$Cluster))
      my_data10$Condition<-factor(my_data10$Condition)
      
      #####Continuation Ratio Model 
      
      cr_vals <- cr_setup(as.factor(my_data10$Outcome))
      cr_data <- my_data10[cr_vals$subs, ]
      cr_data$y_new <- cr_vals$y
      cr_data$cohort <- cr_vals$cohort
      
      CR <- mixed_model(y_new ~ cohort + Condition, random = ~ 1 | Cluster, 
                        data = cr_data, family = binomial())
      
      #First order linearization
      p<-exp(fixef(CR)[[1]])/(1+exp(fixef(CR)[[1]]))
      bv<-(attr(CR$D, which = "L"))^2*(p*(1-p))^2
      ICCcr<-bv/(bv+p*(1-p))
      
      #Monte-carlo 
      fint <- CR$coefficients[[1]]
      re_var <- as.vector(attr(CR$D, which = "L")^2)
      N <- sum(as.vector(table(my_data10$Cluster)))
      z <- rnorm(n = N, mean = 0, sd = sqrt(re_var))
      p <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(p*(1 - p))
      sig2 <- var(p)
      ICCmc <- sig2/(sig1 + sig2)
      
      #####CLMM
      my_data10$y<-as.numeric(my_data10$Outcome)
      fm1<-clmm2(as.factor(y) ~ Condition,random=Cluster,data=my_data10,link="logistic")
      ICClogit <- fm1$stDev[[1]]^2 / (fm1$stDev[[1]]^2 + (pi^2 / 3))
      
      fm2<-clmm2(as.factor(y) ~ Condition,random=Cluster,data=my_data10,link="probit")
      ICCprobit<-fm2$stDev[[1]]^2/(fm2$stDev[[1]]^2+1)
      
      my_data10b<-na.omit(my_data10)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Cumulative Link Mixed Model with logit link (Latent scale):", round(ICClogit,3))})
      ICC2 <- renderText({paste("Cumulative Link Mixed Model with probit link (Latent scale):", round(ICCprobit,3))})
      #ICC3 <- renderText({paste("Continuation Ratio Model with first order linearization Estimate:", ICCcr)})
      #ICC4 <- renderText({paste("Continuation Ratio Model with Monte-Carlo simulations Estimate:", ICCmc)})
      
      output$PrintICC1010 <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data10))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data10$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data10$Cluster)))/mean(as.vector(table(my_data10$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details1010 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot1010 <- renderPlotly({
        ggplotly(ggplot(my_data10b, aes(x = Cluster,
                                        fill = Outcome,
                                        group = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="")+
                   scale_fill_brewer()+
                   theme_minimal()+
                   ggtitle("Outcome category by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
    })
    my_data10
  })
  
  output$my_output_data1010 <- renderTable({data1010()},include.rownames=FALSE, colnames =T)
  
  #3) Binary outcome csv
  output$tb10 <- renderUI({
    if (!is.null(data10())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC10"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot10")),tabPanel("Data", tableOutput("Details10"),tableOutput("my_output_data10")))
    else 
      tags$img(src="cat int xlsx.png")
  })
  #4) Binary outcome xl
  output$tb1010 <- renderUI({
    if (!is.null(data1010())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC1010"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot1010")),tabPanel("Data", tableOutput("Details1010"), tableOutput("my_output_data1010")))
    else 
      tags$img(src="cat int xlsx.png")
  })
  
  
  ###################Data
  ##################/Continuous
  ##################/Same condition
  #Other type of file
  
  data4 <- reactive({
    if(input$Load4 == 0){return()}
    inFile <- input$file4
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load4
      my_data4 <- read.table(inFile$datapath,sep = input$sep4, header=input$header4)
      
      colnames(my_data4)[1] <- "Cluster" 
      colnames(my_data4)[2] <- "Outcome" 
      
      my_data4$Cluster <- as.factor(my_data4$Cluster)
      my_data4$Outcome <- as.numeric(my_data4$Outcome)
      
      ICC<-clus.rho(my_data4$Outcome,my_data4$Cluster)
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data4,corstr="exch", scale.fix = TRUE))
      lmm_mod  <- lmer(Outcome ~ 1 + (1 | Cluster), data = my_data4)
      
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Pearson correlation coefficient :", round(ICC$icc[1],3))})
      ICC2 <- renderText({paste("Adjusted r-square :", round(ICC$icc[2],3))})
      ICC3 <- renderText({paste("ANOVA :", round(ICC$icc[3],3))})
      ICC4 <- renderText({paste("Generalized Estimating Equation (GEE):", round(gee_mod$correlation[1,1],3))})
      ICC5 <- renderText({paste("Linear Mixed Model :", round(performance::model_performance(lmm_mod)$ICC,3))})
      
      output$PrintICC44 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data4))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data4$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data4$Cluster)))/mean(as.vector(table(my_data4$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details4 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      output$boxplot4 <- renderPlotly({
        ggplotly(
          ggplot(my_data4, aes(Cluster, Outcome)) +
            geom_boxplot(fill="#56B4E9") +
            xlab("Cluster") +
            ylab("Outcome") +
            theme_minimal() +
            ggtitle("Outcome by cluster") +
            theme(
              plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
            )
        )
      })
      
      output$dist4 <- renderPlot({
        ggplot(my_data4, aes(Outcome)) + 
          geom_histogram(fill="#56B4E9", color="#e9ecef")+
          theme_minimal()+
          ggtitle("Outcome distribution") +
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",round(min(Outcome,na.rm=T),3),"\n","Q25 =",round(quantile(Outcome, .25,na.rm=T),3),"\n","Median =",round(median(Outcome, na.rm = TRUE),3),"\n","Mean =",round(mean(Outcome,na.rm=T),3),"\n","Q75 =",round(quantile(Outcome, .75,na.rm=T),3),"\n","Max =",round(max(Outcome,na.rm=T),3))))+
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
        
      })
      
    })
    my_data4
  })
  
  output$my_output_data4 <- renderTable({data4()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/Continuous
  ##################/Same condition     
  #Excel file
  
  data44 <- reactive({
    if(input$Load44 == 0){return()}
    inFile <- input$file44
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load44
      my_data4 <- read_excel(inFile$datapath, col_names = input$header44)
      
      colnames(my_data4)[1] <- "Cluster" 
      colnames(my_data4)[2] <- "Outcome" 
      
      my_data4$Cluster <- as.factor(my_data4$Cluster)
      my_data4$Outcome <- as.numeric(my_data4$Outcome)
      
      ICC<-clus.rho(my_data4$Outcome,my_data4$Cluster)
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data4,corstr="exch", scale.fix = TRUE))
      lmm_mod  <- lmer(Outcome ~ 1 + (1 | Cluster), data = my_data4)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Pearson correlation coefficient :", round(ICC$icc[1],3))})
      ICC2 <- renderText({paste("Adjusted r-square :", round(ICC$icc[2],3))})
      ICC3 <- renderText({paste("ANOVA :", round(ICC$icc[3],3))})
      ICC4 <- renderText({paste("Generalized Estimating Equation (GEE):", round(gee_mod$correlation[1,1],3))})
      ICC5 <- renderText({paste("Linear Mixed Model :", round(performance::model_performance(lmm_mod)$ICC,3))})
      
      output$PrintICC44 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data4))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data4$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :",round(sd(as.vector(table(my_data4$Cluster)))/mean(as.vector(table(my_data4$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details44 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      output$boxplot44 <- renderPlotly({
        ggplotly(
          ggplot(my_data4, aes(Cluster, Outcome)) +
            geom_boxplot(fill="#56B4E9") +
            xlab("Cluster") +
            ylab("Outcome") +
            theme_minimal() +
            ggtitle("Outcome by cluster") +
            theme(
              plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
            )
        )
      })
      
      output$dist44 <- renderPlot({
        ggplot(my_data4, aes(Outcome)) + 
          geom_histogram(fill="#56B4E9", color="#e9ecef")+
          theme_minimal()+
          ggtitle("Outcome distribution") +
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",round(min(Outcome,na.rm=T),3),"\n","Q25 =",round(quantile(Outcome, .25,na.rm=T),3),"\n","Median =",round(median(Outcome, na.rm = TRUE),3),"\n","Mean =",round(mean(Outcome,na.rm=T),3),"\n","Q75 =",round(quantile(Outcome, .75,na.rm=T),3),"\n","Max =",round(max(Outcome,na.rm=T),3))))+
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data4
  })
  
  output$my_output_data44 <- renderTable({data44()},include.rownames=FALSE, colnames =T)
  
  
  #5) Continuous outcome same condition csv
  output$tb4 <- renderUI({
    if (is.null(data4()))
      tags$img(src="cont xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC4"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot4"),tags$hr(),plotOutput("dist4")),tabPanel("Data", tableOutput("Details4"), tableOutput("my_output_data4")))
  }) 
  #6) Continuous outcome same condition xl
  output$tb44 <- renderUI({
    if (is.null(data44()))
      tags$img(src="cont xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC44"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot44"),tags$hr(),plotOutput("dist44")),tabPanel("Data", tableOutput("Details44"), tableOutput("my_output_data44")))
  })
  
  
  ###################Data
  ##################/Continuous
  ##################/by condition
  #Other type of file
  
  data5 <- reactive({
    if(input$Load5 == 0){return()}
    inFile <- input$file5
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load5
      my_data5 <- read.table(inFile$datapath,sep = input$sep5,header=input$header5)
      
      colnames(my_data5)[1] <- "Cluster" 
      colnames(my_data5)[2] <- "Outcome" 
      colnames(my_data5)[3] <- "Condition" 
      
      my_data5$Outcome<-as.numeric(my_data5$Outcome)
      my_data5$Condition<-as.factor(my_data5$Condition)
      my_data5$Cluster<-as.factor(my_data5$Cluster)
      
      gee_mod<-summary(geese(Outcome~Condition,id=Cluster,data=my_data5,corstr="exch", scale.fix = TRUE))
      lmm_mod  <- lmer(Outcome ~ Condition + (1 | Cluster), data = my_data5)
      
      #Ouput the ICC          
      
      ICC1 <- renderText({paste("Generalized Estimating Equation:", round(gee_mod$correlation[1,1],3))})
      ICC2 <-  renderText({paste("Linear Mixed Model:", round(performance::model_performance(lmm_mod)$ICC,3))})
      
      output$PrintICC5 <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data5))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data5$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data5$Cluster)))/mean(as.vector(table(my_data5$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details5 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      output$boxplot5 <- renderPlotly({
        ggplotly(ggplot(my_data5, aes(Cluster, Outcome)) +
                   geom_boxplot(aes(fill = Condition)) +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   theme_minimal()+
                   ggtitle("Outcome by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_blank())+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
      output$dist5 <- renderPlot({
        ggplot(my_data5, aes(Outcome)) + 
          geom_histogram(aes(fill=Condition), color="#e9ecef")+
          theme_minimal()+
          ggtitle("Outcome distribution by Condition") +
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",round(min(Outcome,na.rm=T),3),"\n","Q25 =",round(quantile(Outcome, .25,na.rm=T),3),"\n","Median =",round(median(Outcome, na.rm = TRUE),3),"\n","Mean =",round(mean(Outcome,na.rm=T),3),"\n","Q75 =",round(quantile(Outcome, .75,na.rm=T),3),"\n","Max =",round(max(Outcome,na.rm=T),3))))+
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data5
  })
  
  output$my_output_data5 <- renderTable({data5()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/Continuous
  ##################/by condition
  #Excel file
  
  data55 <- reactive({
    if(input$Load55 == 0){return()}
    inFile <- input$file55
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load55
      my_data5 <- read_excel(inFile$datapath, col_names = input$header55)
      
      colnames(my_data5)[1] <- "Cluster" 
      colnames(my_data5)[2] <- "Outcome" 
      colnames(my_data5)[3] <- "Condition" 
      
      my_data5$Outcome<-as.numeric(my_data5$Outcome)
      my_data5$Condition<-as.factor(my_data5$Condition)
      my_data5$Cluster<-as.factor(my_data5$Cluster)
      
      gee_mod<-summary(geese(Outcome~Condition,id=Cluster,data=my_data5,corstr="exch", scale.fix = TRUE))
      lmm_mod  <- lmer(Outcome ~ Condition + (1 | Cluster), data = my_data5)
      
      #Ouput the ICC          
      
      ICC1 <- renderText({paste("Generalized Estimating Equation:", round(gee_mod$correlation[1,1],3))})
      ICC2 <-  renderText({paste("Linear Mixed Model:", round(performance::model_performance(lmm_mod)$ICC,3))})
      
      output$PrintICC55 <- renderUI({
        mylist <- c(ICC1(), ICC2())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data5))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data5$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data5$Cluster)))/mean(as.vector(table(my_data5$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details55 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      output$boxplot55 <- renderPlotly({
        ggplotly(ggplot(my_data5, aes(Cluster, Outcome)) +
                   geom_boxplot(aes(fill = Condition)) +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   theme_minimal()+
                   ggtitle("Outcome by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_blank())+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
      output$dist55 <- renderPlot({
        ggplot(my_data5, aes(Outcome)) + 
          geom_histogram(aes(fill=Condition), color="#e9ecef")+
          theme_minimal()+
          ggtitle("Outcome distribution by Condition") +
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",round(min(Outcome,na.rm=T),3),"\n","Q25 =",round(quantile(Outcome, .25,na.rm=T),3),"\n","Median =",round(median(Outcome, na.rm = TRUE),3),"\n","Mean =",round(mean(Outcome,na.rm=T),3),"\n","Q75 =",round(quantile(Outcome, .75,na.rm=T),3),"\n","Max =",round(max(Outcome,na.rm=T),3))))+
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data5
  })
  
  output$my_output_data55 <- renderTable({data55()},include.rownames=FALSE, colnames =T)
  
  
  #7) Continuous outcome by condition csv
  output$tb5 <- renderUI({
    if (is.null(data5()))
      tags$img(src="cont int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC5"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot5"),tags$hr(),plotOutput("dist5")),tabPanel("Data", tableOutput("Details5"), tableOutput("my_output_data5")))
  })
  #8) Continuous outcome by condition xl
  output$tb55 <- renderUI({
    if (is.null(data55()))
      tags$img(src="cont int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC55"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot55"),tags$hr(),plotOutput("dist55")),tabPanel("Data", tableOutput("Details55"), tableOutput("my_output_data55")))
  })
  
  
  ###################Data
  ##################/Count
  ##################/Same condition     
  #Other file
  
  data7 <- reactive({
    if(input$Load7 == 0){return()}
    inFile <- input$file7
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load7
      my_data7 <- read.table(inFile$datapath,sep = input$sep7,header=input$header7)
      
      colnames(my_data7)[1] <- "Cluster" 
      colnames(my_data7)[2] <- "Outcome" 
      
      my_data7$Outcome<-as.numeric(my_data7$Outcome)
      my_data7$Cluster<-as.factor(my_data7$Cluster)
      
      Poisson<-icc_counts(my_data7,y="Outcome",id="Cluster")
      MPoisson<-glmer(Outcome ~ 1 + (1 | Cluster), family=poisson,data=my_data7)
      model_pois <- glmmTMB(Outcome ~ 1 + (1 | Cluster), family = poisson, data = my_data7)
      
      Zip_Poisson<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zip")
      MZip_Poisson<-glmmTMB(Outcome ~ 1 + (1 | Cluster),ziformula=~1,family=poisson,data=my_data7)
      Nbinom1<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="nbinom1")
      MNbinom1<-glmmTMB(Outcome ~  1 + (1 | Cluster),data=my_data7, family = "nbinom1")
      Nbinom2<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="nbinom2")
      MNbinom2<-glmmTMB(Outcome ~  1 + (1 | Cluster),data=my_data7, family = "nbinom2")
      Zinb1<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zinb1")
      MZinb1<-glmmTMB(Outcome ~ 1 + (1 | Cluster), ziformula = ~ 1 + (1 | Cluster),family = "nbinom1",data = my_data7)
      Zinb2<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zinb2")
      MZinb2<-glmmTMB(Outcome ~ 1 + (1 | Cluster), ziformula = ~ 1 + (1 | Cluster),family = "nbinom2",data = my_data7)
      Gee_Poisson<-summary(geeglm(Outcome ~ 1, id=Cluster, family=poisson, corstr = "exchangeable",data=my_data7))
      
      #Nagakawa
      compute_icc_nakagawa <- function(model, type = c("quasi-poisson", "nbinom")) {
        
        type <- match.arg(type)
        
        # Extract random effect variance
        var_random <- as.numeric(VarCorr(model)$cond[[1]])
        
        # Get predicted marginal mean
        mu <- mean(predict(model, type = "response"))
        
        # Identify family
        fam <- family(model)$family
        
        # ICC calculations
        
        if (type == "quasi-poisson") {
          # Estimate overdispersion from Poisson model
          pearson_resid <- residuals(model, type = "pearson")
          phi <- sum(pearson_resid^2) / df.residual(model)
          var_residual <- log(1 + phi / mu)
          icc <- var_random / (var_random + var_residual)
        }
        
        if (type == "nbinom") {
          theta <- 1 / sigma(model)^2
          var_residual <- log(1 + 1/mu + 1/theta)
          icc <- var_random / (var_random + var_residual)
        }
        
        return(list(ICC = icc,
                    var_random = var_random,
                    var_residual = var_residual))
      }
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("GLMM Poisson :", round(Poisson$ICC[1],3), ", AIC (",performance::model_performance(MPoisson)$AIC,")")})
      ICC11 <- renderText({paste("GLMM Quasi-Poisson (Log scale):", round(compute_icc_nakagawa(model_pois, type = "quasi-poisson")$ICC[1],3))})
      ICC2 <- renderText({paste("GLMM Zero-inflated Poisson :", round(Zip_Poisson$ICC[1],3), ", AIC (",performance::model_performance(MZip_Poisson)$AIC,")")})
      ICC3 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean:", round(Nbinom1$ICC[1],3), ", AIC (",performance::model_performance(MNbinom1)$AIC,")")})
      ICC33 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean (Log scale):", round(compute_icc_nakagawa(MNbinom1, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(MNbinom1)$AIC,")")})
      ICC4 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean:", round(Nbinom2$ICC[1],3), ", AIC (",performance::model_performance(MNbinom2)$AIC,")")})
      ICC44 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean (Log scale):", round(compute_icc_nakagawa(MNbinom2, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(MNbinom2)$AIC,")")})
      ICC5 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing linearly with the mean:", round(Zinb1$ICC[1],3), ", AIC (",performance::model_performance(MZinb1)$AIC,")")})
      ICC6 <-  renderText({paste("GLMM Zero-inflated Negative Binomial (variance increasing quadratically with the mean):", round(Zinb2$ICC[1],3), ", AIC (",performance::model_performance(MZinb2)$AIC,")")})
      ICC7 <-  renderText({paste("GEE Poisson :", round(Gee_Poisson$corr[1,1],3))})
      
      output$PrintICC7 <- renderUI({
        mylist <- c(ICC1(),ICC11(), ICC2(), ICC3(),ICC33(), ICC4(),ICC44(), ICC5(),ICC6(), ICC7())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data7))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data7$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data7$Cluster)))/mean(as.vector(table(my_data7$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details7 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$boxplot7 <- renderPlotly({
        ggplotly(ggplot(my_data7, aes(Cluster, Outcome)) +
                   geom_boxplot(fill="#56B4E9") +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   scale_fill_discrete(name='Cluster')+
                   theme_minimal()+
                   ggtitle("Outcome by cluster") +
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
      output$dist7 <- renderPlot({
        ggplot(my_data7, aes(Outcome)) + 
          geom_histogram(fill="#56B4E9", color="#e9ecef")+
          theme_minimal()+
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",min(Outcome,na.rm=T),"\n","Q25 =",quantile(Outcome, .25,na.rm=T),"\n","Median =",median(Outcome, na.rm = TRUE),"\n","Mean =",mean(Outcome,na.rm=T),"\n","Q75 =",quantile(Outcome, .75,na.rm=T),"\n","Max =",max(Outcome,na.rm=T))))+
          ggtitle("Outcome distribution") +
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data7
  })
  
  output$my_output_data7 <- renderTable({data7()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/Count
  ##################/Same condition     
  #Excel file
  
  data77 <- reactive({
    if(input$Load77 == 0){return()}
    inFile <- input$file77
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load77
      my_data7 <- read_excel(inFile$datapath, col_names = input$header77)
      
      colnames(my_data7)[1] <- "Cluster" 
      colnames(my_data7)[2] <- "Outcome" 
      
      my_data7$Cluster <- as.factor(my_data7$Cluster)
      my_data7$Outcome <- as.numeric(my_data7$Outcome)
      
      Poisson<-icc_counts(my_data7,y="Outcome",id="Cluster")
      MPoisson<-glmer(Outcome ~ 1 + (1 | Cluster), family=poisson,data=my_data7)
      model_pois <- glmmTMB(Outcome ~ 1 + (1 | Cluster), family = poisson, data = my_data7)
      
      Zip_Poisson<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zip")
      MZip_Poisson<-glmmTMB(Outcome ~ 1 + (1 | Cluster),ziformula=~1,family=poisson,data=my_data7)
      Nbinom1<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="nbinom1")
      MNbinom1<-glmmTMB(Outcome ~  1 + (1 | Cluster),data=my_data7, family = "nbinom1")
      Nbinom2<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="nbinom2")
      MNbinom2<-glmmTMB(Outcome ~  1 + (1 | Cluster),data=my_data7, family = "nbinom2")
      Zinb1<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zinb1")
      MZinb1<-glmmTMB(Outcome ~ 1 + (1 | Cluster), ziformula = ~ 1 + (1 | Cluster),family = "nbinom1",data = my_data7)
      Zinb2<-icc_counts(my_data7,y="Outcome",id="Cluster",fam="zinb2")
      MZinb2<-glmmTMB(Outcome ~ 1 + (1 | Cluster), ziformula = ~ 1 + (1 | Cluster),family = "nbinom2",data = my_data7)
      Gee_Poisson<-summary(geeglm(Outcome ~ 1, id=Cluster, family=poisson, corstr = "exchangeable",data=my_data7))
      
      #Nagakawa
      compute_icc_nakagawa <- function(model, type = c("quasi-poisson", "nbinom")) {
        
        type <- match.arg(type)
        
        # Extract random effect variance
        var_random <- as.numeric(VarCorr(model)$cond[[1]])
        
        # Get predicted marginal mean
        mu <- mean(predict(model, type = "response"))
        
        # Identify family
        fam <- family(model)$family
        
        # ICC calculations
        
        if (type == "quasi-poisson") {
          # Estimate overdispersion from Poisson model
          pearson_resid <- residuals(model, type = "pearson")
          phi <- sum(pearson_resid^2) / df.residual(model)
          var_residual <- log(1 + phi / mu)
          icc <- var_random / (var_random + var_residual)
        }
        
        if (type == "nbinom") {
          theta <- 1 / sigma(model)^2
          var_residual <- log(1 + 1/mu + 1/theta)
          icc <- var_random / (var_random + var_residual)
        }
        
        return(list(ICC = icc,
                    var_random = var_random,
                    var_residual = var_residual))
      }
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("GLMM Poisson :", round(Poisson$ICC[1],3), ", AIC (",performance::model_performance(MPoisson)$AIC,")")})
      ICC11 <- renderText({paste("GLMM Quasi-Poisson (Log scale):", round(compute_icc_nakagawa(model_pois, type = "quasi-poisson")$ICC[1],3))})
      ICC2 <- renderText({paste("GLMM Zero-inflated Poisson :", round(Zip_Poisson$ICC[1],3), ", AIC (",performance::model_performance(MZip_Poisson)$AIC,")")})
      ICC3 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean:", round(Nbinom1$ICC[1],3), ", AIC (",performance::model_performance(MNbinom1)$AIC,")")})
      ICC33 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean (Log scale):", round(compute_icc_nakagawa(MNbinom1, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(MNbinom1)$AIC,")")})
      ICC4 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean:", round(Nbinom2$ICC[1],3), ", AIC (",performance::model_performance(MNbinom2)$AIC,")")})
      ICC44 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean (Log scale):", round(compute_icc_nakagawa(MNbinom2, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(MNbinom2)$AIC,")")})
      ICC5 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing linearly with the mean:", round(Zinb1$ICC[1],3), ", AIC (",performance::model_performance(MZinb1)$AIC,")")})
      ICC6 <-  renderText({paste("GLMM Zero-inflated Negative Binomial (variance increasing quadratically with the mean):", round(Zinb2$ICC[1],3), ", AIC (",performance::model_performance(MZinb2)$AIC,")")})
      ICC7 <-  renderText({paste("GEE Poisson :", round(Gee_Poisson$corr[1,1],3))})
      
      output$PrintICC77 <- renderUI({
        mylist <- c(ICC1(),ICC11(), ICC2(), ICC3(),ICC33(), ICC4(),ICC44(), ICC5(),ICC6(), ICC7())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data7))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data7$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data7$Cluster)))/mean(as.vector(table(my_data7$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details77 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      output$boxplot77 <- renderPlotly({
        ggplotly(ggplot(my_data7, aes(Cluster, Outcome)) +
                   geom_boxplot(fill="#56B4E9") +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   scale_fill_discrete(name='Cluster')+
                   theme_minimal()+
                   ggtitle("Outcome by cluster") +
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
      output$dist77 <- renderPlot({
        ggplot(my_data7, aes(Outcome)) + 
          geom_histogram(fill="#56B4E9", color="#e9ecef")+
          theme_minimal()+
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",min(Outcome,na.rm=T),"\n","Q25 =",quantile(Outcome, .25,na.rm=T),"\n","Median =",median(Outcome, na.rm = TRUE),"\n","Mean =",mean(Outcome,na.rm=T),"\n","Q75 =",quantile(Outcome, .75,na.rm=T),"\n","Max =",max(Outcome,na.rm=T))))+
          ggtitle("Outcome distribution") +
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data7
  })
  
  output$my_output_data77 <- renderTable({data77()},include.rownames=FALSE, colnames =T)
  
  
  #12) Count outcome same condition xl
  output$tb77 <- renderUI({
    if (is.null(data77()))
      tags$img(src="count xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC77"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot77"),tags$hr(),plotOutput("dist77")),tabPanel("Data", tableOutput("Details77"), tableOutput("my_output_data77")))
  })
  #13) Count outcome same condition csv
  output$tb7 <- renderUI({
    if (is.null(data7()))
      tags$img(src="count xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC7"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot7"),tags$hr(),plotOutput("dist7")),tabPanel("Data", tableOutput("Details7"), tableOutput("my_output_data7")))
  })
  
  
  ###################Data
  ##################/Count
  ##################/By condition     
  #Other file
  
  data8 <- reactive({
    if(input$Load8 == 0){return()}
    inFile <- input$file8
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load8
      my_data8 <- read.table(inFile$datapath,sep = input$sep8,header=input$header8)
      
      colnames(my_data8)[1] <- "Cluster" 
      colnames(my_data8)[2] <- "Outcome"
      colnames(my_data8)[3] <- "Condition" 
      
      my_data8$Outcome<-as.numeric(my_data8$Outcome)
      my_data8$Condition<-as.factor(my_data8$Condition)
      my_data8$Cluster<-as.factor(my_data8$Cluster)
      
      Poisson<-glmer(Outcome ~ Condition + (1 | Cluster), family=poisson,data=my_data8)
      model_pois <- glmmTMB(Outcome ~ Condition + (1 | Cluster), family = poisson, data = my_data8)
      ICCPoissona<-mean(my_data8$Outcome)*(exp(get_variance(Poisson)$var.random)-1)/(mean(my_data8$Outcome)*(exp(get_variance(Poisson)$var.random)-1)+1)
      Zip_Poisson<-glmmTMB(Outcome ~ Condition + (1 | Cluster),ziformula=~Condition,family=poisson,data=my_data8)
      ICCZipa<-mean(my_data8$Outcome)*(exp(get_variance(Zip_Poisson)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zip")$varcomp$Pi)/(mean(my_data8$Outcome)*(exp(get_variance(Zip_Poisson)$var.random)-1)+1+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zip")$varcomp$Pi)
      Nbinom1<-glmmTMB(Outcome ~ Condition + (1 | Cluster), family = "nbinom1", data=my_data8)
      ICCNbinom1a<-mean(my_data8$Outcome)*(exp(get_variance(Nbinom1)$var.random)-1)/(mean(my_data8$Outcome)*(exp(get_variance(Nbinom1)$var.random)-1)+icc_counts(my_data8,y="Outcome",id="Cluster",fam="nbinom1")$varcomp$r+1)
      Nbinom2<-glmmTMB(Outcome ~ Condition + (1 | Cluster), family = "nbinom2", data=my_data8)
      ICCNbinom2a<-mean(my_data8$Outcome)*(exp(get_variance(Nbinom2)$var.random)-1)/(mean(my_data8$Outcome)*((icc_counts(my_data8,y="Outcome",id="Cluster",fam="nbinom2")$varcomp$r+1)*exp(get_variance(Nbinom2)$var.random)-1)+1)
      Zinb1<-glmmTMB(Outcome ~ Condition + (1 | Cluster), ziformula = ~ Condition + (1 | Cluster),family = "nbinom1",data = my_data8)
      ICCZinb1a<-mean(my_data8$Outcome)*(exp(get_variance(Zinb1)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb1")$varcomp$Pi)/(mean(my_data8$Outcome)*(exp(get_variance(Zinb1)$var.random)-1)+1+icc_counts(my_data8,y="Outcome",id="Cluster",fam="zinb1")$varcomp$r+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb1")$varcomp$Pi)
      Zinb2<-glmmTMB(Outcome ~ Condition + (1 | Cluster), ziformula = ~ Condition + (1 | Cluster),family = "nbinom2",data = my_data8)
      ICCZinb2a<-mean(my_data8$Outcome)*(exp(get_variance(Zinb2)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb2")$varcomp$Pi)/(mean(my_data8$Outcome)*((icc_counts(my_data8,y="Outcome",id="Cluster",fam="zinb2")$varcomp$r+1)*exp(get_variance(Zinb2)$var.random)-1)+1+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb2")$varcomp$Pi)
      Gee_Poisson<-summary(geeglm(Outcome ~ Condition, id=Cluster, family=poisson, corstr = "exchangeable",data=my_data8))
      
      #Nagakawa
      compute_icc_nakagawa <- function(model, type = c("quasi-poisson", "nbinom")) {
        
        type <- match.arg(type)
        
        # Extract random effect variance
        var_random <- as.numeric(VarCorr(model)$cond[[1]])
        
        # Get predicted marginal mean
        mu <- mean(predict(model, type = "response"))
        
        # Identify family
        fam <- family(model)$family
        
        # ICC calculations
        
        if (type == "quasi-poisson") {
          # Estimate overdispersion from Poisson model
          pearson_resid <- residuals(model, type = "pearson")
          phi <- sum(pearson_resid^2) / df.residual(model)
          var_residual <- log(1 + phi / mu)
          icc <- var_random / (var_random + var_residual)
        }
        
        if (type == "nbinom") {
          theta <- 1 / sigma(model)^2
          var_residual <- log(1 + 1/mu + 1/theta)
          icc <- var_random / (var_random + var_residual)
        }
        
        return(list(ICC = icc,
                    var_random = var_random,
                    var_residual = var_residual))
      }
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("GLMM Poisson :", round(ICCPoissona[1],3), ", AIC (",performance::model_performance(Poisson)$AIC,")")})
      ICC11 <- renderText({paste("GLMM Quasi-Poisson (Log scale):", round(compute_icc_nakagawa(model_pois, type = "quasi-poisson")$ICC[1],3))})
      ICC2 <- renderText({paste("GLMM Zero-inflated Poisson :", round(ICCZipa[1],3), ", AIC (",performance::model_performance(Zip_Poisson)$AIC,")")})
      ICC3 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean:", round(ICCNbinom1a[1],3), ", AIC (",performance::model_performance(Nbinom1)$AIC,")")})
      ICC33 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean (Log scale):", round(compute_icc_nakagawa(Nbinom1, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(Nbinom1)$AIC,")")})
      ICC4 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean:", round(ICCNbinom2a[1],3), ", AIC (",performance::model_performance(Nbinom2)$AIC,")")})
      ICC44 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean (Log scale):", round(compute_icc_nakagawa(Nbinom2, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(Nbinom2)$AIC,")")})
      ICC5 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing linearly with the mean:", round(ICCZinb1a[1],3), ", AIC (",performance::model_performance(Zinb1)$AIC,")")})
      ICC6 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing quadratically with the mean:", round(ICCZinb2a[1],3), ", AIC (",performance::model_performance(Zinb2)$AIC,")")})
      ICC7 <-  renderText({paste("GEE Poisson :", round(Gee_Poisson$corr[1,1],3))})
      
      output$PrintICC8 <- renderUI({
        mylist <- c(ICC1(),ICC11(), ICC2(), ICC3(),ICC33(), ICC4(), ICC44(), ICC5(), ICC6(), ICC7())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data8))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data8$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data8$Cluster)))/mean(as.vector(table(my_data8$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details8 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$boxplot8 <- renderPlotly({
        ggplotly(ggplot(my_data8, aes(Cluster, Outcome)) +
                   geom_boxplot(aes(fill = Condition)) +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   theme_minimal()+
                   ggtitle("Outcome by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_blank())+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
      output$dist8 <- renderPlot({
        ggplot(my_data8, aes(Outcome)) + 
          geom_histogram(aes(fill=Condition), color="#e9ecef")+
          theme_minimal()+
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",min(my_data8$Outcome,na.rm=T),"\n","Q25 =",quantile(my_data8$Outcome, .25,na.rm=T),"\n","Median =",median(my_data8$Outcome, na.rm = TRUE),"\n","Mean =",mean(my_data8$Outcome,na.rm=T),"\n","Q75 =",quantile(my_data8$Outcome, .75,na.rm=T),"\n","Max =",max(my_data8$Outcome,na.rm=T))))+
          ggtitle("Outcome distribution by Condition") +
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data8
  })
  
  output$my_output_data8 <- renderTable({data8()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/Count
  ##################/by condition     
  #Excel file
  
  data88 <- reactive({
    if(input$Load88 == 0){return()}
    inFile <- input$file88
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load88
      my_data8 <- read_excel(inFile$datapath, col_names = input$header88)
      
      colnames(my_data8)[1] <- "Cluster" 
      colnames(my_data8)[2] <- "Outcome" 
      colnames(my_data8)[3] <- "Condition" 
      
      my_data8$Outcome<-as.numeric(my_data8$Outcome)
      my_data8$Condition<-as.factor(my_data8$Condition)
      my_data8$Cluster<-as.factor(my_data8$Cluster)
      
      Poisson<-glmer(Outcome ~ Condition + (1 | Cluster), family=poisson,data=my_data8)
      model_pois <- glmmTMB(Outcome ~ Condition + (1 | Cluster), family = poisson, data = my_data8)
      ICCPoissona<-mean(my_data8$Outcome)*(exp(get_variance(Poisson)$var.random)-1)/(mean(my_data8$Outcome)*(exp(get_variance(Poisson)$var.random)-1)+1)
      Zip_Poisson<-glmmTMB(Outcome ~ Condition + (1 | Cluster),ziformula=~Condition,family=poisson,data=my_data8)
      ICCZipa<-mean(my_data8$Outcome)*(exp(get_variance(Zip_Poisson)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zip")$varcomp$Pi)/(mean(my_data8$Outcome)*(exp(get_variance(Zip_Poisson)$var.random)-1)+1+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zip")$varcomp$Pi)
      Nbinom1<-glmmTMB(Outcome ~ Condition + (1 | Cluster), family = "nbinom1", data=my_data8)
      ICCNbinom1a<-mean(my_data8$Outcome)*(exp(get_variance(Nbinom1)$var.random)-1)/(mean(my_data8$Outcome)*(exp(get_variance(Nbinom1)$var.random)-1)+icc_counts(my_data8,y="Outcome",id="Cluster",fam="nbinom1")$varcomp$r+1)
      Nbinom2<-glmmTMB(Outcome ~ Condition + (1 | Cluster), family = "nbinom2", data=my_data8)
      ICCNbinom2a<-mean(my_data8$Outcome)*(exp(get_variance(Nbinom2)$var.random)-1)/(mean(my_data8$Outcome)*((icc_counts(my_data8,y="Outcome",id="Cluster",fam="nbinom2")$varcomp$r+1)*exp(get_variance(Nbinom2)$var.random)-1)+1)
      Zinb1<-glmmTMB(Outcome ~ Condition + (1 | Cluster), ziformula = ~ Condition + (1 | Cluster),family = "nbinom1",data = my_data8)
      ICCZinb1a<-mean(my_data8$Outcome)*(exp(get_variance(Zinb1)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb1")$varcomp$Pi)/(mean(my_data8$Outcome)*(exp(get_variance(Zinb1)$var.random)-1)+1+icc_counts(my_data8,y="Outcome",id="Cluster",fam="zinb1")$varcomp$r+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb1")$varcomp$Pi)
      Zinb2<-glmmTMB(Outcome ~ Condition + (1 | Cluster), ziformula = ~ Condition + (1 | Cluster),family = "nbinom2",data = my_data8)
      ICCZinb2a<-mean(my_data8$Outcome)*(exp(get_variance(Zinb2)$var.random)-1)*(1-icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb2")$varcomp$Pi)/(mean(my_data8$Outcome)*((icc_counts(my_data8,y="Outcome",id="Cluster",fam="zinb2")$varcomp$r+1)*exp(get_variance(Zinb2)$var.random)-1)+1+mean(my_data8$Outcome)*icc_counts(my_data8,y="Outcome",id="Cluster",fam = "zinb2")$varcomp$Pi)
      Gee_Poisson<-summary(geeglm(Outcome ~ Condition, id=Cluster, family=poisson, corstr = "exchangeable",data=my_data8))
      
      #Nagakawa
      compute_icc_nakagawa <- function(model, type = c("quasi-poisson", "nbinom")) {
        
        type <- match.arg(type)
        
        # Extract random effect variance
        var_random <- as.numeric(VarCorr(model)$cond[[1]])
        
        # Get predicted marginal mean
        mu <- mean(predict(model, type = "response"))
        
        # Identify family
        fam <- family(model)$family
        
        # ICC calculations
        
        if (type == "quasi-poisson") {
          # Estimate overdispersion from Poisson model
          pearson_resid <- residuals(model, type = "pearson")
          phi <- sum(pearson_resid^2) / df.residual(model)
          var_residual <- log(1 + phi / mu)
          icc <- var_random / (var_random + var_residual)
        }
        
        if (type == "nbinom") {
          theta <- 1 / sigma(model)^2
          var_residual <- log(1 + 1/mu + 1/theta)
          icc <- var_random / (var_random + var_residual)
        }
        
        return(list(ICC = icc,
                    var_random = var_random,
                    var_residual = var_residual))
      }
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("GLMM Poisson :", round(ICCPoissona[1],3), ", AIC (",performance::model_performance(Poisson)$AIC,")")})
      ICC11 <- renderText({paste("GLMM Quasi-Poisson (Log scale):", round(compute_icc_nakagawa(model_pois, type = "quasi-poisson")$ICC[1],3))})
      ICC2 <- renderText({paste("GLMM Zero-inflated Poisson :", round(ICCZipa[1],3), ", AIC (",performance::model_performance(Zip_Poisson)$AIC,")")})
      ICC3 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean:", round(ICCNbinom1a[1],3), ", AIC (",performance::model_performance(Nbinom1)$AIC,")")})
      ICC33 <- renderText({paste("GLMM Negative Binomial with variance increasing linearly with the mean (Log scale):", round(compute_icc_nakagawa(Nbinom1, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(Nbinom1)$AIC,")")})
      ICC4 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean:", round(ICCNbinom2a[1],3), ", AIC (",performance::model_performance(Nbinom2)$AIC,")")})
      ICC44 <- renderText({paste("GLMM Negative Binomial with variance increasing quadratically with the mean (Log scale):", round(compute_icc_nakagawa(Nbinom2, type = "nbinom")$ICC[1],3), ", AIC (",performance::model_performance(Nbinom2)$AIC,")")})
      ICC5 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing linearly with the mean:", round(ICCZinb1a[1],3), ", AIC (",performance::model_performance(Zinb1)$AIC,")")})
      ICC6 <-  renderText({paste("GLMM Zero-inflated Negative Binomial with variance increasing quadratically with the mean:", round(ICCZinb2a[1],3), ", AIC (",performance::model_performance(Zinb2)$AIC,")")})
      ICC7 <-  renderText({paste("GEE Poisson :", round(Gee_Poisson$corr[1,1],3))})
      
      output$PrintICC88 <- renderUI({
        mylist <- c(ICC1(),ICC11(), ICC2(), ICC3(),ICC33(), ICC4(), ICC44(), ICC5(), ICC6(), ICC7())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data8))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data8$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data8$Cluster)))/mean(as.vector(table(my_data8$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details88 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$boxplot88 <- renderPlotly({
        ggplotly(ggplot(my_data8, aes(Cluster, Outcome)) +
                   geom_boxplot(aes(fill = Condition)) +
                   xlab("Cluster") +
                   ylab("Outcome") +
                   theme_minimal()+
                   ggtitle("Outcome by cluster by condition")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_blank())+
                   facet_wrap(. ~ Condition, scales = 'free_x',strip.position = 'right'))
      })
      
      output$dist88 <- renderPlot({
        ggplot(my_data8, aes(Outcome)) + 
          geom_histogram(aes(fill=Condition), color="#e9ecef")+
          theme_minimal()+
          geom_text_npc(size=5,aes(npcx = "right", npcy = "top", label = paste("Min =",min(Outcome,na.rm=T),"\n","Q25 =",quantile(Outcome, .25,na.rm=T),"\n","Median =",median(Outcome, na.rm = TRUE),"\n","Mean =",mean(Outcome,na.rm=T),"\n","Q75 =",quantile(Outcome, .75,na.rm=T),"\n","Max =",max(Outcome,na.rm=T))))+
          ggtitle("Outcome distribution by Condition") +
          theme(plot.title = element_text(hjust = 0.5,face="bold"),text = element_text(size=17.5))
      })
      
    })
    my_data8
  })
  
  output$my_output_data88 <- renderTable({data88()},include.rownames=FALSE, colnames =T)
  
  
  #14) Count outcome by condition xl
  output$tb88 <- renderUI({
    if (is.null(data88()))
      tags$img(src="count int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC88"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot88"),tags$hr(),plotOutput("dist88")),tabPanel("Data", tableOutput("Details88"), tableOutput("my_output_data88")))
  })
  #15) Count outcome by condition csv
  output$tb8 <- renderUI({
    if (is.null(data8()))
      tags$img(src="count int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC8"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("boxplot8"),tags$hr(),plotOutput("dist8")),tabPanel("Data", tableOutput("Details8"), tableOutput("my_output_data8")))
  })
  
  
  
  ###################Data
  ##################/TTE
  ##################/Same condition     
  #Other file
  
  data11b <- reactive({
    if(input$Load11b == 0){return()}
    inFile <- input$file11b
    if (is.null(inFile)){return(NULL)}
    
    isolate({
      input$Load11b
      my_data11 <- read.table(inFile$datapath,sep = input$sep11b,header=input$header11b)
      
      my_data11 <- my_data11 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3])%>%
        mutate_at(vars(c('Time', 'Censoring')), as.numeric)%>%
        mutate(Cluster=as.factor(Cluster))
      
      # ::: ANOVA method for censoring indicator:::
      # Cluster ID
      cid <- my_data11$Cluster
      # Response variable
      y <- my_data11$Censoring
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.bin <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: ANOVA method for observed event times:::
      Uncensored <- my_data11 %>% filter(my_data11$Censoring==0)
      
      cid <- Uncensored$Cluster
      # Response variable
      y <- Uncensored$Time
      # Number off clusters
      r <- length(unique(cid))
      # Number of observations in each cluster
      mi <- as.vector(table(cid))
      # Total number of observations
      N_star <- sum(ni)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      
      m0 <- (1/(r - 1))*(N_star - sum((mi^2)/N_star))
      ti <- aggregate(y, by = list(cid), mean)[ , 2]
      T<- mean(y)
      msb <- (1/(r - 1))*(sum(ti-T)^2)
      msw <- (1/(N_star - r))*(sum(y-ti)^2)
      rho.time <- (msb - msw)/(msb + (m0 - 1)*msw)
      
      
      c<-sum(my_data11$Censoring==1)/nrow(my_data11)
      
      icc_formula <- function(c, rho) {
        term1 <- c * log(c)^2
        term2 <- rho
        term3 <- (rho^2 / 4) * (2 + log(c))^2
        term4 <- (rho^3 / 9) * (3 + 3 * log(c) + 1/2 * log(c)^2)^2
        numerator <- term1 * (term2 + term3 + term4)
        denominator <- 1 - c
        
        P_c <- numerator / denominator
        return(P_c)
      }
      
      rho.cens <- icc_formula(c, rho.time)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Binary ICC of censoring indicator :", round(rho.bin,3))})
      ICC2 <- renderText({paste("ICC ignoring censored observations :", round(rho.time,3))})
      ICC3 <- renderText({paste("ICC using censoring indicator :", round(rho.cens,3))})
      
      output$PrintICC11b <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data11))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data11$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data11$Cluster)))/mean(as.vector(table(my_data11$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details11b <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$survplot11b <- renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data11 )
        # Extract data from survfit object
        surv_df <- data.frame(
          time = fit$time,
          surv = fit$surv,
          strata = rep(names(fit$strata), fit$strata)
        )
        
        # Create a separate data frame for each cluster
        surv_list <- split(surv_df, surv_df$strata)
        
        # Create plotly plot
        plt <- plot_ly()
        
        for (cluster in names(surv_list)) {
          df <- surv_list[[cluster]]
          plt <- plt %>% add_lines(x = df$time, y = df$surv,
                                   name = cluster,
                                   line = list(width = 2))
        }
        
        plt %>% layout(title = "Survival curves by Cluster",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Survival Probability"))
        
        
      })
      
    })
    my_data11
  })
  
  output$my_output_data11b <- renderTable({data11b()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/TTE
  ##################/Same condition     
  #Excel file
  
  data1111 <- reactive({
    if(input$Load1111 == 0){return()}
    inFile <- input$file1111
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load1111
      my_data11 <- read_excel(inFile$datapath, col_names = input$header1111)
      
      my_data11 <- my_data11 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3])%>%
        mutate_at(vars(c('Time', 'Censoring')), as.numeric)%>%
        mutate(Cluster=as.factor(Cluster))
      
      # ::: ANOVA method for censoring indicator:::
      # Cluster ID
      cid <- my_data11$Cluster
      # Response variable
      y <- my_data11$Censoring
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.bin <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: ANOVA method for observed event times:::
      Uncensored <- my_data11 %>% filter(my_data11$Censoring==0)
      
      cid <- Uncensored$Cluster
      # Response variable
      y <- Uncensored$Time
      # Number off clusters
      r <- length(unique(cid))
      # Number of observations in each cluster
      mi <- as.vector(table(cid))
      # Total number of observations
      N_star <- sum(ni)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      
      m0 <- (1/(r - 1))*(N_star - sum((mi^2)/N_star))
      ti <- aggregate(y, by = list(cid), mean)[ , 2]
      T<- mean(y)
      msb <- (1/(r - 1))*(sum(ti-T)^2)
      msw <- (1/(N_star - r))*(sum(y-ti)^2)
      rho.time <- (msb - msw)/(msb + (m0 - 1)*msw)
      
      c<-sum(my_data11$Censoring==1)/nrow(my_data11)
      
      icc_formula <- function(c, rho) {
        term1 <- c * log(c)^2
        term2 <- rho
        term3 <- (rho^2 / 4) * (2 + log(c))^2
        term4 <- (rho^3 / 9) * (3 + 3 * log(c) + 1/2 * log(c)^2)^2
        numerator <- term1 * (term2 + term3 + term4)
        denominator <- 1 - c
        
        P_c <- numerator / denominator
        return(P_c)
      }
      
      rho.cens <- icc_formula(c, rho.time)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Binary ICC of censoring indicator :", round(rho.bin,3))})
      ICC2 <- renderText({paste("ICC ignoring censored observations :", round(rho.time,3))})
      ICC3 <- renderText({paste("ICC using censoring indicator :", round(rho.cens,3))})
      
      output$PrintICC1111 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data11))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data11$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data11$Cluster)))/mean(as.vector(table(my_data11$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details1111 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      
      output$survplot1111 <- renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data11 )
        # Extract data from survfit object
        surv_df <- data.frame(
          time = fit$time,
          surv = fit$surv,
          strata = rep(names(fit$strata), fit$strata)
        )
        
        # Create a separate data frame for each cluster
        surv_list <- split(surv_df, surv_df$strata)
        
        # Create plotly plot
        plt <- plot_ly()
        
        for (cluster in names(surv_list)) {
          df <- surv_list[[cluster]]
          plt <- plt %>% add_lines(x = df$time, y = df$surv,
                                   name = cluster,
                                   line = list(width = 2))
        }
        
        plt %>% layout(title = "Survival curves by Cluster",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Survival Probability"))
      })
      
    })
    my_data11
  })
  
  output$my_output_data1111 <- renderTable({data1111()},include.rownames=FALSE, colnames =T)
  
  
  #12) TTE same condition xl
  output$tb1111 <- renderUI({
    if (is.null(data1111()))
      tags$img(src="TTE xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC1111"),uiOutput('boxU'),br(),br()),tabPanel("Graph",plotlyOutput("survplot1111")),tabPanel("Data", tableOutput("Details1111"), tableOutput("my_output_data1111")))
  })
  #13) TTE same condition csv
  output$tb11b <- renderUI({
    if (is.null(data11b()))
      tags$img(src="TTE xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC11b"),uiOutput('boxU'),br(),br()),tabPanel("Graphs",plotlyOutput("survplot11b")),tabPanel("Data", tableOutput("Details11b"), tableOutput("my_output_data11b")))
  })
  
  
  ###################Data
  ##################/TTE
  ##################/By condition     
  #Other file
  
  data12 <- reactive({
    if(input$Load12 == 0){return()}
    inFile <- input$file12
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load12
      my_data12 <- read.table(inFile$datapath,sep = input$sep12,header=input$header12)
      
      my_data12 <- my_data12 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3],Condition=names(.)[4])%>%
        mutate_all(as.numeric)
      
      shared_frailty_model <- coxph(Surv(Time, Censoring) ~ Condition
                                    +frailty(Cluster),
                                    data = my_data12,
                                    control =coxph.control(iter.max = 10000))
      
      icc.sf <- shared_frailty_model$var[1] / (shared_frailty_model$var[1] + pi^2 / 3)
      
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Shared frailty estimation :", round(icc.sf,3))})
      
      output$PrintICC12 <- renderUI({
        mylist <- c(ICC1())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data12))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data12$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data12$Cluster)))/mean(as.vector(table(my_data12$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details12 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$survplot12<-renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data12 )
        # Extract data from survfit object
        surv_df <- data.frame(
          time = fit$time,
          surv = fit$surv,
          strata = rep(names(fit$strata), fit$strata)
        )
        
        # Create a separate data frame for each cluster
        surv_list <- split(surv_df, surv_df$strata)
        
        # Create plotly plot
        plt <- plot_ly()
        
        for (cluster in names(surv_list)) {
          df <- surv_list[[cluster]]
          plt <- plt %>% add_lines(x = df$time, y = df$surv,
                                   name = cluster,
                                   line = list(width = 2))
        }
        
        plt %>% layout(title = "Survival curves by Cluster",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Survival Probability"))
      })
      
    })
    my_data12
  })
  
  output$my_output_data12 <- renderTable({data12()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/TTE
  ##################/by condition     
  #Excel file
  
  data1212 <- reactive({
    if(input$Load1212 == 0){return()}
    inFile <- input$file1212
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load1212
      my_data12 <- read_excel(inFile$datapath, col_names = input$header1212)
      
      my_data12 <- my_data12 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3],Condition=names(.)[4])%>%
        mutate_all(as.numeric)
      
      shared_frailty_model <- coxph(Surv(Time, Censoring) ~ Condition
                                    +frailty(Cluster),
                                    data = my_data12,
                                    control =coxph.control(iter.max = 10000))
      
      
      icc.sf <- shared_frailty_model$var[1] / (shared_frailty_model$var[1] + pi^2 / 3)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Shared frailty estimation :", round(icc.sf,3))})
      
      output$PrintICC1212 <- renderUI({
        mylist <- c(ICC1())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data12))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data12$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data12$Cluster)))/mean(as.vector(table(my_data12$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details1212 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$survplot1212 <- renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data12 )
        # Extract data from survfit object
        surv_df <- data.frame(
          time = fit$time,
          surv = fit$surv,
          strata = rep(names(fit$strata), fit$strata)
        )
        
        # Create a separate data frame for each cluster
        surv_list <- split(surv_df, surv_df$strata)
        
        # Create plotly plot
        plt <- plot_ly()
        
        for (cluster in names(surv_list)) {
          df <- surv_list[[cluster]]
          plt <- plt %>% add_lines(x = df$time, y = df$surv,
                                   name = cluster,
                                   line = list(width = 2))
        }
        
        plt %>% layout(title = "Survival curves by Cluster",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Survival Probability"))
      })
      
    })
    my_data12
  })
  
  output$my_output_data1212 <- renderTable({data1212()},include.rownames=FALSE, colnames =T)
  
  
  #14) TTE outcome by condition xl
  output$tb1212 <- renderUI({
    if (is.null(data1212()))
      tags$img(src="TTE int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC1212"),uiOutput('boxU'),br(),br()),tabPanel("Graphs", plotlyOutput("survplot1212")),tabPanel("Data", tableOutput("Details1212"), tableOutput("my_output_data1212")))
  })
  #15) TTE outcome by condition csv
  output$tb12 <- renderUI({
    if (is.null(data12()))
      tags$img(src="TTE int xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC12"),uiOutput('boxU'),br(),br()),tabPanel("Graphs",plotlyOutput("survplot12")),tabPanel("Data", tableOutput("Details12"), tableOutput("my_output_data12")))
  })
  
  
  
}

shinyApp(ui = ui, server = server)

