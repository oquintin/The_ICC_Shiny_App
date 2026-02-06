#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##Libraries to load
library(ICCbin)
library(statip)
library(geepack)
library(nlme)
library(lme4)
library(sjstats)
library(tidyverse)
library(stringr)
library(shiny)
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
library(writexl)
library(ordinal)
library(shinyjs)
library(bslib)
library(survival)
library(RColorBrewer)
library(redcapAPI) 


####CODE for the ICC database in the app######################################
#Connect to redcap database and display in shiny app
# REDCap API details
redcap_url <- ""
api_token <- ""

#Call user interface program
source("./Ui.R",local=T)

server = function(input, output,session) {

#Call server programs
source("./The_ICC_database.R",local=T)
source("./Uncertainty.R",local=T)
source("./Bayesian.R",local=T)
source("./Proportions.R",local=T)
source("./Binary.R",local=T)
source("./Binary by condition.R",local=T)
source("./Categorical.R",local=T)
source("./Categorical by condition.R",local=T)
source("./Continuous.R",local=T)
source("./Continuous by condition.R",local=T)
source("./Count.R",local=T)
source("./Count by condition.R",local=T)
source("./TTE.R",local=T)
source("./TTE by condition.R",local=T)
  
}

shinyApp(ui = ui, server = server)


