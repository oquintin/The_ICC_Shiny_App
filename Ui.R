



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
               tabPanel("Data",fluidPage(em("Estimate the ICC with your own dataset. Please, answer the questions below..."),br(),br(),

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
                                           h5("Your data must be disposed in a vertical manner with three columns. The first column should contain the cluster identifiers, the second one the outcome with 0 or 1 values (missing values are also accepted) and the third the condition status. See image below for example.",a("See article for more information on ICC estimates", href = "https://doi.org/10.1177/1740774510392256",
                                                                                                                                                                                                                                                                                                                                 target = "_blank")),
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
                                         
               ))
               ,tabPanel("The ICC database",fluidPage(em("This is a database of ICC estimates added by users. You can add your own estimate by clicking on 'Add ICC estimate'. Thank you!"),br(),br(),
                 tags$a(
                   href = "https://trials2.pctu.qmul.ac.uk/surveys/?s=JLFXPRK74DT3RNJW",
                   target = "_blank",   # opens in new tab
                   class = "btn btn-default",  # Bootstrap button styling
                   "Add ICC estimate"
                 ),tags$hr(),
                 DTOutput("redcap_table"), tags$hr())
               ))


