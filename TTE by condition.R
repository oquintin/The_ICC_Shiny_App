
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
  
