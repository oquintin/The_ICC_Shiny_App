
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
