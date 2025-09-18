
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
