
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
