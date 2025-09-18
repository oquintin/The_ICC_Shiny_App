
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
