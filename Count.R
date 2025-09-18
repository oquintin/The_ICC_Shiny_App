
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
