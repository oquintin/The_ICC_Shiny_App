
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
  
