
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
