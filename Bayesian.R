

###Template to download
  output$downloadOP <- downloadHandler(
    filename = "Previous ICC templates.xlsx",
    content = function(file) {
      file.copy("Previous_estimates_template.xlsx", file)
    }
  )
  

  ################
  ##################Bayesian Approach
  
  data3 <- reactive({
    if(input$Load3 == 0){return()}
    inFile <- input$file3
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load3
      my_data3 <- read_excel(inFile$datapath)
      
      my_data3$`Outcome ID` <-as.factor(my_data3$`Outcome ID`)
      my_data3$`Study ID` <-as.factor(my_data3$`Study ID`)
      my_data3$`Outcome weight num` <- ifelse(my_data3$`Outcome weight` =="Same outcome", 1, 
                                              ifelse(my_data3$`Outcome weight` =="Similar outcome", 0.7,
                                                     ifelse(my_data3$`Outcome weight` =="Outcome somewhat similar", 0.5, 0.2)))
      my_data3$`Study weight num` <- ifelse(my_data3$`Study weight` =="Very similar study", 1, 
                                            ifelse(my_data3$`Study weight` =="Similar study", 0.7,
                                                   ifelse(my_data3$`Study weight` =="Study somewhat similar", 0.5, 0.2)))
      
      my_data3$ICC <- as.numeric(my_data3$ICC)
      my_data3$N <- as.numeric(my_data3$N)
      my_data3$K <- as.numeric(my_data3$K)
      my_data3$`Outcome weight num` <- as.numeric(my_data3$`Outcome weight num`)
      my_data3$`Study weight num` <- as.numeric(my_data3$`Study weight num`)
      
      my_data3$Var <- (2*(my_data3$N-1)*(1-my_data3$ICC)^(2)*(1+((my_data3$N/my_data3$K)-1)*my_data3$ICC)^(2))/((my_data3$N/my_data3$K)^(2)*(my_data3$N-my_data3$K)*(my_data3$K-1))
      my_data3$Precision <- 1/my_data3$Var
      
      
      datax1 <- list(N=length(my_data3$ICC), ICC=my_data3$ICC, Precision=my_data3$Precision)
      
      model1 <- function() {
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu,Precision2)
        }
        
        # priors for the parameters
        mu~dnorm(0,0.001)
        sigma~dunif(0,5)
        Precision2<-pow(sigma,-2)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu=-2.95)
      init2 <-list(mu=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      model_1 <- jags(data=datax1,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=model1,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med1 <- exp(model_1$BUGSoutput[13]$median$mu)/(1+exp(model_1$BUGSoutput[13]$median$mu))
      Inf1 <- exp(model_1$BUGSoutput$summary[2,3])/(1+exp(model_1$BUGSoutput$summary[2,3]))
      Sup1 <- exp(model_1$BUGSoutput$summary[2,7])/(1+exp(model_1$BUGSoutput$summary[2,7]))
      
      
      res <- as.mcmc(model_1)
      res <- rbind(res[[1]],res[[2]])
      ICC <- exp(res[,"mu"])/(1+exp(res[,"mu"]))   
      
      p1<- ggplot() +
        geom_histogram(aes(x=ICC),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf1,"\n","Median =",Med1,"\n","97.5% =",Sup1)))+
        ggtitle("Model 1 (Regardless of outcome and study type)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      
      datax2 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`))
      
      Model2 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2)
          mu[i]<- a[s[i]] 
        }
        
        # priors for the parameters
        Precision2<-pow(sigma,-2)
        sigma~dunif(0,5)
        
        #likelihood 2
        for (j in 1:S)
        {
          a[j]~dnorm(mu.a,tau.a)
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a <- pow(sigma.a,-2)
        sigma.a~dunif(0,5)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_2 <- jags(data=datax2,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model2,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med2 <- exp(Model_2$BUGSoutput[13]$median$mu)/(1+exp(Model_2$BUGSoutput[13]$median$mu))
      Inf2 <- exp(Model_2$BUGSoutput$summary[2,3])/(1+exp(Model_2$BUGSoutput$summary[2,3]))
      Sup2 <- exp(Model_2$BUGSoutput$summary[2,7])/(1+exp(Model_2$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_2)
      res <- rbind(res[[1]],res[[2]])
      
      ICC2 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p2<-ggplot() +
        geom_histogram(aes(x=ICC2),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf2,"\n","Median =",Med2,"\n","97.5% =",Sup2)))+
        ggtitle("Model 2 (Exchangeability between and within studies)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      datax3 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`),wl=my_data3$`Outcome weight num`,ws=my_data3$`Study weight num`)
      
      Model3 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2[i]*wl[i])
          mu[i]<- a[s[i]] 
          
          # priors for the parameters
          Precision2[i]<-pow(sigma[i],-2)
          sigma[i]~dunif(0,5)
        }
        
        #likelihood 2
        for (j in 1:S)
        {
          a[j]~dnorm(mu.a,tau.a*ws[j])
          
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a<-pow(sigma.a,-2)
        sigma.a~dunif(0,5)
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_3 <- jags(data=datax3,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model3,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med3 <- exp(Model_3$BUGSoutput[13]$median$mu)/(1+exp(Model_3$BUGSoutput[13]$median$mu))
      Inf3 <- exp(Model_3$BUGSoutput$summary[2,3])/(1+exp(Model_3$BUGSoutput$summary[2,3]))
      Sup3 <- exp(Model_3$BUGSoutput$summary[2,7])/(1+exp(Model_3$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_3)
      res <- rbind(res[[1]],res[[2]])
      
      ICC3 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p3<-ggplot() +
        geom_histogram(aes(x=ICC3),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf3,"\n","Median =",Med3,"\n","97.5% =",Sup3)))+
        ggtitle("Model 3 (Multiplicative weighting)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      datax4 <- list(N=length(my_data3$ICC),S=length(levels(my_data3$`Study ID`)),ICC=my_data3$ICC, Precision=my_data3$Precision, s=as.numeric(my_data3$`Study ID`),wl=my_data3$`Outcome weight num`,ws=my_data3$`Study weight num`)
      
      Model4 <- function() {
        #likelihood 1
        for (i in 1:N)
        {
          ICC[i]~dnorm(rho[i],Precision[i])
          logit(rho[i])<-logRho[i]
          logRho[i]~dnorm(mu[i],Precision2[i]*wl[i]/(wl[i]+lambda.l[i]*(1-wl[i])))
          mu[i]<- a[s[i]] 
          
          # priors for the parameters
          Precision2[i]<-pow(sigma[i],-2)
          sigma[i]~dunif(0,5)
          log.l[i]~dnorm(0,1)
          lambda.l[i]<- exp(log.l[i])
          
        }
        
        #likelihood 2
        for (j in 1:S)
        {
          
          a[j]~dnorm(mu.a,tau.a*ws[j]/(ws[j]+lambda.s*(1-ws[j])))
          
        }
        #priors
        mu.a~dnorm(0,0.001)
        tau.a<-pow(sigma.a,-2)
        sigma.a~dunif(0,5)
        log.s~dnorm(0,1)
        lambda.s<- exp(log.s)
        
      }
      
      #List of initial values (one for each MCMC chain)  
      init1 <-list(mu.a=-2.95)
      init2 <-list(mu.a=-4.60)
      inits <- list(init1,init2)
      
      
      #Specify the parameters that need to be estimated
      parameters <- c("mu.a")
      
      #Specify the parameters for the inference
      nb.burnin<-5000
      nb.iterations<-100000
      
      #Run Jags
      Model_4 <- jags(data=datax4,
                      inits= inits,
                      parameters.to.save = parameters,
                      model.file=Model4,
                      n.chains=2,
                      n.iter=nb.iterations,
                      n.burnin = nb.burnin)
      
      Med4 <- exp(Model_4$BUGSoutput[13]$median$mu)/(1+exp(Model_4$BUGSoutput[13]$median$mu))
      Inf4 <- exp(Model_4$BUGSoutput$summary[2,3])/(1+exp(Model_4$BUGSoutput$summary[2,3]))
      Sup4 <- exp(Model_4$BUGSoutput$summary[2,7])/(1+exp(Model_4$BUGSoutput$summary[2,7]))
      
      res <- as.mcmc(Model_4)
      res <- rbind(res[[1]],res[[2]])
      
      ICC4 <- exp(res[,"mu.a"])/(1+exp(res[,"mu.a"]))
      
      p4<-ggplot() +
        geom_histogram(aes(x=ICC4),fill="steelblue")+
        labs(y="Frequency")+
        labs(x="ICC")+
        theme_bw()+
        geom_text_npc(aes(npcx = "right", npcy = "top", label = paste("2.5% =",Inf4,"\n","Median =",Med4,"\n","97.5% =",Sup4)))+
        ggtitle("Model 4 (Additive weighting and uncertainty)")+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))
      
      output$plotM1_4 <- renderPlot({
        
        grid.arrange(p1, p2,p3, p4, ncol = 2)
      })
      
      output$plotM11_44 <- renderPlotly({
        
        p11 <- ggplotly(ggplot() +
                          geom_line(aes(ICC),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 1")+
                          theme_bw()+
                          ggtitle("Model 1 (Regardless of outcome and study type)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p22 <- ggplotly(ggplot() +
                          geom_line(aes(ICC2),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 2")+
                          theme_bw()+
                          ggtitle("Model 2 (Exchangeability between and within studies)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p33 <- ggplotly(ggplot() +
                          geom_line(aes(ICC3),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 3")+
                          theme_bw()+
                          ggtitle("Model 3 (Multiplicative weighting)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        p44 <- ggplotly(ggplot() +
                          geom_line(aes(ICC4),stat = "ecdf")+
                          labs(y="ecdf")+
                          labs(x="ICC Model 4")+
                          theme_bw()+
                          ggtitle("Model 4 (Additive weighting and uncertainty)")+
                          theme(plot.title = element_text(hjust = 0.5,face="bold")))
        
        subplot(p11,p22,p33,p44,nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.1)%>% 
          layout(title = 'Empirical cumulative distribution (ecdf)')
        
      })
      
    })
    my_data3
  })
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE, colnames =T)
  

  #11) Bayesian
  output$tb3 <- renderUI({
    if (is.null(data3()))
      h5("Please, wait for a  moment after loading your file, it will only take a minute.")
    else
      tabsetPanel(tabPanel("Results", plotOutput("plotM1_4"), plotlyOutput("plotM11_44")),tabPanel("Data", tableOutput("my_output_data3")))
  })
