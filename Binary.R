
  ########Binary
  ######/Same condition
  ######/csv
  
  data2 <- reactive({
    if(input$Load2 == 0){return()}
    inFile <- input$file2
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load2
      my_data2 <- read.table(inFile$datapath, header = input$header2, sep = input$sep2)
      
      colnames(my_data2)[1] <- "Cluster" 
      colnames(my_data2)[2] <- "Outcome" 
      
      my_data2$Outcome <- as.numeric(my_data2$Outcome)
      my_data2$Cluster <- factor(my_data2$Cluster,levels=unique(my_data2$Cluster))
      
      my_data2b<-na.omit(my_data2)
  
      colnames(my_data2b)[1] <- "cid" 
      colnames(my_data2b)[2] <- "y"     
      
      # Cluster ID
      cid <- my_data2b$cid
      # Response variable
      y <- my_data2b$y
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      # ::: ANOVA method :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: Modified ANOVA (Fleiss, 1981) :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msbs <- (1/(k))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aovs <- (msbs - msw)/(msbs + (n0 - 1)*msw)
      
      # ::: Moment estimators :::
      pii <- yi/ni
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.keq <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.kpr <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.keqs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.kprs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      kappa = 0.45
      p <- sum(yi)/sum(ni)
      wi <- ni/N
      sw <- sum(wi*(pii - piw)^2)
      rho.stab <- (1/(n0 - 1))*((N*sw)/((k - 1)*p*(1 - p)) + kappa - 1)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.ub <- 1 - (N*n0*(k - 1)*msw)/(sum(yi)*(n0*(k - 1) - sum(yi)) + sum(yisq))
     
      # ::: Estimators based on a direct probabilistic method :::
      # Fleiss and Cuzick (1979) Method 
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      piio <- sum(yi)/sum(ni)
      rho.fc <- 1 - (1/((N - k)*piio*(1 - piio)))*sum(yi*(ni - yi)/ni)
      
      # Mak (1988) method
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      ni <- as.vector(table(cid))
      rho.mak <- 1 - (k - 1)*sum((yi*(ni - yi))/(ni*(ni - 1)))/(sum(yisq/ni^2) + sum(yi/ni)*(k - 1 - sum(yi/ni)))
      
      # ::: Estimators based on direct calculation of correlation within each group :::
      # wi = constant and equal weight for every pair of observations; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.peq <- sum((ni - 1)*yi)/sum((ni - 1)*ni)
      rho.peq <- (1/(mu.peq*(1 - mu.peq)))*(sum(yi*(yi - 1))/sum(ni*(ni - 1)) - mu.peq^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.pgp <- sum(yi/ni)/k
      rho.pgp <- (1/(mu.pgp*(1 - mu.pgp)))*(sum((yi*(yi - 1))/(ni*(ni - 1)))/k - mu.pgp^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.ppr <- sum(yi)/N
      rho.ppr <- (1/(mu.ppr*(1 - mu.ppr)))*(sum(yi*(yi - 1)/(ni - 1))/N - mu.ppr^2)
      
      # ::: Estimators using ressampling method; Chakraborty & Sen (2016) :::
      # U ststistics
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      n <- sum(ni)
      u1 <- sum(yi)/N
      alp <- u1
      # Within cluster pairwise probabilities
      ucid <- sort(unique(cid))
      nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
      for(i in 1:k){
        dti <- my_data2b[cid == ucid[i], ]
        wsamp1 <- c()
        wsamp2 <- c()
        for(m in 1:(nrow(dti) -1)){
          wsamp1 <- c(wsamp1, rep(dti$y[m], length((m + 1):nrow(dti))))
          wsamp2 <- c(wsamp2, dti$y[(m + 1):nrow(dti)])
        }
        wsamp <- rbind(wsamp1, wsamp2)
        for(j in 1:ncol(wsamp)){
          if(all(wsamp[ , j] == c(0, 0)) == TRUE){
            nw00 <- nw00 + 1}
          else if(all(wsamp[ , j] == c(0, 1)) == TRUE){
            nw01 <- nw01 + 1}
          else if(all(wsamp[ , j] == c(1, 0)) == TRUE){
            nw10 <- nw10 + 1}
          else{nw11 <- nw11 + 1}
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nw11n <- nw11*2; nw00n <- nw00*2
      nw10n <- nw10 + nw01; nw01n <- nw01 + nw10
      uw11 <- nw11n/sum(ni*(ni - 1))
      uw10 <- nw10n/sum(ni*(ni - 1))
      uw01 <- nw01n/sum(ni*(ni - 1))
      uw00 <- nw00n/sum(ni*(ni - 1))
      tw <- uw11 + uw00 - uw10 - uw01
      
      # Between cluster pairwise probabilities
      nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
      for(i in 1:(k - 1)){
        dti <- my_data2b[cid == ucid[i], ]
        for(m in (i + 1):k){
          dtm <- my_data2b[cid == ucid[m], ]
          bsamp1 <- rep(dti$y, each = nrow(dtm))
          bsamp2 <-rep(dtm$y, times = nrow(dti))
          bsamp <- rbind(bsamp1, bsamp2)
          for(j in 1:ncol(bsamp)){
            if(all(bsamp[ , j] == c(0, 0)) == TRUE){
              nb00 <- nb00 + 1}
            else if(all(bsamp[ , j] == c(0, 1)) == TRUE){
              nb01 <- nb01 + 1}
            else if(all(bsamp[ , j] == c(1, 0)) == TRUE){
              nb10 <- nb10 + 1}
            else{nb11 <- nb11 + 1}
          }
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nb11n <- nb11*2; nb00n <- nb00*2
      nb10n <- nb10 + nb01; nb01n <- nb01 + nb10
      ub11 <- nb11n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub10 <- nb10n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub01 <- nb01n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub00 <- nb00n/(N*(N - 1) - sum(ni*(ni - 1)))
      tb <- ub11 + ub00 - ub10 - ub01
      
      rho.rm <- (tw - tb)/(4*u1*(1 - u1))
      
      # Model Linearization and Monte Carlo Simulation Methods; Goldstein et al. (2002)
      mmod <- lme4::glmer(Outcome ~ 1 + (1 | Cluster), family = binomial, data = my_data2, nAGQ = 1)
      fint <- lme4::fixef(mmod)
      re_var <- as.vector(lme4::VarCorr(mmod)[[1]])
      pr <- exp(fint)/(1 + exp(fint))
      sig1 <- pr*(1 - pr)
      sig2 <- re_var*pr^2*(1 + exp(fint))^(-2)
      rho.lin <- sig2/(sig1 + sig2)
      
      z <- rnorm(n = 1000, mean = 0, sd = sqrt(re_var))
      pr <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(pr*(1 - pr))
      sig2 <- var(pr)
      rho.sim <- sig2/(sig1 + sig2)
      
      
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data2,corstr="exch",family=binomial ,scale.fix = TRUE))
      glmm_mod  <- glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCfo <-bv/(bv+p*(1-p))
      
      colnames(my_data2b)[1] <- "Cluster" 
      colnames(my_data2b)[2] <- "Outcome"  
      my_data2b$Outcome <- as.factor(my_data2b$Outcome)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("ANOVA Estimate:", round(rho.aov,3))})
      ICC2 <- renderText({paste("Modified ANOVA Estimate:", round(rho.aovs,3))})
      ICC3 <- renderText({paste("Moment Estimate with Equal Weights:", round(rho.keq,3))})
      ICC4 <- renderText({paste("Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kpr,3))})
      ICC5 <- renderText({paste("Modified Moment Estimate with Equal Weights:", round(rho.keqs,3))})
      ICC6 <- renderText({paste("Modified Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kprs,3))})
      ICC7 <- renderText({paste("Stabilized Moment Estimate:", round(rho.stab,3))})
      ICC8 <- renderText({paste("Modified Moment Estimate from Unbiased Estimating Equation:", round(rho.ub,3))})
      ICC9 <- renderText({paste("Fleiss-Cuzick Kappa Type Estimate:", round(rho.fc,3))})
      ICC10 <- renderText({paste("Mak's Unweighted Average Estimate:", round(rho.mak,3))})
      ICC11 <- renderText({paste("Correlation Estimate with Equal Weight to Every Pair of Observations:", round(rho.peq,3))})
      ICC12 <- renderText({paste("Correlation Estimate with Equal Weight to Each Cluster Irrespective of Size:", round(rho.pgp,3))})
      ICC13 <- renderText({paste("Correlation Estimate with Weighting Each Pair According to Number of Pairs individuals Appear:", round(rho.ppr,3))})
      ICC14 <- renderText({paste("Resampling Estimate:", round(rho.rm,3))})
      ICC15 <- renderText({paste("First-order Model Linearized Estimate:", round(rho.lin,3))})
      ICC16 <- renderText({paste("Monte Carlo Simulation Estimate:", round(rho.sim,3))})
      ICC17 <-  renderText({paste("Mixed Model with logit link (log-odds scale):", round(icc(glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2, family = binomial))[1],3))})
      #ICC18 <-  renderText({paste("First-order Taylor extension with logit link:", round(ICCfo,3))})
      #ICC19 <-  renderText({paste("Estimate via Generalized Estimating Equations:", round(gee_mod$correlation[1,1],3))})
      
      output$PrintICC2 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5(), ICC6(), ICC7(), ICC8(), ICC9(), ICC10(), ICC11(), ICC12(), ICC13(), ICC14(), ICC15(), ICC16(), ICC17())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data2))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data2$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data2$Cluster)))/mean(as.vector(table(my_data2$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details2 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot4 <- renderPlotly({
        ggplotly(ggplot(my_data2b, aes(x = Cluster,
                                       fill = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data2
  })
  
  output$my_output_data2 <- renderTable({data2()},include.rownames=FALSE, colnames =T)
  
  ########Data/ Binary
  ######/Same condition
  #######/Excel 
  
  data22 <- reactive({
    if(input$Load22 == 0){return()}
    inFile <- input$file22
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load22
      my_data2 <- read_excel(inFile$datapath, col_names = input$header22)
      
      colnames(my_data2)[1] <- "Cluster" 
      colnames(my_data2)[2] <- "Outcome" 
      
      my_data2$Outcome <- as.numeric(my_data2$Outcome)
      my_data2$Cluster <- factor(my_data2$Cluster,levels=unique(my_data2$Cluster))
      
      my_data2b<-na.omit(my_data2)
      
      colnames(my_data2b)[1] <- "cid" 
      colnames(my_data2b)[2] <- "y"     
      
      # Cluster ID
      cid <- my_data2b$cid
      # Response variable
      y <- my_data2b$y
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      # ::: ANOVA method :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: Modified ANOVA (Fleiss, 1981) :::
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msbs <- (1/(k))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.aovs <- (msbs - msw)/(msbs + (n0 - 1)*msw)
      
      # ::: Moment estimators :::
      pii <- yi/ni
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.keq <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      rho.kpr <- (sw - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- rep(1/k, k)
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.keqs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      wi <- ni/N
      piw <- sum(wi*pii)
      sw <- sum(wi*(pii - piw)^2)
      swn <- (k - 1)*sw/k
      rho.kprs <- (swn - piw*(1 - piw)*sum(wi*(1 - wi)/ni))/(piw*(1 - piw)*(sum(wi*(1 - wi))) - sum(wi*(1 - wi)/ni))
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      kappa = 0.45
      p <- sum(yi)/sum(ni)
      wi <- ni/N
      sw <- sum(wi*(pii - piw)^2)
      rho.stab <- (1/(n0 - 1))*((N*sw)/((k - 1)*p*(1 - p)) + kappa - 1)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.ub <- 1 - (N*n0*(k - 1)*msw)/(sum(yi)*(n0*(k - 1) - sum(yi)) + sum(yisq))
      
      # ::: Estimators based on a direct probabilistic method :::
      # Fleiss and Cuzick (1979) Method 
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      piio <- sum(yi)/sum(ni)
      rho.fc <- 1 - (1/((N - k)*piio*(1 - piio)))*sum(yi*(ni - yi)/ni)
      
      # Mak (1988) method
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      ni <- as.vector(table(cid))
      rho.mak <- 1 - (k - 1)*sum((yi*(ni - yi))/(ni*(ni - 1)))/(sum(yisq/ni^2) + sum(yi/ni)*(k - 1 - sum(yi/ni)))
      
      # ::: Estimators based on direct calculation of correlation within each group :::
      # wi = constant and equal weight for every pair of observations; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.peq <- sum((ni - 1)*yi)/sum((ni - 1)*ni)
      rho.peq <- (1/(mu.peq*(1 - mu.peq)))*(sum(yi*(yi - 1))/sum(ni*(ni - 1)) - mu.peq^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.pgp <- sum(yi/ni)/k
      rho.pgp <- (1/(mu.pgp*(1 - mu.pgp)))*(sum((yi*(yi - 1))/(ni*(ni - 1)))/k - mu.pgp^2)
      
      # wi = 1/(k*ni*(ni - 1)) and equal weight for each group regardless of cluster size; Ridout, Demetrio, et al. (1999)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      mu.ppr <- sum(yi)/N
      rho.ppr <- (1/(mu.ppr*(1 - mu.ppr)))*(sum(yi*(yi - 1)/(ni - 1))/N - mu.ppr^2)
      
      # ::: Estimators using ressampling method; Chakraborty & Sen (2016) :::
      # U ststistics
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      ni <- as.vector(table(cid))
      n <- sum(ni)
      u1 <- sum(yi)/N
      alp <- u1
      # Within cluster pairwise probabilities
      ucid <- sort(unique(cid))
      nw11 <- 0; nw10 <- 0; nw01 <- 0; nw00 <- 0
      for(i in 1:k){
        dti <- my_data2b[cid == ucid[i], ]
        wsamp1 <- c()
        wsamp2 <- c()
        for(m in 1:(nrow(dti) -1)){
          wsamp1 <- c(wsamp1, rep(dti$y[m], length((m + 1):nrow(dti))))
          wsamp2 <- c(wsamp2, dti$y[(m + 1):nrow(dti)])
        }
        wsamp <- rbind(wsamp1, wsamp2)
        for(j in 1:ncol(wsamp)){
          if(all(wsamp[ , j] == c(0, 0)) == TRUE){
            nw00 <- nw00 + 1}
          else if(all(wsamp[ , j] == c(0, 1)) == TRUE){
            nw01 <- nw01 + 1}
          else if(all(wsamp[ , j] == c(1, 0)) == TRUE){
            nw10 <- nw10 + 1}
          else{nw11 <- nw11 + 1}
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nw11n <- nw11*2; nw00n <- nw00*2
      nw10n <- nw10 + nw01; nw01n <- nw01 + nw10
      uw11 <- nw11n/sum(ni*(ni - 1))
      uw10 <- nw10n/sum(ni*(ni - 1))
      uw01 <- nw01n/sum(ni*(ni - 1))
      uw00 <- nw00n/sum(ni*(ni - 1))
      tw <- uw11 + uw00 - uw10 - uw01
      
      # Between cluster pairwise probabilities
      nb11 <- 0; nb10 <- 0; nb01 <- 0; nb00 <- 0
      for(i in 1:(k - 1)){
        dti <- my_data2b[cid == ucid[i], ]
        for(m in (i + 1):k){
          dtm <- my_data2b[cid == ucid[m], ]
          bsamp1 <- rep(dti$y, each = nrow(dtm))
          bsamp2 <-rep(dtm$y, times = nrow(dti))
          bsamp <- rbind(bsamp1, bsamp2)
          for(j in 1:ncol(bsamp)){
            if(all(bsamp[ , j] == c(0, 0)) == TRUE){
              nb00 <- nb00 + 1}
            else if(all(bsamp[ , j] == c(0, 1)) == TRUE){
              nb01 <- nb01 + 1}
            else if(all(bsamp[ , j] == c(1, 0)) == TRUE){
              nb10 <- nb10 + 1}
            else{nb11 <- nb11 + 1}
          }
        }
      }
      # The frequencies are doubled as originiallny they were computed
      # as half in above loop
      nb11n <- nb11*2; nb00n <- nb00*2
      nb10n <- nb10 + nb01; nb01n <- nb01 + nb10
      ub11 <- nb11n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub10 <- nb10n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub01 <- nb01n/(N*(N - 1) - sum(ni*(ni - 1)))
      ub00 <- nb00n/(N*(N - 1) - sum(ni*(ni - 1)))
      tb <- ub11 + ub00 - ub10 - ub01
      
      rho.rm <- (tw - tb)/(4*u1*(1 - u1))
      
      # Model Linearization and Monte Carlo Simulation Methods; Goldstein et al. (2002)
      mmod <- lme4::glmer(Outcome ~ 1 + (1 | Cluster), family = binomial, data = my_data2, nAGQ = 1)
      fint <- lme4::fixef(mmod)
      re_var <- as.vector(lme4::VarCorr(mmod)[[1]])
      pr <- exp(fint)/(1 + exp(fint))
      sig1 <- pr*(1 - pr)
      sig2 <- re_var*pr^2*(1 + exp(fint))^(-2)
      rho.lin <- sig2/(sig1 + sig2)
      
      z <- rnorm(n = 1000, mean = 0, sd = sqrt(re_var))
      pr <- exp(fint + z)/(1 + exp(fint + z))
      sig1 <- mean(pr*(1 - pr))
      sig2 <- var(pr)
      rho.sim <- sig2/(sig1 + sig2)
      
      
      gee_mod<-summary(geese(Outcome~1,id=Cluster,data=my_data2,corstr="exch",family=binomial ,scale.fix = TRUE))
      glmm_mod  <- glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2,family=binomial)
      p<-exp(fixef(glmm_mod)[[1]])/(1+exp(fixef(glmm_mod)[[1]]))
      bv<-(get_variance(glmm_mod)$var.random)*(p*(1-p))^2
      ICCfo <-bv/(bv+p*(1-p))
      
      colnames(my_data2b)[1] <- "Cluster" 
      colnames(my_data2b)[2] <- "Outcome"  
      my_data2b$Outcome <- as.factor(my_data2b$Outcome)
      
      #Output the ICC          
      ICC1 <- renderText({paste("ANOVA Estimate:", round(rho.aov,3))})
      ICC2 <- renderText({paste("Modified ANOVA Estimate:", round(rho.aovs,3))})
      ICC3 <- renderText({paste("Moment Estimate with Equal Weights:", round(rho.keq,3))})
      ICC4 <- renderText({paste("Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kpr,3))})
      ICC5 <- renderText({paste("Modified Moment Estimate with Equal Weights:", round(rho.keqs,3))})
      ICC6 <- renderText({paste("Modified Moment Estimate with Weights Proportional to Cluster Size:", round(rho.kprs,3))})
      ICC7 <- renderText({paste("Stabilized Moment Estimate:", round(rho.stab,3))})
      ICC8 <- renderText({paste("Modified Moment Estimate from Unbiased Estimating Equation:", round(rho.ub,3))})
      ICC9 <- renderText({paste("Fleiss-Cuzick Kappa Type Estimate:", round(rho.fc,3))})
      ICC10 <- renderText({paste("Mak's Unweighted Average Estimate:", round(rho.mak,3))})
      ICC11 <- renderText({paste("Correlation Estimate with Equal Weight to Every Pair of Observations:", round(rho.peq,3))})
      ICC12 <- renderText({paste("Correlation Estimate with Equal Weight to Each Cluster Irrespective of Size:", round(rho.pgp,3))})
      ICC13 <- renderText({paste("Correlation Estimate with Weighting Each Pair According to Number of Pairs individuals Appear:", round(rho.ppr,3))})
      ICC14 <- renderText({paste("Resampling Estimate:", round(rho.rm,3))})
      ICC15 <- renderText({paste("First-order Model Linearized Estimate:", round(rho.lin,3))})
      ICC16 <- renderText({paste("Monte Carlo Simulation Estimate:", round(rho.sim,3))})
      ICC17 <-  renderText({paste("Mixed Model with logit link (log-odds scale):", round(icc(glmer(Outcome ~ 1 + (1 | Cluster), data = my_data2, family = binomial))[1],3))})
      #ICC18 <-  renderText({paste("First-order Taylor extension with logit link:", round(ICCfo,3))})
      #ICC19 <-  renderText({paste("Estimate via Generalized Estimating Equations:", round(gee_mod$correlation[1,1],3))})
      output$PrintICC22 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3(), ICC4(), ICC5(), ICC6(), ICC7(), ICC8(), ICC9(), ICC10(), ICC11(), ICC12(), ICC13(), ICC14(), ICC15(), ICC16(), ICC17())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data2))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data2$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data2$Cluster)))/mean(as.vector(table(my_data2$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details22 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graph
      output$plot44 <- renderPlotly({
        ggplotly(ggplot(my_data2b, aes(x = Cluster,
                                       fill = Outcome)) +
                   geom_bar(position = "fill",
                            stat= "count") +
                   labs(y="Proportions")+
                   labs(x="Cluster")+
                   labs(fill = "Outcome Category")+
                   scale_fill_brewer(palette=1)+
                   theme_minimal()+
                   ggtitle("Outcome by Cluster")+
                   theme(plot.title = element_text(hjust = 0.5,face="bold"),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
      })
      
    })
    my_data2
  })
  
  output$my_output_data22 <- renderTable({data22()},include.rownames=FALSE, colnames =T)
  
  #3) Binary outcome csv
  output$tb2 <- renderUI({
    if (!is.null(data2())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC2"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot4")),tabPanel("Data", tableOutput("Details2"),tableOutput("my_output_data2")))
    else 
      tags$img(src="bin_xlsx.png")
  })
  #4) Binary outcome xl
  output$tb22 <- renderUI({
    if (!is.null(data22())) 
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC22"),uiOutput('boxU'),br(),br()),tabPanel("Graph", plotlyOutput("plot44")),tabPanel("Data", tableOutput("Details22"), tableOutput("my_output_data22")))
    else 
      tags$img(src="bin_xlsx.png")
  })

