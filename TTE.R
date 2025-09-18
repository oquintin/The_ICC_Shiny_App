
  ###################Data
  ##################/TTE
  ##################/Same condition     
  #Other file
  
  data11b <- reactive({
    if(input$Load11b == 0){return()}
    inFile <- input$file11b
    if (is.null(inFile)){return(NULL)}
    
    isolate({
      input$Load11b
      my_data11 <- read.table(inFile$datapath,sep = input$sep11b,header=input$header11b)
      
      my_data11 <- my_data11 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3])%>%
        mutate_at(vars(c('Time', 'Censoring')), as.numeric)%>%
        mutate(Cluster=as.factor(Cluster))
      
      # ::: ANOVA method for censoring indicator:::
      # Cluster ID
      cid <- my_data11$Cluster
      # Response variable
      y <- my_data11$Censoring
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.bin <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: ANOVA method for observed event times:::
      Uncensored <- my_data11 %>% filter(my_data11$Censoring==0)
      
      cid <- Uncensored$Cluster
      # Response variable
      y <- Uncensored$Time
      # Number off clusters
      r <- length(unique(cid))
      # Number of observations in each cluster
      mi <- as.vector(table(cid))
      # Total number of observations
      N_star <- sum(ni)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      
      m0 <- (1/(r - 1))*(N_star - sum((mi^2)/N_star))
      ti <- aggregate(y, by = list(cid), mean)[ , 2]
      T<- mean(y)
      msb <- (1/(r - 1))*(sum(ti-T)^2)
      msw <- (1/(N_star - r))*(sum(y-ti)^2)
      rho.time <- (msb - msw)/(msb + (m0 - 1)*msw)
      
      
      c<-sum(my_data11$Censoring==1)/nrow(my_data11)
      
      icc_formula <- function(c, rho) {
        term1 <- c * log(c)^2
        term2 <- rho
        term3 <- (rho^2 / 4) * (2 + log(c))^2
        term4 <- (rho^3 / 9) * (3 + 3 * log(c) + 1/2 * log(c)^2)^2
        numerator <- term1 * (term2 + term3 + term4)
        denominator <- 1 - c
        
        P_c <- numerator / denominator
        return(P_c)
      }
      
      rho.cens <- icc_formula(c, rho.time)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Binary ICC of censoring indicator :", round(rho.bin,3))})
      ICC2 <- renderText({paste("ICC ignoring censored observations :", round(rho.time,3))})
      ICC3 <- renderText({paste("ICC using censoring indicator :", round(rho.cens,3))})
      
      output$PrintICC11b <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data11))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data11$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data11$Cluster)))/mean(as.vector(table(my_data11$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details11b <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      output$survplot11b <- renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data11 )
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
    my_data11
  })
  
  output$my_output_data11b <- renderTable({data11b()},include.rownames=FALSE, colnames =T)
  
  ###################Data
  ##################/TTE
  ##################/Same condition     
  #Excel file
  
  data1111 <- reactive({
    if(input$Load1111 == 0){return()}
    inFile <- input$file1111
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load1111
      my_data11 <- read_excel(inFile$datapath, col_names = input$header1111)
      
      my_data11 <- my_data11 %>%
        rename(Cluster = names(.)[1],Time= names(.)[2],Censoring=names(.)[3])%>%
        mutate_at(vars(c('Time', 'Censoring')), as.numeric)%>%
        mutate(Cluster=as.factor(Cluster))
      
      # ::: ANOVA method for censoring indicator:::
      # Cluster ID
      cid <- my_data11$Cluster
      # Response variable
      y <- my_data11$Censoring
      # Number off clusters
      k <- length(unique(cid))
      # Number of observations in each cluster
      ni <- as.vector(table(cid))
      # Total number of observations
      N <- sum(ni)
      
      n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      yisq <- yi^2
      msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
      msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
      rho.bin <- (msb - msw)/(msb + (n0 - 1)*msw)
      
      # ::: ANOVA method for observed event times:::
      Uncensored <- my_data11 %>% filter(my_data11$Censoring==0)
      
      cid <- Uncensored$Cluster
      # Response variable
      y <- Uncensored$Time
      # Number off clusters
      r <- length(unique(cid))
      # Number of observations in each cluster
      mi <- as.vector(table(cid))
      # Total number of observations
      N_star <- sum(ni)
      yi <- aggregate(y, by = list(cid), sum)[ , 2]
      
      m0 <- (1/(r - 1))*(N_star - sum((mi^2)/N_star))
      ti <- aggregate(y, by = list(cid), mean)[ , 2]
      T<- mean(y)
      msb <- (1/(r - 1))*(sum(ti-T)^2)
      msw <- (1/(N_star - r))*(sum(y-ti)^2)
      rho.time <- (msb - msw)/(msb + (m0 - 1)*msw)
      
      c<-sum(my_data11$Censoring==1)/nrow(my_data11)
      
      icc_formula <- function(c, rho) {
        term1 <- c * log(c)^2
        term2 <- rho
        term3 <- (rho^2 / 4) * (2 + log(c))^2
        term4 <- (rho^3 / 9) * (3 + 3 * log(c) + 1/2 * log(c)^2)^2
        numerator <- term1 * (term2 + term3 + term4)
        denominator <- 1 - c
        
        P_c <- numerator / denominator
        return(P_c)
      }
      
      rho.cens <- icc_formula(c, rho.time)
      
      #Ouput the ICC          
      ICC1 <- renderText({paste("Binary ICC of censoring indicator :", round(rho.bin,3))})
      ICC2 <- renderText({paste("ICC ignoring censored observations :", round(rho.time,3))})
      ICC3 <- renderText({paste("ICC using censoring indicator :", round(rho.cens,3))})
      
      output$PrintICC1111 <- renderUI({
        mylist <- c(ICC1(), ICC2(), ICC3())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Ouput dataset details          
      Nrows <- renderText({paste("Number of rows in dataset :", nrow(my_data11))})
      Nclusters <- renderText({paste("Number of clusters :", length(unique(my_data11$Cluster)))})
      CV <- renderText({paste("Coefficient of variation of cluster sizes :", round(sd(as.vector(table(my_data11$Cluster)))/mean(as.vector(table(my_data11$Cluster))),3),"(unequal cluster sizes is determined by a coefficient > 0.23)")})
      
      output$Details1111 <- renderUI({
        mylist <- c(Nrows(), Nclusters(), CV())
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
      })
      
      #Output graphs
      
      output$survplot1111 <- renderPlotly({
        fit<-survfit( Surv(Time, Censoring) ~ Cluster, data = my_data11 )
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
    my_data11
  })
  
  output$my_output_data1111 <- renderTable({data1111()},include.rownames=FALSE, colnames =T)
  

  #12) TTE same condition xl
  output$tb1111 <- renderUI({
    if (is.null(data1111()))
      tags$img(src="TTE xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC1111"),uiOutput('boxU'),br(),br()),tabPanel("Graph",plotlyOutput("survplot1111")),tabPanel("Data", tableOutput("Details1111"), tableOutput("my_output_data1111")))
  })
  #13) TTE same condition csv
  output$tb11b <- renderUI({
    if (is.null(data11b()))
      tags$img(src="TTE xlsx.png")
    else
      tabsetPanel(tabPanel("ICC estimates", tableOutput("PrintICC11b"),uiOutput('boxU'),br(),br()),tabPanel("Graphs",plotlyOutput("survplot11b")),tabPanel("Data", tableOutput("Details11b"), tableOutput("my_output_data11b")))
  })
