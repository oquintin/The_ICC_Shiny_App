
  #####################Uncertainty
  rv <- reactive({
    tibble::tibble(ICC = input$ICC,N = input$N,K = input$K, 
                   #Swiger
                   Var1= (2*(N-1)*(1-ICC)^2*(1+((N/K)-1)*ICC)^2)/((N/K)^2*(N-K)*(K-1)),
                   lower1=ICC-1.96*sqrt(Var1),
                   upper1=ICC+1.96*sqrt(Var1),
                   #Fisher's tranformation
                   ZF=(1/2)*log((1+((N/K)-1)*ICC)/1-ICC),
                   VZ=(1/2)*((K-1)^(-1)+(N-K)^(-1)),
                   lowerF=ZF-1.96*sqrt(VZ),
                   upperF=ZF+1.96*sqrt(VZ),
                   lower2=(exp(2*ZF-1.96*sqrt(VZ))-1)/((N/K)-1+(exp(2*ZF-1.96*sqrt(VZ)))),
                   upper2=(exp(2*ZF+1.96*sqrt(VZ))-1)/((N/K)-1+(exp(2*ZF+1.96*sqrt(VZ)))),
                   #Searle's method
                   FS=(ICC*((N/K)-1)+1)/(1-ICC),
                   lower3=((FS/qf(.95, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.95, df1=K-1, df2=N-1)-1))),
                   Q13=((FS/qf(.75, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.75, df1=K-1, df2=N-1)-1))),
                   Median3=((FS/qf(.5, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.5, df1=K-1, df2=N-1)-1))),
                   Q33=((FS/qf(.25, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.25, df1=K-1, df2=N-1)-1))),
                   upper3=((FS/qf(.05, df1=K-1, df2=N-1)-1)/((N/K)+(FS/qf(.05, df1=K-1, df2=N-1)-1)))
    )
    
  })
  
  output$Uncertainty_plot<- renderPlot({
    Swiger<-ggplot() + 
      stat_function(fun = function(x){qnorm(x,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96)},linewidth=1.5,colour="blue") +
      theme_bw() +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower1,3),";",round(rv()$upper1,3),"]","\n",
                                                                       "Q1=",round(qnorm(0.25,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3),"\n",
                                                                       "Median=",round(qnorm(0.5,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3),"\n",
                                                                       "Q3=",round(qnorm(0.75,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower1)/1.96),3))))+
      ggtitle("Swiger's method")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    Fisher<-ggplot() + 
      stat_function(fun = function(x){qnorm(x,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96)},linewidth=1.5,colour="green") +
      theme_bw() +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower2,3),";",round(rv()$upper2,3),"]","\n",
                                                                       "Q1=",round(qnorm(0.25,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3),"\n",
                                                                       "Median=",round(qnorm(0.5,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3),"\n",
                                                                       "Q3=",round(qnorm(0.75,mean=rv()$ICC,sd=(rv()$ICC-rv()$lower2)/1.96),3))))+
      ggtitle("Fisher's transformation")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    Searle<-ggplot() + 
      stat_function(fun = function(x){(rv()$FS/qf(1-x, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-x, df1=rv()$K-1, df2=rv()$N-1)-1))},size=1.5,colour="red") +
      ylab("ICC")+
      xlab("Empirical Cumulative Distribution")+
      theme_bw() +
      xlim(c(0, 1))+
      coord_flip()+
      geom_text_npc(size=5,aes(npcx = "right", npcy = "bottom", label = paste("95% CI: [",round(rv()$lower3,3),";",round(rv()$upper3,3),"]","\n",
                                                                       "Q1=",round((rv()$FS/qf(1-0.25, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.25, df1=rv()$K-1, df2=rv()$N-1)-1)),3),"\n",
                                                                       "Median=",round((rv()$FS/qf(1-0.5, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.5, df1=rv()$K-1, df2=rv()$N-1)-1)),3),"\n",
                                                                       "Q3=",round((rv()$FS/qf(1-0.75, df1=rv()$K-1, df2=rv()$N-1)-1)/((rv()$N/rv()$K)+(rv()$FS/qf(1-0.75, df1=rv()$K-1, df2=rv()$N-1)-1)),3))))+
      ggtitle("Searle's method")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    
    grid.arrange(Swiger, Fisher,Searle, ncol = 3)
  })
  
