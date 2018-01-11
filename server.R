#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(survival)
library(splines)
library(MASS)
library(flexsurv)
library(dplyr)

library(formattable)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  #This function is repsonsible for loading in the selected file
  
  filedata1 <- reactive({
    infile <- input$datafile1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)[1:max(na.omit(read.csv(infile$datapath))),]

  })
  
  filedata2 <- reactive({
    infile <- input$datafile2
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)[complete.cases(read.csv(infile$datapath)),]
    
  })
  
  # This allows user to select variables in the filedata
  # First variable will be the survival probability variable (Column 5 in the excel sheet)
  output$Variables1 <- renderUI({
    selectInput('vars1', 'S(t): Select the variable/column that represents the survival probabilities', names(filedata1()) , multiple = FALSE)
  })
  
  
  # Second variable will be the time variable (Column 2 in the excel sheet)
  output$Variables2 <- renderUI({
    selectInput('vars2', 'Time: Select the variable/column that represents the time', names(filedata1()) , multiple = FALSE)
  })
  
  #This previews the CSV data files
  output$filetable1 <- renderDataTable({
    filedata1()
  })
  
  output$IPDtable <- renderDataTable({
    IPD()
  })
  
  output$filetable2 <- renderDataTable({
    filedata2()
  })
  
  # R session info
  output$info <- renderPrint({
    sessionInfo()
  })

  output$exp <- renderPrint({
    expfl()
  })
  
  # Guyot algorithm for recreating survival curve
  
  
  #Output IPD
  IPD <- reactive({
    #Read in survival times 
    t.S<-filedata1()[,input$vars2]
    S<-filedata1()[,input$vars1]
    
    #Read in published numbers at risk, n.risk, at time, t.risk, lower and upper
    # indexes for time interval
    t.risk<-filedata2()[,2]
    lower<-filedata2()[,3]
    upper<-filedata2()[,4]
    n.risk<-filedata2()[,5]
    n.int<-length(n.risk)
    n.t<- upper[n.int]
    
    #Initialise vectors
    arm<-rep(input$arm.id,n.risk[1]) 
    n.censor<- rep(0,(n.int-1))
    n.hat<-rep(n.risk[1]+1,n.t)
    cen<-rep(0,n.t)
    d<-rep(0,n.t)
    KM.hat<-rep(1,n.t)
    last.i<-rep(1,n.int)
    sumdL<-0
    
    
    try(
      if (n.int > 1){
        #Time intervals 1,...,(n.int-1)
        for (i in 1:(n.int-1)){
          #First approximation of no. censored on interval i
          n.censor[i]<- round(n.risk[i]*S[lower[i+1]]/S[lower[i]]- n.risk[i+1])
          #Adjust tot. no. censored until n.hat = n.risk at start of interval (i+1)
          while((n.hat[lower[i+1]]>n.risk[i+1])||((n.hat[lower[i+1]]<n.risk[i+1])&&(n.censor[i]>0))){
            if (n.censor[i]<=0){
              cen[lower[i]:upper[i]]<-0
              n.censor[i]<-0
            }
            if (n.censor[i]>0){
              cen.t<-rep(0,n.censor[i])
              for (j in 1:n.censor[i]){
                cen.t[j]<- t.S[lower[i]] +
                  j*(t.S[lower[(i+1)]]-t.S[lower[i]])/(n.censor[i]+1)
              }
              #Distribute censored observations evenly over time. Find no. censored on each time interval.
              cen[lower[i]:upper[i]]<-hist(cen.t,breaks=t.S[lower[i]:lower[(i+1)]],
                                           plot=F)$counts 
            }
            #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
            n.hat[lower[i]]<-n.risk[i]
            last<-last.i[i] 
            for (k in lower[i]:upper[i]){
              if (i==1 & k==lower[i]){
                d[k]<-0
                KM.hat[k]<-1
              }
              else {
                d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
                KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
              }
              n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
              if (d[k] != 0) last<-k
            }
            n.censor[i]<- n.censor[i]+(n.hat[lower[i+1]]-n.risk[i+1])
          }
          if (n.hat[lower[i+1]]<n.risk[i+1]) n.risk[i+1]<-n.hat[lower[i+1]]
          last.i[(i+1)]<-last
        }
      }
      ,
      silent=TRUE
    )
    
    
    #Time interval n.int.
    try(
      {if (n.int>1){
        #Assume same censor rate as average over previous time intervals.
        n.censor[n.int]<- min(round(sum(n.censor[1:(n.int-1)])*(t.S[upper[n.int]]-
                                                                  t.S[lower[n.int]])/(t.S[upper[(n.int-1)]]-t.S[lower[1]])), n.risk[n.int])
      }
        
        
        
        if (n.int==1){n.censor[n.int]<-0}
        if (n.censor[n.int] <= 0){
          cen[lower[n.int]:(upper[n.int]-1)]<-0
          n.censor[n.int]<-0
          
          if (n.censor[n.int]>0){
            cen.t<-rep(0,n.censor[n.int])
            for (j in 1:n.censor[n.int]){
              cen.t[j]<- t.S[lower[n.int]] +
                j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
            }
            cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                                     plot=F)$counts 
          }
        }
        #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
        n.hat[lower[n.int]]<-n.risk[n.int]
        last<-last.i[n.int]
        for (k in lower[n.int]:upper[n.int]){
          if(KM.hat[last] !=0){
            d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))} else {d[k]<-0}
          KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
          n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
          #No. at risk cannot be negative
          if (n.hat[k+1] < 0) {
            n.hat[k+1]<-0
            cen[k]<-n.hat[k] - d[k]
          }
          if (d[k] != 0) last<-k
        }
        
        
        #If total no. of events reported, adjust no. censored so that total no. of events agrees.
        if (tot.events != "NA"){
          if (n.int>1){
            sumdL<-sum(d[1:upper[(n.int-1)]])
            #If total no. events already too big, then set events and censoring = 0 on all further time intervals
            if (sumdL >= tot.events){
              d[lower[n.int]:upper[n.int]]<- rep(0,(upper[n.int]-lower[n.int]+1))
              cen[lower[n.int]:(upper[n.int]-1)]<- rep(0,(upper[n.int]-lower[n.int]))
              n.hat[(lower[n.int]+1):(upper[n.int]+1)]<- rep(n.risk[n.int],(upper[n.int]+1-lower[n.int]))
            }
          }
          #Otherwise adjust no. censored to give correct total no. events
          if ((sumdL < tot.events)|| (n.int==1)){
            sumd<-sum(d[1:upper[n.int]])
            while ((sumd > tot.events)||((sumd< tot.events)&&(n.censor[n.int]>0))){
              n.censor[n.int]<- n.censor[n.int] + (sumd - tot.events)
              if (n.censor[n.int]<=0){
                cen[lower[n.int]:(upper[n.int]-1)]<-0
                n.censor[n.int]<-0
              }
              if (n.censor[n.int]>0){
                cen.t<-rep(0,n.censor[n.int])
                for (j in 1:n.censor[n.int]){
                  cen.t[j]<- t.S[lower[n.int]] +
                    j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
                }
                cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                                         plot=F)$counts 
              }
              n.hat[lower[n.int]]<-n.risk[n.int]
              last<-last.i[n.int]
              for (k in lower[n.int]:upper[n.int]){
                d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
                KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
                if (k != upper[n.int]){
                  n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
                  #No. at risk cannot be negative
                  if (n.hat[k+1] < 0) {
                    n.hat[k+1]<-0
                    cen[k]<-n.hat[k] - d[k]
                  }
                }
                if (d[k] != 0) last<-k
              }
              sumd<- sum(d[1:upper[n.int]])
            }
          }
        }
        
      },
      silent = TRUE
    )
    
    #Initialise vectors
    try({
      t.IPD<-rep(t.S[n.t],n.risk[1])
      event.IPD<-rep(0,n.risk[1])
      #Write event time and event indicator (=1) for each event, as separate row in t.IPD and event.IPD
      k=1
      for (j in 1:n.t){
        if(d[j]!=0){
          t.IPD[k:(k+d[j]-1)]<- rep(t.S[j],d[j])
          event.IPD[k:(k+d[j]-1)]<- rep(1,d[j])
          k<-k+d[j]
        }
      }
    },
    silent = TRUE)
    
    #Write censor time and event indicator (=0) for each censor, as separate row in t.IPD and event.IPD
    for (j in 1:(n.t-1)){
      if(cen[j]!=0){
        t.IPD[k:(k+cen[j]-1)]<- rep(((t.S[j]+t.S[j+1])/2),cen[j])
        event.IPD[k:(k+cen[j]-1)]<- rep(0,cen[j])
        k<-k+cen[j]
      }
    }
    
    #Output IPD
    
    IPData <-matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F)
    IPData <-as.data.frame(IPData)
    #IPData
    IPData
  })
  
  KMest <- reactive({
    survfit(Surv(IPD()[,1],IPD()[,2])~1,data=IPD(),type="kaplan-meier")
  })

  output$KMsum <- renderPrint({
    summary(KMest())
  })

  
  # Fit survival distributions
  
  exp <- reactive({
    survreg(Surv(IPD()[,1], IPD()[,2])~1, dist="exponential")   # Exponential function, interval censoring			
  })
  
  
  expfl <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="exponential")   # Exponential function, interval censoring				
  })
  
  gengam <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="gengamma")   # Generalized gamma function, interval censoring		
  }) 										

  gompert <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="gompertz")   # Generalized gamma function, interval censoring	
  }) 											

  llogfl <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="llogis")   # Loglogistic function, interval censoring	
  }) 											

  lnormfl <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="lognormal") 
  }) 
  
  weibfl <- reactive({
    flexsurvreg(Surv(IPD()[,1], IPD()[,2])~1, dist="weibull")   # Loglogistic function, interval censoring						
  })
  
  splinefl <- reactive({
    flexsurvspline(Surv(IPD()[,1], IPD()[,2])~1, k = input$numspline, scale = "hazard") # spline
  }) 
  
  
  # Make Plots
  
  makeplot <- function(){
    # If splin == yes, include splin in the distribution plot
    if(input$splineyn == 'yes'){
      
      SurvObj <- with(IPD(), Surv(IPD()[,1], IPD()[,2]))
      KM <- survfit(SurvObj ~ 1, data=IPD())
      
      plot(KM, xmax=45, xlab = "months", ylab = "S(t)", main = input$titletxt)
      # add the other distribution lines 
      #Plot Exponential
      lines(expfl(), col="red", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      
      #Plot gen gamma
      lines(gengam(), col="orange", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      #Plot log-logistic
      lines(llogfl(), col="yellow", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      
      lines(weibfl(), col="green", ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      
      lines(lnormfl(), col = 'blue', ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      lines(gompert(), col = "purple", ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      lines(splinefl(), col = "chocolate", ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      ## Add legends
      legend(x = "bottomleft",
             legend = c("Kaplan-Meier","Exp" ,"Gen-Gamma", "Log-logistic", "Weibull", "Log-Normal", "Gompertz", "Spline"),
             lwd = 2, bty = "n", lty=c(1,1,1,1,1,1,1,1),
             col = c("black", "red","orange", "yellow", "green", "blue", "purple", "chocolate"))
      
    } else {
      # no spline plot
      SurvObj <- with(IPD(), Surv(IPD()[,1], IPD()[,2]))
      KM <- survfit(SurvObj ~ 1, data=IPD())
      
      plot(KM, xmax=45, xlab = "months", ylab = "S(t)", main = input$titletxt)
      # add the other distribution lines 
      #Plot Exponential
      lines(expfl(), col="red", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      
      #Plot gen gamma
      lines(gengam(), col="orange", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      #Plot log-logistic
      lines(llogfl(), col="yellow", ci= ifelse(input$confint == 'yes', TRUE, FALSE))
      
      lines(weibfl(), col="green", ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      
      lines(lnormfl(), col = 'blue', ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      lines(gompert(), color = "purple", ci = ifelse(input$confint == 'yes', TRUE, FALSE))
      
      ## Add legends
      legend(x = "bottomleft",
             legend = c("Kaplan-Meier","Exp" ,"Gen-Gamma", "Log-logistic", "Weibull", "Log-Normal", "Gompertz"),
             lwd = 2, bty = "n", lty=c(1,1,1,1,1,1,1),
             col = c("black", "red","orange", "yellow", "green", "blue", "purple"))
      
    } 
  }
  
  output$survplot <- renderPlot({
    print(makeplot())
  })
  
  
  # Parameter Estimates
  
  est <- reactive({

    sstab <- data.frame(c("Exponential","Weibull","Log-normal","Log-logistic","Gompertz", "Generalized-Gamma"),
                        c(1, base::exp(weibfl()$coefficients[1]), 
                          lnormfl()$coefficients[1], base::exp(llogfl()$coefficients[1]),
                          gompert()$coefficients[1], gengam()$coefficients[1]),
                        c(1/base::exp(exp()$coefficients), base::exp(weibfl()$coefficients[2]),
                          base::exp(lnormfl()$coefficients[2]), base::exp(llogfl()$coefficients[2]),
                          base::exp(gompert()$coefficients[2]), base::exp(gengam()$coefficients[2])),
                        c("NA","NA","NA","NA","NA", base::exp(gengam()$coefficients[3]))
    )
    names(sstab) <- c("Model","Shape","Scale","GenGamparam")
    
    ptab <- cbind(sstab$Model, data.frame(lapply(sstab[2:ncol(sstab)], function(x)  round(as.numeric(as.character(x)), 3))))
    
    ptab
  })
  
  output$parest <- renderDataTable({
    est()
  })
  
  
  # AIC/BIC fit statistics
  
  fitstats <- reactive({
    if(input$splineyn == 'yes'){
      x <- list(exp(), weibfl(), lnormfl(), llogfl(), gengam(), gompert(), splinefl())
      aictable <- do.call(cbind, lapply(lapply(x, function(x) {
        c(AIC(x), BIC(x))
      }), data.frame, stringsAsFactors=FALSE))
      names(aictable) <- c("Exponential","Weibull","Log-normal","Log-logistic","Generalized-Gamma","Gompertz", "Spline")
      rownames(aictable) <- c("AIC", "BIC")
    } else {
      x <- list(exp(), weibfl(), lnormfl(), llogfl(), gengam(), gompert())
      aictable <- do.call(cbind, lapply(lapply(x, function(x) {
        c(AIC(x), BIC(x))
      }), data.frame, stringsAsFactors=FALSE))
      names(aictable) <- c("Exponential","Weibull","Log-normal","Log-logistic","Generalized-Gamma","Gompertz")
      rownames(aictable) <- c("AIC", "BIC")
    }
    t(aictable)
  })
  
  output$fitstat <- renderDataTable({
    fitstats()
  })
  
  
  ## Standardized Solutions
  
  output$stdsol <- renderFormattable({
    formattable(standardizedsolution(est()$fit),  digits = 4)
  })
  
  outputOptions(output, 'parest', suspendWhenHidden=FALSE)
  # Rsquared
  

  
  
})
