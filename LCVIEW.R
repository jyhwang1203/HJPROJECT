#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes", "shinydashboard","dashboard")
ipak(pkg)

source("C:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/R-CODE/FUNCTION/Mortalityfn.r")
source("C:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/R-CODE/FUNCTION/mortality.r")
source("C:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/R-CODE/FUNCTION/mortality2.r")

  
  ui <- 
    
    navbarPage("Dashboard", theme = shinytheme("flatly"),
               tabPanel("사망률모델",
    dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        shinydashboard::valueBox("Lee-Carter","Mortality",color="red", width = 12)
        ),
      fluidRow(box(plotOutput("lc"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("kt"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("cusum"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("kt2"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("mr"), width = 12, solidHeader = TRUE,background = "red")),
               fluidRow(box(plotOutput("mr2"), width = 12, solidHeader = TRUE,background = "red")),
                        fluidRow(box(plotOutput("mr3"), width = 12, solidHeader = TRUE,background = "red")
              )
      )
    )
  )
  )
  
  server <- function(input, output){
    
    # output$YN <- renderPlot({
    #   #compares the number of 20 to 34 year olds ("Yuppies") to the number of 40 to 54 year olds ("Nerds").
    #   data.frame(STD_DT=c(1970:2021),TOTAL=BASEKR$Ext[21:35,]%>%apply(2,sum)/
    #   BASEKR$Ext[41:55,]%>%apply(2,sum),
    #   MALE=BASEKRM$Ext[21:35,]%>%apply(2,sum)/
    #   BASEKRM$Ext[41:55,]%>%apply(2,sum),
    #   FEMALE=BASEKRF$Ext[21:35,]%>%apply(2,sum)/
    #   BASEKRF$Ext[41:55,]%>%apply(2,sum))%>%reshape2::melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT, y=value, col = variable)) +             
    #     geom_line(size=1)+ggtitle("ALPHA") +
    #     theme(legend.text = element_text(size=15))
    #   
    #   
    #   
    # })
    
    output$lc <- renderPlot({
 
    g1 <-  lcax %>%as.data.frame()%>%reshape2::melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT, y=value, col = variable)) +             
     geom_line(size=1)+ggtitle("ALPHA") +
      theme(legend.text = element_text(size=15))
    
      
    g2 <-  lcbx %>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
        geom_line()+ggtitle("BETA") +
      theme(legend.text = element_text(size=15))
    
      
    g3 <-  lckt%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
        geom_line()+ggtitle("KAPPA") +
      theme(legend.text = element_text(size=15))
    
    
    grid.arrange(g1,g2,g3, 
                 ncol = 3, nrow = 1)
    
    })
    
    output$kt <- renderPlot({
      
      fit11<- autoplot(forecast(lckt$TOTAL ))
      fit22<- autoplot(forecast(lckt$MALE))
      fit33<- autoplot(forecast(lckt$FEMALE))
     # fit44<- autoplot(forecast(fit4))
      grid.arrange(fit11,fit22,fit33,ncol=3)
    })
  
    
    output$cusum <- renderPlot({
      
      par(mfrow=c(1,3))
      plot(c(1970:2021),cusum(data1) ,type='l')
      plot(c(1970:2021),cusum(data2) ,type='l')
      plot(c(1970:2021),cusum(data3) ,type='l')
    })
    
    
  output$kt2 <- renderPlot({
    
    g1<- data.frame(STD_DT=c(1970:2021),res4$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("KAPPA(TOTAL)") +
      theme(legend.text = element_text(size=15))
    
    g2<-  data.frame(STD_DT=c(1970:2021),res4m$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("KAPPA(MALE)") +
      theme(legend.text = element_text(size=15))
    
    g3<- data.frame(STD_DT=c(1970:2021),res4f$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("KAPPA(FEMALE)") +
      theme(legend.text = element_text(size=15))
    
    
    grid.arrange(g1,g2,g3, 
                 ncol = 3, nrow = 1)
  })
  
  output$mr <- renderPlot({
    
    g1<- data.frame(STD_DT=c(1970:2021),res4$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    g2<-  data.frame(STD_DT=c(1970:2021),res4m$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    g3<- data.frame(STD_DT=c(1970:2021),res4f$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    g4<- data.frame(STD_DT=c(1970:2021),res5$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    g5<-  data.frame(STD_DT=c(1970:2021),res5m$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    g6<- data.frame(STD_DT=c(1970:2021),res5f$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line()+ggtitle("사망률예측") +
      theme(legend.text = element_text(size=15))
    
    
    grid.arrange(g1,g2,g3,g4,g5,g6, 
                 ncol = 3, nrow = 2)

  })
  

  output$mr2 <- renderPlot({
    #sTD_DT,LL,LSTM,REAL,ARIMA,APC,CBD,RH
    g1 <-TEMPM%>%select(STD_DT,LL,LSTM,REAL,ARIMA,RH)%>%filter(STD_DT>2000)%>%
      melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
      geom_line()+ggtitle("주요사망률모형") +
      theme(legend.text = element_text(size=15))
    
    
    g2 <-TEMPF%>%select(STD_DT,LL,LSTM,REAL,ARIMA,RH)%>%filter(STD_DT>2000)%>%
      melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
      geom_line()+ggtitle("주요사망률모형") +
      theme(legend.text = element_text(size=15))
    
    
    grid.arrange(g1,g2,
                 ncol = 2, nrow = 1)
  })

  output$mr3 <- renderPlot({
    source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/mortality.r")
    #res4 <-   lstm(LCfit$kt,BASEKR,10,"LC",300,100)
    llm <- LLfitm$a + (LLfitm$B %*% LLfitm$K)  + LLfitm$b%*%LLfitm$k
    llf <- LLfitf$a + (LLfitf$B %*% LLfitf$K)  + LLfitf$b%*%LLfitf$k
    lcm <- LCfitm$ax +(LCfitm$bx%*% LCfitm$kt)
    rlm <- BASEKRM$Dxt/BASEKRM$Ext
    lcf <- LCfitf$ax +(LCfitf$bx%*% LCfitf$kt)
    rlf <- BASEKRF$Dxt/BASEKRF$Ext
    STD_DT <- c(1970:2021)
    
    tmp1<- cbind(exp(llm[55,]),exp(lcm[55,]),(rlm[55,]))
    colnames(tmp1)[1:3] <- c("Li-Lee","LC","REAL")
    rownames(tmp1)<- STD_DT
     g1 <- tmp1%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
       theme(legend.text = element_text(size=15))
     
     tmp2<- cbind(exp(llf[55,]),exp(lcf[55,]),(rlf[55,]))
     colnames(tmp2)[1:3] <- c("Li-Lee","LC","REAL")
     rownames(tmp2)<- STD_DT
     g2 <- tmp2%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
       theme(legend.text = element_text(size=15))
    
     grid.arrange(g1,g2,
                 ncol = 2, nrow = 1)
  })
  }
  
  # output$mr3 <- renderPlot({
  #   
  #   llm <- LLfitm$a + (LLfitm$B %*% LLfitm$K)  + LLfitm$b%*%LLfitm$k
  #   llf <- LLfitf$a + (LLfitf$B %*% LLfitf$K)  + LLfitf$b%*%LLfitf$k
  # 
  #   tmp1 <- cbind(AGE,LLfitm$b,LLfitf$b)
  #   colnames(tmp1)[2:3] <- c("MALE","FEMALE")
  #   tmp2 <- cbind(STD_DT,LLfitm$k,LLfitf$k)
  #   colnames(tmp2)[2:3] <- c("MALE","FEMALE")
  #   
  #   g1 <- tmp1%>%melt(id.vars="AGE")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   tmp2<- cbind(exp(llf[45,]),exp(lcf[45,]),(rlf[45,]))
  #   colnames(tmp2)[1:3] <- c("Li-Lee","LC","REAL")
  #   rownames(tmp2)<- STD_DT
  #   g2 <- tmp2%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   grid.arrange(g1,g2,
  #                ncol = 2, nrow = 1)
  # })
  # }
  
  shinyApp(ui, server)
  
  res3
  tmp2
