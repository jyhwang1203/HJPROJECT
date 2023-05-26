#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes", "shinydashboard","dashboard")
ipak(pkg)
  source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/Mortalityfn.r")
####
  
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
      fluidRow(box(DTOutput("lc2"), width = 4, solidHeader = TRUE),box(DTOutput("lc3"), width = 4, solidHeader = TRUE),box(DTOutput("lc4"), width = 4, solidHeader = TRUE)),
      fluidRow(box(plotOutput("lc"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("kt"), width = 12, solidHeader = TRUE,background = "red")),
      fluidRow(box(plotOutput("cusum"), width = 12, solidHeader = TRUE,background = "red"))
      # fluidRow(box(plotOutput("kt2"), width = 12, solidHeader = TRUE,background = "red")),
      # fluidRow(box(plotOutput("mr"), width = 12, solidHeader = TRUE,background = "red")),
      # fluidRow(box(plotOutput("mr2"), width = 12, solidHeader = TRUE,background = "red")),
      # fluidRow(box(plotOutput("mr3"), width = 12, solidHeader = TRUE,background = "red")
              )
      )
    )
  )
  
  server <- function(input, output){
    
    output$lc <- renderPlot({
      
    g1 <-  lcax %>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
     geom_line()+ggtitle("ALPHA") +
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
    
    output$lc2 <- renderDT({lcax%>%.[,-1]})
    output$lc3 <- renderDT({lcbx%>%.[,-1]})
    output$lc4 <- renderDT({lckt%>%.[,-1]})
    output$kt <- renderPlot({
      
      fit11<- autoplot(forecast(lckt$TOTAL))
      fit22<- autoplot(forecast(lckt$MALE))
      fit33<- autoplot(forecast(lckt$FEMALE))
     # fit44<- autoplot(forecast(fit4))
      grid.arrange(fit11,fit22,fit33,ncol=3)
    })
  
    
    output$cusum <- renderPlot({
      
      data.frame(STD_DT=c(1970:2021),TOTAL=cusum(data1),MALE=cusum(data2),FEMALE=cusum(data3))%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
        geom_line()+ggtitle("Cusum 통계량") +
        theme(legend.text = element_text(size=15))
    })
    
   
  # output$kt2 <- renderPlot({
  #   
  #   g1<- data.frame(STD_DT=c(1970:2021),res4$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("KAPPA(TOTAL)") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g2<-  data.frame(STD_DT=c(1970:2021),res4m$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("KAPPA(MALE)") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g3<- data.frame(STD_DT=c(1970:2021),res4f$kt[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("KAPPA(FEMALE)") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   
  #   grid.arrange(g1,g2,g3, 
  #                ncol = 3, nrow = 1)
  # })
  # 
  # output$mr <- renderPlot({
  #   
  #   g1<- data.frame(STD_DT=c(1970:2021),res4$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g2<-  data.frame(STD_DT=c(1970:2021),res4m$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g3<- data.frame(STD_DT=c(1970:2021),res4f$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g4<- data.frame(STD_DT=c(1970:2021),res5$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g5<-  data.frame(STD_DT=c(1970:2021),res5m$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   g6<- data.frame(STD_DT=c(1970:2021),res5f$mor[,-1])%>%filter(STD_DT>1900)%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  #     geom_line()+ggtitle("사망률예측") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   
  #   grid.arrange(g1,g2,g3,g4,g5,g6, 
  #                ncol = 3, nrow = 2)
  # 
  # })
  # 
  # 
  # output$mr2 <- renderPlot({
  #   #sTD_DT,LL,LSTM,REAL,ARIMA,APC,CBD,RH
  #   g1 <-TEMPM%>%select(STD_DT,LL,LSTM,REAL,ARIMA,RH)%>%filter(STD_DT>2000)%>%
  #     melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
  #     geom_line()+ggtitle("주요사망률모형") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   
  #   g2 <-TEMPF%>%select(STD_DT,LL,LSTM,REAL,ARIMA,RH)%>%filter(STD_DT>2000)%>%
  #     melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
  #     geom_line()+ggtitle("주요사망률모형") +
  #     theme(legend.text = element_text(size=15))
  #   
  #   
  #   grid.arrange(g1,g2,
  #                ncol = 2, nrow = 1)
  # })
  # 
  # output$mr3 <- renderPlot({
  #   source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/mortality.r")
  #   #res4 <-   lstm(LCfit$kt,BASEKR,10,"LC",300,100)
  #   llm <- LLfitm$a + (LLfitm$B %*% LLfitm$K)  + LLfitm$b%*%LLfitm$k
  #   llf <- LLfitf$a + (LLfitf$B %*% LLfitf$K)  + LLfitf$b%*%LLfitf$k
  #   lcm <- LCfitm$ax +(LCfitm$bx%*% LCfitm$kt)
  #   rlm <- BASEKRM$Dxt/BASEKRM$Ext
  #   lcf <- LCfitf$ax +(LCfitf$bx%*% LCfitf$kt)
  #   rlf <- BASEKRF$Dxt/BASEKRF$Ext
  #   STD_DT <- c(1970:2021)
  #   
  #   tmp1<- cbind(exp(llm[55,]),exp(lcm[55,]),(rlm[55,]))
  #   colnames(tmp1)[1:3] <- c("Li-Lee","LC","REAL")
  #   rownames(tmp1)<- STD_DT
  #    g1 <- tmp1%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
  #      theme(legend.text = element_text(size=15))
  #    
  #    tmp2<- cbind(exp(llf[55,]),exp(lcf[55,]),(rlf[55,]))
  #    colnames(tmp2)[1:3] <- c("Li-Lee","LC","REAL")
  #    rownames(tmp2)<- STD_DT
  #    g2 <- tmp2%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1 , y=value, col = Var2)) + geom_line()+ggtitle("Li-Lee & Lee-Carter") +
  #      theme(legend.text = element_text(size=15))
  #   
  #    grid.arrange(g1,g2,
  #                ncol = 2, nrow = 1)
  # })
  }
  plot(c(1970:2021),LLfitm$k,type="l")
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

  