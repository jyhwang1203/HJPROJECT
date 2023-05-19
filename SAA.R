#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes","shinydashboard","dplyr","PerformanceAnalytics","plyr","data.table","DT","lubridate","dualplt","quantmod","scales","ggcorrplot","xlsx","nloptr","RiskPortfolios","ggplot2")
ipak(pkg)

RAWDATA <-  read.csv("c:/work/RAWDATA.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%
  mutate(STD_DT=as.Date(STD_DT))
STDDT <- "2023-04-30"%>%as.Date()

source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/Quant UDF.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/SOURCE V3.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/CMA.R")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/MVO_TARGETRT.R")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/MVO_Voltarget.R")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/BAA V2.R")





ui <-
  navbarPage("Dashboard", theme = shinytheme("flatly"),
             tabPanel("BAA",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(dateRangeInput('dateRange',
                                                        label = '',
                                                        start = as.Date('2008-01-01') , end = as.Date('2023-12-31')),
                                         selectInput("state", "공격자산유니버스", c("G12", "G4")),
                                         numericInput("noff", "공격자산선택", value = 6),
                                         numericInput("ndef", "수비비자산선택", value = 3),
                                         dateInput("port", "포트폴리오 구성"),
                                         checkboxGroupInput("strategy", "Variables to show:",
                                                            c("BAA"="BAA",
                                                              "60/40"="BM",
                                                              "글로벌주식"="WORLD")),
                                         checkboxGroupInput("univ", "Variables to show:",
                                                            c("달러미포함"="UUPX",
                                                              "달러포함"="UUPO"))
                        ),
                                    
                        dashboardBody( fluidRow(box(plotOutput("BAA3"), width =6),box(plotOutput("BAA9"), width =6)),
                                       fluidRow(box(DTOutput("BAA1"), width =4),box(DTOutput("BAA8"), width =4),box(plotOutput("BAA6"), width =4)),
                                       fluidRow(box(plotOutput("BAA4"), width =6),box(plotOutput("BAA7"), width =6)),
                                       fluidRow(box(plotOutput("BAA10"), width =12))
                                     )
                      )),
             
             tabPanel("BAA cor",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(dateRangeInput('dateRange',
                                                        label = '',
                                                        start = as.Date('2008-01-01') , end = as.Date('2023-12-31'))
                                        
                        ),
                        
                        dashboardBody(fluidRow(box(DTOutput("BAA5"), width =12))
                                      
                        )
                      )),
             tabPanel("목표수익률",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(),
                        dashboardBody(fluidRow(box(plotOutput("TRT"), width =12)),
                                     fluidRow(box(DTOutput("TRT2"), width =12))
                                     )
                      )
                      ),
             tabPanel("목표변동성",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(),
                        dashboardBody(fluidRow(box(plotOutput("VRT"), width =12)),
                                      fluidRow(box(DTOutput("VRT2"), width =12))
                        )
                      )
             )
             
             )
  

server <- function(input, output){
####baa
  output$BAA1 <- renderDT({
  BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>% PA  

    
  })
  output$BAA2 <- renderDT({
    MYFUN <- function(data){(prod(1+data)-1)%>%round(4)}
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%mutate(STD_DT=substr(STD_DT,1,4))%>%aggregate(cbind(BAA,WORLD,BM)~STD_DT,.,FUN=MYFUN)
 })

output$BAA3 <- renderPlot({
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
      geom_line(size=1)+
      ggtitle("누적수익률") +
      theme(legend.text = element_text(size=15))
  })
  output$BAA4 <- renderPlot({
    UNIV%>%select(STD_DT,state)%>%ggplot(aes(STD_DT, state)) +
      geom_line(size=1)+
      ggtitle("카나리아자산") +
      theme(legend.text = element_text(size=15))
  })
  output$BAA5 <- renderDT({
    PAA%>%select(STD_DT,TIP,DBC,UUP,TLT,LQD,AGG,IEF,BIL,SPY,QQQ,IWM,VGK,EWJ,VNQ,VWO,GLD,DBC,HYG)%>%trans_rt("month")%>%cor%>%round(2)
    
  })
  output$BAA6 <- renderPlot({
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%.[,c("STD_DT","BAA",input$strategy)]%>%melt(id.vars="STD_DT") %>% 
      ggplot(aes(x=value,fill=variable))+
      geom_histogram(alpha=0.5, position="identity")+
      theme(legend.text = element_text(size=15))
  }) 
  output$BAA7<- renderPlot({
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%.[,c("STD_DT","BAA",input$strategy)]%>%Drawdowns%>%as.xts%>%dt_trans%>%melt(id.vars="STD_DT") %>% 
      ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line(size=1)+ 
      ggtitle("Maximal Drawdown") +
      theme(legend.text = element_text(size=15))
    
  })
  output$BAA8<- renderDT({
    STDDT<- input$port
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%.[,c("STD_DT","BAA",input$strategy)]
    UNIV_BAA2%>%filter(substr(STD_DT,1,7)==substr(STDDT,1,7))%>%mutate(sma=sma%>%round(3))%>%mutate(Return=Return%>%round(3))
  })
  output$BAA9 <- renderPlot({
    MYFUN <- function(data){(prod(1+data)-1)%>%round(4)}
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,input$univ)%>%mutate(STD_DT=substr(STD_DT,1,4))%>%aggregate(cbind(BAA,WORLD,BM)~STD_DT,.,FUN=MYFUN)%>%
      melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, fill = variable)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))+
      ggtitle("연간수익률")
  })
  output$BAA10 <- renderPlot({
    UNIV%>%select(-state)%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
      geom_line(size=1)+
      ggtitle("누적수익률") +
      theme(legend.text = element_text(size=15))
  })
  ##TARGET##########################
  
  output$TRT<- renderPlot({
    G1TRT <- wei1%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.065)) 
    G2TRT <- wei2%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.060)) 
    G3TRT <- wei3%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.055)) 
    G4TRT <- wei4%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.050)) 
    G5TRT <- wei5%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.045)) 
    G6TRT <- wei6%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.040)) 
  grid.arrange(G1TRT,G2TRT,G3TRT,G4TRT,G5TRT,ncol=5)
  })
  output$TRT2<- renderDT({
    targetrt
  })
  ##VOLARGET##########################
  
  output$VRT<- renderPlot({
    grid.arrange(G1VOL,G2VOL,G3VOL,G4VOL,G5VOL,ncol=5)
  })
  output$VRT2<- renderDT({
    voltarget
  })
}


shinyApp(ui, server)

