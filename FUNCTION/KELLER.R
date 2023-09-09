#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes","shinydashboard","dplyr","PerformanceAnalytics","plyr","data.table","DT","lubridate","dualplt","quantmod","scales","ggcorrplot","xlsx","nloptr","RiskPortfolios","ggplot2")
ipak(pkg)

RAWDATA2 <-     RAWDATA%>%filter(variable=="USBOND"|variable=="SP500"|variable=="DM"|variable=="EM"|variable=="US3M"|variable=="CRB"|variable=="USIG"|variable=="USMID"|variable=="USLONG"|variable=="WRTIP"|variable=="USTIP"|variable=="WRBOND"|
                                   variable=="TIP"|variable=="WORLD"|variable=="NASDAQ"|variable=="BIL"|variable=="RUSS"|variable=="MSEU"|variable=="EWJ"|variable=="VNQ"|variable=="GOLD"|variable=="USHY"|variable=="VNQ"|variable=="CRB"|variable=="WREPRA"|variable=="USREIT")%>% dcast(STD_DT~variable)
retm <- RAWDATA2 %>%trans_rt("month")%>%dt_trans()%>%mutate(STD_DT_L1=lag(STD_DT,1))%>%mutate(BM=0.6*WORLD+0.4*WRBOND)
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/Quant UDF.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/BAA V2.R")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/HAA.R")




ui <-
  navbarPage("Dashboard", theme = shinytheme("flatly"),
             tabPanel("BAA",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(dateRangeInput('dateRange',
                                                        label = '',
                                                        start = as.Date('2008-01-01') , end = as.Date('2023-12-31')),
                                         selectInput("state", "공격자산유니버스", c("G12", "G4")),
                                         numericInput("noff", "공격자산선택z", value = 6),
                                         numericInput("ndef", "수비비자산선택", value = 3),
                                         dateInput("port", "포트폴리오 구성"),
                                         checkboxGroupInput("strategy", "Variables to show:",
                                                            c("BAA"="BAA",
                                                              "60/40"="BM",
                                                              "50/30/20"="BM2",
                                                              "글로벌주식"="WORLD"))
                        ),
                                    
                        dashboardBody( fluidRow(box(plotOutput("BAA3"), width =8),box(DTOutput("BAA1"), width =4)),
                                       fluidRow(box(DTOutput("BAA9"), width =4),box(DTOutput("BAA8"), width =4),box(DTOutput("BAA6"), width =4)),
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
                      ))
             
             )
  

server <- function(input, output){

  output$BAA1 <- renderDT({
  BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%dplyr::select(STD_DT,BAA)%>%left_join(
    HAA(input$dateRange[1],input$dateRange[2],4,1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA,BM,WORLD),by="STD_DT")%>%PA
  })
  output$BAA2 <- renderDT({
    MYFUN <- function(data){(prod(1+data)-1)%>%round(4)}
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%dplyr::select(STD_DT,BAA)%>%left_join(
      HAA(input$dateRange[1],input$dateRange[2],4,1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA,BM,WORLD),by="STD_DT")%>%aggregate(cbind(BAA,WORLD,BM)~STD_DT,.,FUN=MYFUN)
 })

  output$BAA3 <- renderPlot({
  BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%dplyr::select(STD_DT,BAA)%>%left_join(
    HAA(input$dateRange[1],input$dateRange[2],4,1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA,BM,WORLD),by="STD_DT")%>%cuml%>% cplot
  })
  
  output$BAA4 <- renderPlot({
    UNIV_HAA0%>%dplyr::select(STD_DT,state)%>%rename(c("state"="HAA"))%>%left_join(UNIV_BAA0%>%dplyr::select(STD_DT,state)%>%rename(c("state"="BAA")),by="STD_DT")%>%cplot
  })
  output$BAA5 <- renderDT({
    PAA%>%dplyr::select(STD_DT,TIP,DBC,UUP,TLT,LQD,AGG,IEF,BIL,SPY,QQQ,IWM,VGK,EWJ,VNQ,VWO,GLD,DBC,HYG)%>%trans_rt("month")%>%cor%>%round(2)
    
  })
  output$BAA6 <- renderDT({
    #BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%.[,c("STD_DT","BAA",input$strategy)]%>%filter(STD_DT>"2023-01-01")%>%as.data.frame
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%dplyr::select(STD_DT,BAA)%>%left_join(
      HAA(input$dateRange[1],input$dateRange[2],4,1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA,BM,WORLD),by="STD_DT") %>%filter(STD_DT>"2023-01-01")%>%as.data.frame 
    }) 
  output$BAA7<- renderPlot({
    BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%dplyr::select(STD_DT,BAA)%>%left_join(
      HAA(input$dateRange[1],input$dateRange[2],4,1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA,BM,WORLD),by="STD_DT")%>%Drawdowns%>%as.xts%>%dt_trans%>%melt(id.vars="STD_DT") %>% 
      ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line(size=1)+ 
      ggtitle("Maximal Drawdown") +
      theme(legend.text = element_text(size=15))
    
  })
  output$BAA8<- renderDT({
    UNIV_BAA2%>%mutate(sma=sma%>%round(3))%>%mutate(Return=Return%>%round(3)) %>% dplyr::select(STD_DT,sma,variable,Return)%>%filter(substr(STD_DT,1,7)==substr(input$port,1,7))
    
      
    })

  output$BAA9<- renderDT({
    UNIV_HAA2%>%mutate(sma=sma%>%round(3))%>%mutate(Return=Return%>%round(3)) %>% dplyr::select(STD_DT,sma,variable,Return)%>%filter(substr(STD_DT,1,7)==substr(input$port,1,7))
  })
  # output$BAA9 <- renderPlot({
  #   MYFUN <- function(data){(prod(1+data)-1)%>%round(4)}
  #   BAA(input$dateRange[1],input$dateRange[2],input$state,input$noff,input$ndef,UNIV_BAA)%>%mutate(STD_DT=substr(STD_DT,1,4))%>%aggregate(cbind(BAA,WORLD,BM)~STD_DT,.,FUN=MYFUN)%>%
  #     melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, fill = variable)) +  
  #     geom_bar(position="dodge", stat="identity")+
  #     theme(legend.text = element_text(size=15))+
  #     ggtitle("연간수익률")
  # })
  output$BAA10 <- renderPlot({
    UNIV%>%dplyr::select(-state)%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
      geom_line(size=1)+
      ggtitle("누적수익률") +
      theme(legend.text = element_text(size=15))
  })
}
shinyApp(ui, server)
UNIV_HAA%>%filter(STD_DT>"2023-06-01")
UNIV_HAA2%>%filter(STD_DT>"2023-06-01")
HAA("2010-01-01","2023-11-30",n=4,nn=1,UNIV_HAA)%>%left_join(
  BAA("2010-01-01","2023-11-30","G12",n=6,nn=3,UNIV_BAA)%>%dplyr::select(STD_DT,BAA) ,by="STD_DT"  
)
RAWDATA2%>%dplyr::select(STD_DT,BIL,US3M,IEF,USMID)%>%trans_rt("month")%>%dt_trans%>%filter(STD_DT>"2010-01-01")
HAA("2010-01-01","2023-11-30",n=4,nn=1,UNIV_HAA)

res1<- HAA("2010-01-01","2023-11-30",n=4,nn=1,UNIV_HAA)%>%left_join(
  BAA("2010-01-01","2023-11-30","G12",n=6,nn=3,UNIV_BAA)%>%dplyr::select(STD_DT,BAA) ,by="STD_DT"  
)%>%PA
res2<- HAA("2010-01-01","2023-11-30",n=4,nn=1,UNIV_HAA)%>%left_join(
  BAA("2010-01-01","2023-11-30","G12",n=6,nn=3,UNIV_BAA)%>%dplyr::select(STD_DT,BAA) ,by="STD_DT"  
)%>%cuml
res3<-HAA("2010-01-01","2023-11-30",n=4,nn=1,UNIV_HAA)%>%left_join(
  BAA("2010-01-01","2023-11-30","G12",n=6,nn=3,UNIV_BAA)%>%dplyr::select(STD_DT,BAA) ,by="STD_DT"  
)%>%filter(STD_DT>"2023-01-01")
UNIV_HAA2%>%filter(STD_DT>"2023-01-01")
res4<- UNIV_BAA2%>%mutate(sma=sma%>%round(3))%>%mutate(Return=Return%>%round(3)) %>% dplyr::select(STD_DT,sma,variable,Return)%>%filter(STD_DT>"2023-01-01")%>%as.data.frame
res4[is.na(res4)] <- 0
res5<- UNIV_HAA2%>%mutate(sma=sma%>%round(3))%>%mutate(Return=Return%>%round(3)) %>% dplyr::select(STD_DT,sma,variable,Return)%>%filter(STD_DT>"2023-01-01")%>%as.data.frame
res5[is.na(res5)] <- 0
write.xlsx(res1 ,"c:/work/HAA.xlsx", sheetName="UNIV_HAA",append=F)
write.xlsx(res2 ,"c:/work/HAA.xlsx", sheetName="UNIV_BAA",append=T)
write.xlsx(res3 ,"c:/work/HAA.xlsx", sheetName="RT",append=T)
write.xlsx(res4 ,"c:/work/HAA.xlsx", sheetName="cum",append=T)
write.xlsx(res5 ,"c:/work/HAA.xlsx", sheetName="PA_BAA",append=T)
write.xlsx(res6 ,"c:/work/HAA.xlsx", sheetName="PABAA",append=T)

