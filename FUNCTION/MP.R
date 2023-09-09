
# Define the user interface (UI)
ui <- dashboardPage(
  dashboardHeader(title = "RDashBond Example"),
  dashboardSidebar(dateRangeInput('date',
                                  label = '',
                                  start = as.Date('2000-01-01') , end = as.Date('2023-12-31')),
                   
                   numericInput("WEI", 
                                h3("Numeric input"), 
                                value = 1)
  ),
  dashboardBody(
    # Add content to the dashboard body
    
    fluidRow(box(DTOutput("MP1"), width = 12, solidHeader = TRUE)),
             fluidRow(box(DTOutput("MP2"), width = 12, solidHeader = TRUE)),
                      fluidRow(box(DTOutput("MP3"), width = 12, solidHeader = TRUE))
    )
    )




server <- function(input, output) {
  
  output$MP1 <- renderDT({
    
 RES1
    
  })
  output$MP2 <- renderDT({
    
 
RES2    
    
  })
  output$MP3 <- renderDT({
    RES3
    
  })
  
  output$port4 <- renderDT({
    
    RAWDATA %>% dplyr::select(STD_DT,NASDAQ,USLONG,WRTIP)%>%na.omit %>%trans_rt("month")%>%dt_trans%>%mutate(BM=input$WEI*NASDAQ+(1-input$WEI)*WRTIP)%>%mutate(BM2=input$WEI*NASDAQ+(1-input$WEI)*USLONG)%>%
      filter(STD_DT>input$date[1]&STD_DT<input$date[2])%>%dplyr::select(STD_DT,BM,BM2,WRTIP,USLONG)%>%cuml%>%cplot
    
  })
  
 
  
}
shinyApp(ui = ui, server = server)
UNIV_MP <-retm%>%mutate(AL=(WREPRA+WRINFRA)/3)%>%mutate(BM=WORLD*0.3+0.2*WRBOND+0.2*AL+0.1*GSCI)%>%melt(id.vars="STD_DT")

UNIV_MP%>%filter(variable=="PEF")
 

RES1 <- MPWEIGHT
RES2 <- MPWEIGHT%>%melt(id.vars="STD_DT")%>%mutate(STD_DT=STD_DT+months(1)-days(1)) %>% left_join(UNIV_MP,by=c("STD_DT","variable"))%>%mutate(wei=value.x)%>%mutate(retm=value.y%>% round(5))%>%mutate(ret=(retm*wei)%>% round(5))%>%
   dcast(STD_DT~variable,value.var="retm") 

RES3 <- MPWEIGHT%>%melt(id.vars="STD_DT")%>%mutate(STD_DT=STD_DT+months(1)-days(1)) %>% left_join(UNIV_MP,by=c("STD_DT","variable"))%>%mutate(wei=value.x)%>%mutate(retm=value.y)%>%mutate(ret=(retm*wei)%>% round(5))%>%
  dcast(STD_DT~variable,value.var="ret") 

MPWEIGHT%>%melt(id.vars="STD_DT")%>%mutate(STD_DT=STD_DT+months(1)-days(1)) %>% left_join(UNIV_MP,by=c("STD_DT","variable"))%>%mutate(wei=value.x)%>%mutate(retm=value.y)%>%mutate(ret=retm*wei)%>%
  dcast(STD_DT~variable,value.var="ret")%>%dplyr::select(-STD_DT)%>%t%>%apply(2,sum)

write.xlsx(RES1 ,"c:/work/MP.xlsx", sheetName="RES1",append=F)
write.xlsx(RES2 ,"c:/work/MP.xlsx", sheetName="RES2",append=T)
write.xlsx(RES3 ,"c:/work/MP.xlsx", sheetName="RES3",append=T)
write.xlsx(BASER ,"c:/work/MP.xlsx", sheetName="BASER",append=T)
BASER