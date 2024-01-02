
tmp   <- RAWDATA%>%filter(variable=="SP500"|variable=="KOSPI"|variable=="USGOVT"|variable=="KRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("month")%>%dt_trans
ttmp  <- RAWDATA%>%filter(variable=="SP500"|variable=="KOSPI"|variable=="USGOVT"|variable=="KRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("week")%>%dt_trans
#ROLLING CORRELATION 5YEAR
data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$SP500, tmp$USGOVT, width = 36))%>%cplot
data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$KOSPI, tmp$KRBOND, width = 36))%>%cplot
#ROLLING CORRELATION 6MONTH
data.table(STD_DT=ttmp$STD_DT,ROLLIMG3MONTH =roll_cor(ttmp$SP500, ttmp$USGOVT, width = 26),ROLLIM3MONTH =roll_cor(ttmp$KOSPI, ttmp$KRBOND, width = 26))%>%cplot

#주식시장 정리
ui <-   navbarPage("Dashboard", theme = shinytheme("flatly"),
                   tabPanel("주요주가지수",dashboardPage(dashboardHeader(title = "View"),dashboardSidebar(dateInput("date",h3("Date input"),value = "2023-09-01")),
                                                   dashboardBody(fluidRow(shinydashboard::valueBox("WEEK","주가지수,EPS",color="red", width = 4),
                                                                          shinydashboard::valueBox("MONTH","주가지수,EPS",color="blue", width = 4),
                                                                          shinydashboard::valueBox("YTD","주가지수,EPS",color="orange", width = 4)),
                                                                 fluidRow(box(plotOutput("stock_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                                          box(plotOutput("stock_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                                          box(plotOutput("stock_ytd"), width = 4, solidHeader = TRUE,background = "orange")),
                                                                 fluidRow(box(plotOutput("bond_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                                          box(plotOutput("bond_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                                          box(plotOutput("bond_ytd"), width = 4, solidHeader = TRUE,background = "orange")),
                                                                 fluidRow(box(plotOutput("al_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                                          box(plotOutput("al_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                                          box(plotOutput("al_ytd"), width = 4, solidHeader = TRUE,background = "orange"))
                                                   ))))


server <- function(input, output)
{   
  output$stock_wek <- renderPlot({graphbar(stock_wek%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$stock_mon <- renderPlot({graphbar(stock_mon%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$stock_ytd <- renderPlot({graphbar(stock_ytd%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  output$bond_wek  <- renderPlot({graphbar(bond_wek %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$bond_mon  <- renderPlot({graphbar(bond_mon %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$bond_ytd  <- renderPlot({graphbar(bond_ytd %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  output$al_wek    <- renderPlot({graphbar(al_wek   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$al_mon    <- renderPlot({graphbar(al_mon   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$al_ytd    <- renderPlot({graphbar(al_ytd   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  
  
}
shinyApp(ui, server)



stock_wek <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                              variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

stock_mon <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                              variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

stock_ytd <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                              variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

grid.arrange(graphbar(stock_mon,"steelblue","주식MON"),graphbar(stock_ytd,"steelblue","주식YTD"),ncol=2)
write.xlsx(stock_mon ,"c:/work/monthly.xlsx", sheetName="stock_mon",append=T)
write.xlsx(stock_ytd ,"c:/work/monthly.xlsx", sheetName="stock_ytd",append=T)

#주식시장 정리

FEPS_mon <- RAWDATA%>%filter(variable=="FEPS_WORLD"|variable=="FEPS_DOW"|variable=="FEPS_NASDAQ"|variable=="FEPS_MSEU"|variable=="FEPS_MSUK"|variable=="FEPS_MEDE"|variable=="FEPS_MEFR"|variable=="FEPS_MSIN"|variable=="FEPS_MSBR"|
                             variable=="FEPS_MSAU"|variable=="FEPS_MSCA"|variable=="FEPS_KOSPI"|variable=="FEPS_KOSDAQ"|variable=="FEPS_MSJP"|variable=="FEPS_SHANGHAI"|variable=="FEPS_CSI300"|variable=="FEPS_HANGS")%>%
                      filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans

FEPS_ytd <- RAWDATA%>%filter(variable=="FEPS_WORLD"|variable=="FEPS_DOW"|variable=="FEPS_NASDAQ"|variable=="FEPS_MSEU"|variable=="FEPS_MSUK"|variable=="FEPS_MEDE"|variable=="FEPS_MEFR"|variable=="FEPS_MSIN"|variable=="FEPS_MSBR"|
                               variable=="FEPS_MSAU"|variable=="FEPS_MSCA"|variable=="FEPS_KOSPI"|variable=="FEPS_KOSDAQ"|variable=="FEPS_MSJP"|variable=="FEPS_SHANGHAI"|variable=="FEPS_CSI300"|variable=="FEPS_HANGS")%>%
  filter(STD_DT<input$date)%>%
  dcast(STD_DT~variable)%>%trans_rt("year")%>%tail(n=1)%>%exname%>%dt_trans

grid.arrange(graphbar(FEPS_mon,"steelblue","주식"),graphbar(FEPS_ytd,"steelblue","FEPS"),ncol=2)

write.xlsx(FEPS_mon ,"c:/work/monthly.xlsx", sheetName="FEPS_mon",append=T)
write.xlsx(FEPS_ytd ,"c:/work/monthly.xlsx", sheetName="FEPS_ytd",append=T)

#채권시장 정리
bond_wek <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                               variable=="EUIG"|variable=="USSHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")

bond_mon <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                               variable=="EUIG"|variable=="USSHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")
         
bond_ytd <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                               variable=="EUIG"|variable=="USHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")

grid.arrange(graphbar(bond_mon,"steelblue","채권MON"),graphbar(bond_ytd,"steelblue","채권YTD"),ncol=2)

write.xlsx(bond_mon ,"c:/work/monthly.xlsx", sheetName="bond_mon",append=T)
write.xlsx(bond_ytd ,"c:/work/monthly.xlsx", sheetName="bond_ytd",append=T)


#대체투자자


al_wek   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                               variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")

al_mon   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                               variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")



al_ytd   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                               variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")

grid.arrange(graphbar(al_mon,"steelblue","채권MON"),graphbar(al_ytd,"steelblue","채권YTD"),ncol=2)

write.xlsx(al_mon ,"c:/work/monthly.xlsx", sheetName="al_mon",append=T)
write.xlsx(al_ytd ,"c:/work/monthly.xlsx", sheetName="al_ytd",append=T)

#외환
RAWDATA%>%filter(variable=="DXY"|variable=="USDKRW"|variable=="EURO"|variable=="CNH"|variable=="EMX"|variable=="GBP"|variable=="YEN")%>%
  dcast(STD_DT~variable)%>%filter(STD_DT=="2023-08-01")
