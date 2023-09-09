
# Define the user interface (UI)
ui <- dashboardPage(
  dashboardHeader(title = "RDashBond Example"),
  dashboardSidebar(checkboxGroupInput("univ", "투자자산군",
                                      c("달러미포함"="UUPX",
                                        "달러포함"="UUPO")),
                   dateRangeInput('date',
                                  label = '',
                                  start = as.Date('2000-01-01') , end = as.Date('2023-12-31')),
                   
                   numericInput("WEI", 
                                h3("Numeric input"), 
                                value = 1)
                   ),
  dashboardBody(
    # Add content to the dashboard body
  
      fluidRow(box(plotOutput("port"), width = 4, solidHeader = TRUE,background = "red"),
               box(DTOutput("port2"), width = 4, solidHeader = TRUE),
               box(plotOutput("port3"), width = 4, solidHeader = TRUE)
      ),
      fluidRow(box(plotOutput("port4"), width = 6, solidHeader = TRUE,background = "red"),
               box(DTOutput("port5"), width = 6, solidHeader = TRUE)
      )
      )
    )
  


server <- function(input, output) {
  
  output$port <- renderPlot({
    
    RAWDATA %>% dplyr::select(STD_DT,NASDAQ,USGOVT) %>%trans_rt("month")%>%dt_trans%>%mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USGOVT)%>%mutate(BM=input$WEI*TQQQ+(1-input$WEI)*TMF)%>%
      dplyr::select(STD_DT,TQQQ,TMF,BM)%>%filter(STD_DT>input$date[1]&STD_DT<input$date[2])%>%cuml%>%cplot

  })
  
  output$port2 <- renderDT({
      RAWDATA %>% dplyr::select(STD_DT,NASDAQ,USGOVT) %>%trans_rt("month")%>%dt_trans%>%mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USGOVT)%>%mutate(BM=input$WEI*TQQQ+(1-input$WEI)*TMF)%>%
      dplyr::select(STD_DT,TQQQ,TMF,BM)%>%filter(STD_DT>input$date[1]&STD_DT<input$date[2])%>%PA
    
    
  })
  output$port3 <- renderPlot({

      RAWDATA %>% dplyr::select(STD_DT,NASDAQ,WRTIP) %>%trans_rt("day")%>%dt_trans%>%mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USLONG)%>%mutate(BM=input$WEI*TQQQ+(1-input$WEI)*TMF)%>%
      dplyr::select(STD_DT,BM) %>% apply.yearly(., Return.cumulative)%>%dt_trans%>%melt(id.vars = "STD_DT")%>%ggplot(aes(STD_DT, value, fill = variable)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    
  })
  
  output$port4 <- renderPlot({
    
    RAWDATA %>% dplyr::select(STD_DT,NASDAQ,USLONG,WRTIP)%>%na.omit %>%trans_rt("month")%>%dt_trans%>%mutate(BM=input$WEI*NASDAQ+(1-input$WEI)*WRTIP)%>%mutate(BM2=input$WEI*NASDAQ+(1-input$WEI)*USLONG)%>%
      filter(STD_DT>input$date[1]&STD_DT<input$date[2])%>%dplyr::select(STD_DT,BM,BM2,WRTIP,USLONG)%>%cuml%>%cplot
    
  })
  
  output$port5 <- renderDT({
    RAWDATA %>% dplyr::select(STD_DT,NASDAQ,WRTIP) %>%trans_rt("month")%>%dt_trans%>%mutate(BM=input$WEI*NASDAQ+(1-input$WEI)*WRTIP)%>%filter(STD_DT>input$date[1]&STD_DT<input$date[2])%>%dplyr::select(STD_DT,BM,BM2)%>%PA
    
    
  })
  
}

  
  
# No server logic is needed for this example


# Run the Shiny app
shinyApp(ui = ui, server = server)

getSymbols('TQQQ', src='yahoo')
getSymbols('TMF', src='yahoo')
tmp<- cbind(TQQQ$TQQQ.Adjusted,TMF$TMF.Adjusted)
colnames(tmp)<-c("TQQQ","TMF")