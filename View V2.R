#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes","shinydashboard","dplyr","PerformanceAnalytics","plyr","data.table","DT","lubridate","quantmod","scales","ggcorrplot","xlsx")
ipak(pkg)

RAWDATA <-  read.csv("c:/work/RAWDATA.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%
            mutate(STD_DT=as.Date(STD_DT))
STDDT <- "2023-07-13"%>%as.Date()
# source("c:/work/SOURCE V3.r")
# 
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/Quant UDF.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/SOURCE V3.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/SEVERFUNCTION.r")

EQUITY <- c("글로벌주식"="FEPS_WORLD",
  "선진국주식"          ="FEPS_DM",
  "신흥국주식"          ="FEPS_EM",
  "다우지수"            ="FEPS_DOW",
  "S&P500"              ="FEPS_SP500",
  "나스닥"              ="FEPS_NASDAQ",
  "코스피"              ="FEPS_KOSPI",
  "유로스톡스"          ="FEPS_EURO50", 
  "FTSE"                ="FEPS_FTSE",
  "니케이"              ="FEPS_NIKKEI",
  "독일DAX"             ="FEPS_DAX",
  "프랑스CAC"           ="FEPS_CAC",
  "캐나나TSX"           ="FEPS_TSX",
  "보베스파"            ="FEPS_BVSP",
  "상해지수"            ="FEPS_SHANGHAI",
  "CSI300"              ="FEPS_CSI300",
  "항셍"                ="FEPS_HANGS",
  "니프티"              ="FEPS_NIFTY"
  #"글로벌주식"="WORLD",
)

cols <- names(retsy)


ui <-
  navbarPage("Dashboard", theme = shinytheme("flatly"),
             tabPanel("주요주가지수",
                      dashboardPage(
                        dashboardHeader(title = "View"),
                        dashboardSidebar(dateRangeInput('DATE',
                                                        label = '',
                                                        start = as.Date('2018-01-01') , end = as.Date('2023-12-31')),
                                         selectInput("table", "Variable:",
                                                     c("수익률"   = "RT",
                                                       "이익전망" = "EPS")),
                                         checkboxGroupInput("EQ", "EQUITY", EQUITY)
                        ),
                        dashboardBody(
                            fluidRow(
                              shinydashboard::valueBox("1 Week","주가지수,EPS",color="red", width = 3),
                              shinydashboard::valueBox("1 Month","주가지수,EPS",color="blue", width = 3),
                              shinydashboard:: valueBox("YTD","주가지수,EPS",color="orange", width = 3),
                              shinydashboard:: valueBox("1 Year","주가지수,EPS",color="green", width = 3),
                            
                          ),
                          fluidRow(box(plotOutput("eqrtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("eqrtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("eqytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("eqrty"), width = 3, solidHeader = TRUE,background = "green")),
                          
                          fluidRow(box(plotOutput("fepsrtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("fepsrtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("fepsytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("fepsrty"), width = 3, solidHeader = TRUE,background = "green")),
                          fluidRow(box(DTOutput("tbl"), width = NULL, solidHeader = TRUE)),
                          fluidRow(box(plotOutput("EPS"), width =NULL))
                          
                        ))),
             
             tabPanel("스타일",
                      dashboardPage(
                        dashboardHeader(title = "View"),
                        dashboardSidebar(selectInput("table3", "Variable:",
                                                     c("수익률" = "RT",
                                                       "이익전망" = "EPS"))
                                         
                        ),
                        dashboardBody(
                            fluidRow(
                              shinydashboard::valueBox("1 Week","주가지수,EPS",color="red", width = 3),
                              shinydashboard::valueBox("1 Month","주가지수,EPS",color="blue", width = 3),
                              shinydashboard::valueBox("YTD","주가지수,EPS",color="orange", width = 3),
                              shinydashboard::valueBox("1 Year","주가지수,EPS",color="green", width = 3),
                            
                          ),
                          fluidRow(box(plotOutput("strtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("strtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("stytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("strty"), width = 3, solidHeader = TRUE,background = "green")),
                          
                          fluidRow(box(plotOutput("stfepsrtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("stfepsrtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("stfepsytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("stfepsrty"), width = 3, solidHeader = TRUE,background = "green")),
                          fluidRow(box(DTOutput("tbl3"), width = NULL, solidHeader = TRUE))
                        ))),
             
             tabPanel("채권&대체투자",
                      dashboardPage(
                        dashboardHeader(title = "View"),
                        dashboardSidebar(selectInput("table2", "Variable:",
                                                     c("채권" = "FI",
                                                       "대체투자" = "AI"))
                                         
                        ),
                        dashboardBody(
                          
                          fluidRow(
                            shinydashboard::valueBox("1 Week","수익률",color="red", width = 3),
                            shinydashboard::valueBox("1 Month","수익률",color="blue", width = 3),
                            shinydashboard::valueBox("YTD","수익률",color="orange", width = 3),
                            shinydashboard::valueBox("1 Year","수익률",color="green", width = 3),
                            
                          ),
                          fluidRow(box(plotOutput("firtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("firtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("fiytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("firty"), width = 3, solidHeader = TRUE,background = "green"))
                          ,
                          fluidRow(box(plotOutput("airtw"), width = 3, solidHeader = TRUE,background = "red"),box(plotOutput("airtm"), width = 3, solidHeader = TRUE,background = "blue"),
                                   box(plotOutput("aiytd"), width = 3, solidHeader = TRUE,background = "orange"),box(plotOutput("airty"), width = 3, solidHeader = TRUE,background = "green")),
                          fluidRow(box(DTOutput("tbl2"), width = NULL, solidHeader = TRUE))
                          # fluidRow(box(plotOutput("eq1"), width =5),
                          #          box(plotOutput("eq2"), width =5))
                        ))),
             
             tabPanel("주요주가지수(Historical)",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(dateRangeInput('dateRange',
                                                        label = '',
                                                        start = as.Date('2018-01-01') , end = as.Date('2023-12-31')),
                                         checkboxGroupInput("variable", "Variables to show:",
                                                            c("다우지수"="DOW",
                                                              "S&P500"="SP500",
                                                              "나스닥"="NASDAQ",
                                                              "코스피"="KOSPI",
                                                              "유로스톡스"="EURO50", 
                                                              "FTSE"="FTSE",
                                                              "니케이"="NIKKEI",
                                                              "독일DAX"="DAX",
                                                              "프랑스CAC"="CAC",
                                                              "캐나나TSX"="TSX",
                                                              "보베스파"="BVSP",
                                                              "상해지수"="SHANGHAI",
                                                              "CSI300"="CSI300",
                                                              "항셍"="HANGS",
                                                              "니프티"="NIFTY"
                                                              #"글로벌주식"="WORLD",
                                                            )),
                                         selectInput("variableh", "기초 자산군", cols[-1], selected = "나스닥")
                                         ),
                        dashboardBody(fluidRow(box(plotOutput("eq3"), width = 4, solidHeader = TRUE,background = "blue"),box(plotOutput("mdd"), width = 4, solidHeader = TRUE,background = "blue"),
                                               box(plotOutput("boxplot"), width = 4, solidHeader = TRUE,background = "blue")),
                                      fluidRow(box(plotOutput("Plot2"), width = 4, solidHeader = TRUE,background = "blue"),box(plotOutput("den"), width = 4, solidHeader = TRUE,background = "blue"),
                                               box(DTOutput("den2"), width = 4, solidHeader = TRUE))
                                              
                        )
                        )),
            tabPanel("MACRO INDEX",
             dashboardPage(
             dashboardHeader(),
             dashboardSidebar(dateRangeInput('RI',
                                            label = '',
                                            start = as.Date('2018-01-01') , end = as.Date('2023-12-31'))),
             dashboardBody(fluidRow(box(plotOutput("KRSP"), width = 4, solidHeader = TRUE),box(plotOutput("USSP"), width = 4, solidHeader = TRUE),box(plotOutput("GOVT"), width = 4, solidHeader = TRUE)),
                           fluidRow(box(plotOutput("SIGNAL"), width = 4, solidHeader = TRUE),box(plotOutput("SIGNAL2"), width = 4, solidHeader = TRUE),box(plotOutput("FX"), width = 4, solidHeader = TRUE)),
                           fluidRow(box(plotOutput("rollcor1"), width = 6, solidHeader = TRUE),box(plotOutput("rollcor2"), width = 6, solidHeader = TRUE))
                           )
            )
            ),
            tabPanel("FX",
                     dashboardPage(
                       dashboardHeader(),
                       dashboardSidebar(dateRangeInput('MP',
                                                       label = '',
                                                       start = as.Date('2018-01-01') , end = as.Date('2023-12-31'))),
                       dashboardBody(fluidRow(box(plotOutput("mp"), width = 6, solidHeader = TRUE),box(plotOutput("mp2"), width = 6, solidHeader = TRUE)),
                                     fluidRow(box(plotOutput("mp3"), width = 6, solidHeader = TRUE),box(DTOutput("mp4"), width = 6, solidHeader = TRUE)))
                       )
                     )
  )
      
shinyApp(ui, server)





