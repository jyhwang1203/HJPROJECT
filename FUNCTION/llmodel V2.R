cusum <- function(data) {
  fit <-auto.arima(data)
  autoplot(forecast(fit))
  fit$sigma2
  
  demean <- diff(data) %>%mean
  z <- diff(data)
  e <- fit$residuals
  e   %>%length
  tt  <- fit$x %>% length
  ttt <- tt-1
  
  res2 <- sapply(1:(e%>%length),function(t){
    ((abs(e[1:t]^2 %>%sum - sum(e^2)* t/(e%>%length)))/
       ((e%>%length*fit$sigma2)^0.5))%>%return
  })
  return(res2)
}

kt <- data.frame(LCfit$kt%>%as.numeric ,
           LCfitm$kt%>%as.numeric,
           LCfitf$kt%>%as.numeric
           )

source("c:/Users/ghkdw/OneDrive/Documents/GitHub/HJPROJECT/FUNCTION/mortality.R")

LCfit  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit)
LCfitm  <- StMoMo::fit(LC, data = BASEKRM, ages.fit = ages.fit)
#LLfitm <-  llmodel(BASEKRM,BASEKR)
LCfitf  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit)
#LLfitf <-  llmodel(BASEKRF,BASEKR)

LCfor  <- forecast::forecast(LCfit, h = 50)
LCform <- forecast::forecast(LCfitm, h = 50)
LCforf <- forecast::forecast(LCfitf, h = 50)
MXT    <- cbind(LCfitm$Dxt/LCfitm$Ext,LCform$rates)
FXT    <- cbind(LCfitf$Dxt/LCfitf$Ext,LCforf$rates)
TXT    <- cbind(LCfit$Dxt/LCfit$Ext,LCfor$rates)

source("c:/Users/ghkdw/OneDrive/Documents/GitHub/HJPROJECT/FUNCTION/mortality2.R")

QT <- BASEKR2$Dxt/BASEKR2$Ext
QM <- BASEKRM2$Dxt/BASEKRM2$Ext
QF <- BASEKRF2$Dxt/BASEKRF2$Ext
PT <- 1-QT
PM <- 1-QM
PF <- 1-QF

LCfit2  <- StMoMo::fit(LC, data = BASEKR2, ages.fit = ages.fit)
LCfitm2  <- StMoMo::fit(LC, data = BASEKRM2, ages.fit = ages.fit)
#LLfitm <-  llmodel(BASEKRM,BASEKR)
LCfitf2  <- StMoMo::fit(LC, data = BASEKRF2, ages.fit = ages.fit)
#LLfitf <-  llmodel(BASEKRF,BASEKR)

LCfor2  <- forecast::forecast(LCfit2, h = 50)
LCform2 <- forecast::forecast(LCfitm2, h = 50)
LCforf2 <- forecast::forecast(LCfitf2, h = 50)
MXT     <- cbind(LCfitm2$Dxt/LCfitm2$Ext,LCform2$rates)
FXT     <- cbind(LCfitf2$Dxt/LCfitf2$Ext,LCforf2$rates)
TXT     <- cbind(LCfit2$Dxt/LCfit2$Ext,LCfor2$rates)


RMXT     <- cbind(LCfitm$Dxt/LCfitm$Ext,LCform$rates)
RFXT     <- cbind(LCfitf$Dxt/LCfitf$Ext,LCforf$rates)
RTXT     <- cbind(LCfit$Dxt/LCfit$Ext,LCfor$rates)

# 남녀 기대여명
NT <- TXT %>%ncol
PT <- 1-TXT
ET <- sapply(c(1:NT),function(i){
      cumprod(PT[,i])%>%sum + (1-cumprod(PT[,i])%>%.[100])
      })
names(ET) <- c(1970:(1970+NT-1))

PM <- 1-MXT
NM <- MXT %>%ncol
  EM <- sapply(c(1:NM),function(i){
  cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
})
names(EM) <- c(1970:(1970+NM-1))

PF <- 1-FXT
NF <- FXT %>%ncol
EF <- sapply(c(1:NF),function(i){
  cumprod(PF[,i])%>%sum + (1-cumprod(PF[,i])%>%.[100])
})
names(EF) <- c(1970:(1970+NF-1))

#ultimate bx

wt <- ((EF-80)/22)%>%.[50]

# range period
# rotated lee carter

mxf <- BASEKRM$Dxt/BASEKRM$Ext
mxf <- BASEKRF$Dxt/BASEKRF$Ext

e0f <- as.numeric(subset(e0Fproj, name == country)[-(1:2)])
e0m <- as.numeric(subset(e0Mproj, name == country)[-(1:2)])
rownames(mxf) <- rownames(mxf) <- c(0,1, seq(2, 99, by=1))
lc <- lileecarter.estimate(mxf, mxf)
rotlc <- rotate.leecarter(lc$bx, lc$ultimate.bx,ET, e0l = 80, e0u = 102, p = 0.5)

plot(lc$bx, type="l")
lines(lc$ultimate.bx, col="red")
for(i in 1:ncol(rotlc)) lines(rotlc[,i], col="grey")


# 궁극의 연령대에 도달하는 시기는 아직 없음 2009년 80

#wt
w <- sapply(c(1:NT), function(i){
  (ET[i]-80)/(102-80)}) 


t <- as.character(c(1970:2071))
#t <- as.character(c(2009:(2071)))
#t <- which(w==w["2008"])
bu <- sapply(c(1:102), function(i){
  if(i<40){LCfit$bx}
  else{LCfit$bx*(1-w[t[i]])+w[t[i]]*lc$ultimate.bx}
}
)%>%as.data.frame()
colnames(bu) <- c((1970):(2071))

# EM <- sapply(c(1:50),function(i){
#   cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
# })
# names(EM)  <- c((1970+NT-50):(1970+NT-1))

t <- as.character(c(1970:2021))
#t <- as.character(c((1970+NT-50):(1970+NT-1)))
K_LLG_M <- sapply(c(1:52),function(i) {
  my_function <- function(K) {
    A <-matrix(data = (LCfitm$ax), nrow = 100, ncol = 1)
    PM <- 1- exp(rotlc[,t[i]]%>%as.matrix()*(K) + A)
    #K=30
    RES <- cumprod(PM)%>%sum + (1-cumprod(PM)%>%.[100]) - EM[t[i]]
    return(RES)  # Example: Find the sqare root of 4
  }
  uniroot(my_function,c(-400,110), tol = 1e-15)$root
})
K_LLG_M
LCform$kt.f$mean

  K_LLG_F <- sapply(c(1:52),function(i) {
  my_function <- function(K) {
    A <-matrix(data = (LCfitf$ax), nrow = 100, ncol = 1)
    PF <- 1 - exp(rotlc[,t[i]]%>%as.matrix()*(K) + A)
    #K=30
    RES <- cumprod(PF)%>%sum + (1-cumprod(PF)%>%.[100]) - EF[t[i]]
    return(RES)  # Example: Find the sqare root of 4
  }
  uniroot(my_function,c(-400,110), tol = 1e-15)$root
})
  K_LLG_F
  # 사망률 비교
  # real
  DX
  exp((LCfit$ax+LCfit$bx%*%FOR1$mean))
  
  
   #과거데이터로 적합성검증 LSTM모형 추가가
  LSTMKAPPA <- readxl::read_excel("c:/work/LSTMKAPPA.xlsx",sheet="Sheet1")
  
   tmp  <- cbind(K_LLG_M,K_LLG_F)
   kappa_fv <- cbind(STD_DT=c(2012:2021),
      auto.arima(K_LLG_M[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean,
      auto.arima(K_LLG_F[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean,
      auto.arima(LCfitm$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean,
      auto.arima(LCfitf$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean
      )%>%as.data.frame
   colnames(kappa_fv) <- c("STD_DT","Li-LEE-MALE","Li-LEE-FEMALE","LC-MALE","LC-FEMALE")
   kappa_fv <-kappa_fv %>% left_join(LSTMKAPPA,by="STD_DT")
   tmp5 <- cbind(LCfit2$ax,LCfitm2$ax,LCfitf2$ax)
   tmp6 <- cbind(LCfit2$bx,LCfitm2$bx,LCfitf2$bx)
   
   #REAL
   mxm <- BASEKRM$Dxt/BASEKRM$Ext
   mxf <- BASEKRF$Dxt/BASEKRF$Ext
   mxm_llg   <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$`Li-LEE-MALE`+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_llg   <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$`Li-LEE-FEMALE`+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxm_lc    <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$`LC-MALE`+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_lc    <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$`LC-FEMALE`+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxm_lstm  <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$LSTMM+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_lstm  <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$LSTMF+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   #MSE,RMSE
   
   MSEM <- data.frame(LC     =sum((mxm_lc  -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LLG    =sum((mxm_llg -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LSTM   =sum((mxm_lstm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100)
   )%>%round(4)
   
   
   MSEF <- data.frame(LC     =sum((mxf_lc  -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LLG    =sum((mxf_llg -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LSTM   =sum((mxf_lstm-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100)
   )%>%round(4)
   
   RMSEM <- data.frame(LC     =(sum((mxm_lc  -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LLG    =(sum((mxm_llg -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LSTM   =(sum((mxm_lstm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 
   )%>%round(4)
   
   RMSEF <- data.frame(LC     =(sum((mxf_lc  -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LLG    =(sum((mxf_llg -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LSTM   =(sum((mxf_lstm-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 
   )%>%round(4)
   
   
   MAPEM <- data.frame(LC     =sum(((mxm_lc  -mxm[,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100 ,
                       LLG    =sum(((mxm_llg -mxm[,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxm_lstm-mxm[,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
  
   
   MAPEF <- data.frame(LC     =sum(((mxf_lc  -mxf[,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100 ,
                       LLG    =sum(((mxf_llg -mxf[,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxf_lstm-mxf[,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
   
   MSEM <- sapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =sum((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10,
                LLG    =sum((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10,
                LSTM   =sum((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10
     )})%>%do.call(rbind,.)

   RMSEM <- sapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =(sum((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 ,
                LLG    =(sum((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 ,
                LSTM   =(sum((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 
     )})%>%do.call(rbind,.)
   plot(RMSEM[201:300],type="l")
   MAPEM <- sapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =sum(((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100 ,
                LLG    =sum(((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100,
                LSTM   =sum(((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100
     )})%>%do.call(rbind,.)
   
   
   
   MSEF <- sapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =sum((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10,
                LLG    =sum((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10,
                LSTM   =sum((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10
     )})%>%do.call(rbind,.)
   
   RMSEF <- sapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =(sum((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 ,
                LLG    =(sum((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 ,
                LSTM   =(sum((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])^2)/10)^0.5 
     )})%>%do.call(rbind,.)
   
   MAPE <- lapply(c(1:100), function(i){
     data.frame(AGE=i,
                LC     =sum(((mxf_lc[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100 ,
                LLG    =sum(((mxf_llg[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100,
                LSTM   =sum(((mxf_lstm[i,]-mxf[i,c(2012:2021)%>%as.character()])/mxf[i,c(2012:2021)%>%as.character()])%>%abs)/10*100
     )})%>%do.call(rbind,.)
  

   
   
   xlsx::write.xlsx(tmp ,"c:/work/kappa.xlsx", sheetName="tmp",append=F)
   xlsx::write.xlsx(tmp1 ,"c:/work/kappa.xlsx", sheetName="tmp1",append=T)
   xlsx::write.xlsx(tmp2 ,"c:/work/kappa.xlsx", sheetName="tmp2",append=T)
   xlsx::write.xlsx(tmp3 ,"c:/work/kappa.xlsx", sheetName="tmp3",append=T)
   xlsx::write.xlsx(tmp4 ,"c:/work/kappa.xlsx", sheetName="tmp4",append=T)  
   xlsx::write.xlsx(tmp5 ,"c:/work/kappa.xlsx", sheetName="tmp5",append=T)
   xlsx::write.xlsx(tmp6,"c:/work/kappa.xlsx",  sheetName="tmp6",append=T)
LLG_M<- c(LCfitm$kt%>%c,K_LLG_M%>%c)
names(LLG_M) <-  c((1970):(1970+NT-1))
LC_M<- c(LCfitm$kt%>%c,LCform$kt.f$mean%>%c)
names(LC_M) <-  c((1970):(1970+NT-1))
LLG_F<- c(LCfitf$kt%>%c,K_LLG_F%>%c)
names(LLG_F) <-  c((1970):(1970+NF-1))
LC_F<- c(LCfitf$kt%>%c,LCforf$kt.f$mean%>%c)
names(LC_F) <-  c((1970):(1970+NF-1))

data.frame(STD_DT=c(1970:(1970+NT-1)),LC_M,LLG_M) %>% cplot
data.frame(STD_DT=c(1970:(1970+NF-1)),LC_F,LLG_F) %>% cplot


MT_LCM<-sapply(c((1970+NT-50):(1970+NT-1))%>%as.character,function(i){
exp(bu[i]%>%as.matrix()*LC_M[i]+matrix(data = (LCfitm$ax), nrow = 100, ncol = 1))%>%round(5)
})

MT_LLGM<- sapply(c((1970+NT-50):(1970+NT-1))%>%as.character,function(i){
  exp(bu[i]%>%as.matrix()*LLG_M[i]+matrix(data = (LCfitm$ax), nrow = 100, ncol = 1))%>%round(5)
})




MT_LCF<-sapply(c((1970+NF-50):(1970+NF-1))%>%as.character,function(i){
  exp(bu[i]%>%as.matrix()*LC_F[i]+matrix(data = (LCfitf$ax), nrow = 100, ncol = 1))%>%round(5)
})

MT_LLGF<- sapply(c((1970+NF-50):(1970+NF-1))%>%as.character,function(i){
  exp(bu[i]%>%as.matrix()*LLG_F[i]+matrix(data = (LCfitf$ax), nrow = 100, ncol = 1))%>%round(5)
})


AGE <- 55
data.frame(STD_DT=c((1970+NT-50):(1970+NT-1)),LC_MALE=MT_LCM[AGE,],LLG_MALE=MT_LLGM[AGE,]) %>% 
  left_join(data.frame(STD_DT=c(1970:(1970+NT-1)),REAL_MALE=MXT[AGE,]),by="STD_DT")%>%filter(STD_DT<"2022")%>%cplot
  

AGE <- 55
data.frame(STD_DT=c((1970+NT-50):(1970+NF-1)),LC_FEMALE=MT_LCF[AGE,],LLG_FEMALE=MT_LLGF[AGE,]) %>% 
  left_join(data.frame(STD_DT=c(1970:(1970+NT-1)),REAL_FEMALE=FXT[AGE,]),by="STD_DT")%>%filter(STD_DT<"2022")%>%cplot

TIME <- 55
data.frame(STD_DT=c((1970+NT-50):(1970+NT-1)),LC_MALE=MT_LCM[AGE,],LLG_MALE=MT_LLGM[AGE,]) %>% 
  left_join(data.frame(STD_DT=c(1970:(1970+NT-1)),REAL_MALE=MXT[AGE,]),by="STD_DT")%>%filter(STD_DT<"2022")%>%cplot


  
  
plot(lc$bx, type="l")
lines(lc$ultimate.bx, col="red")
for(i in 1:ncol(rotlc)) lines(rotlc[,i], col="grey")
  
  
  
  ui <-     navbarPage("LI-LEE류", theme = shinytheme("flatly"),
                       tabPanel("기대여명",dashboardPage(dashboardHeader(),
                                                     dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31'))),
                                                     dashboardBody(fluidRow(box(plotOutput("EM"), width =6, solidHeader = TRUE),box(plotOutput("EF"), width =6, solidHeader = TRUE)),
                                                                   fluidRow(box(plotOutput("UBU"), width =12, solidHeader = TRUE),
                                                                   fluidRow(box(plotOutput("KTM"), width =6, solidHeader = TRUE),box(plotOutput("KTF"), width =6, solidHeader = TRUE))
                                                                            
                                                                            )
                                                     ))))
  server <- function(input, output)
  {   
    output$EM <- renderPlot({
      QM <- MXT
      PM <- 1-QM
      EM <- sapply(c(1:NT),function(i){
        cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
      })
      data.frame(STD_DT=c(1970:(1970+NT-1)),EM,80,102)%>%cplot
      
    })
    output$EF <- renderPlot({
      QF <- FXT
      PF <- 1-QF
      EF <- sapply(c(1:NT),function(i){
        cumprod(PF[,i])%>%sum + (1-cumprod(PF[,i])%>%.[100])
      })
      
      data.frame(STD_DT=c(1970:(1970+NT-1)),EF,80,102)%>%cplot
    })
    output$UBU <- renderPlot({
      plot(lc$bx, type="l")
      lines(lc$ultimate.bx, col="red")
      for(i in 1:ncol(rotlc)) lines(rotlc[,i], col="grey")
    })
    output$KTF <- renderPlot({
      data.frame(STD_DT=c(1970:(1970+NT-1)),LC_M,LLG_M) %>% cplot
    
    })
    output$KTM <- renderPlot({
      data.frame(STD_DT=c(1970:(1970+NF-1)),LC_F,LLG_F) %>% cplot
    })
  }
   
  shinyApp(ui, server)
 
