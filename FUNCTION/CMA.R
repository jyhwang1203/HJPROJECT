
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","ggplot2","PerformanceAnalytics","writexl","plyr","quantmod",
        "flexdashboard","gridExtra","tidyverse","knitr","lubridate","tseries","nloptr","mvtnorm",'xlsx',"cvar")
ipak(pkg)
GDPCPI           <-  readxl::read_excel("c:/work/GDPCPI.xlsx",sheet="Sheet1")
GDPCPIF          <-  readxl::read_excel("c:/work/GDPCPI.xlsx",sheet="Sheet2")
WB               <-  read.csv("c:/work/WB.csv",stringsAsFactors =FALSE)%>%mutate(STD_DT=as.Date(STD_DT))

STD_DT           <-  GDPCPI[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
GDPCPI           <-  cbind(STD_DT,GDPCPI[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
TEMP             <-  apply(GDPCPI%>%select(-STD_DT), 2, as.numeric) 
GDPCPI           <-  data.frame(STD_DT,TEMP)

STD_DT           <-  GDPCPIF[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
GDPCPIF          <-  cbind(STD_DT,GDPCPIF[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
TEMP             <-  apply(GDPCPIF%>%select(-STD_DT), 2, as.numeric) 
GDPCPIF          <-  data.frame(STD_DT,TEMP)%>%fillf
#bond expected return
##########################################################################################################################################################
      STDDT <- ("2023-04-30")%>%as.Date
      TEMP <- GDPCPI%>%select(STD_DT,KR5Y)%>%fillf%>%
             right_join(GDPCPI%>%select(STD_DT,KRGDPY),by="STD_DT")%>%na.omit
      TEMP <- TEMP%>%filter(STD_DT<="2020-12-31"&STD_DT>"2010-12-31")%>%na.omit%>%left_join(GDPCPI%>%select(STD_DT,KRINFY),by="STD_DT")
      
      res2 <- lm(KR5Y~KRGDPY+KRINFY,data=TEMP)
      TMP  <- GDPCPIF%>%filter(STD_DT>STDDT-years(5)&STD_DT<=STDDT)
      TMP  <- xts(TMP%>%select(AA.),order.by=TMP$STD_DT)
      
      SPKR  <-(TMP%>%apply.yearly(.,sum)/365)%>%.[-6]%>%mean
      res6 <-(TMP%>%apply.yearly(.,sum)/365)%>%.[-6]
      TMP  <- summary(res2)
      res9 <-TMP$coefficients
      RTF1  <- GDPCPIF%>%select(STD_DT,KRGDP23,KRCPI23,KRGDP24,KRCPI24)%>%filter(STD_DT==STDDT)%>%.[-1]
      rfkr <- c(
        ((RTF1[1:2]*res2$coefficients[-1])%>%sum+res2$coefficients[1]),
        ((RTF1[3:4]*res2$coefficients[-1])%>%sum+res2$coefficients[1])
      )


      TEMP <- GDPCPI%>%select(STD_DT,US10Y)%>%fillf%>%
              right_join(GDPCPI%>%select(STD_DT,USGDPY,USINFY),by="STD_DT")%>%na.omit
      
      TEMP <- TEMP%>%filter(STD_DT<="2020-12-31"&STD_DT>"2010-12-31")%>%
              na.omit
      res4 <- lm(US10Y~USGDPY+USINFY,data=TEMP)
      TMP  <- GDPCPIF%>%filter(STD_DT>STDDT-years(5)&STD_DT<=STDDT)
      TMP  <- xts(TMP%>%select(AA),order.by=TMP$STD_DT)%>%na.omit
      SPUS  <-(TMP%>%apply.yearly(.,sum)/365)%>%.[-6]%>%mean/100
      res7 <-(TMP%>%apply.yearly(.,sum)/365)%>%.[-6]
      summary(res4)
      TTMP<- summary(res4)
      res8<- TTMP$coefficients
      
      RTF2<- GDPCPIF%>%select(STD_DT,WRGDP23,WRCPI23,WRGDP24,WRCPI24)%>%filter(STD_DT==STDDT)%>%.[-1]
      rfus <- c(
        ((RTF2[1:2]*res4$coefficients[-1])%>%sum+res4$coefficients[1]),
        ((RTF2[3:4]*res4$coefficients[-1])%>%sum+res4$coefficients[1])
      )
      rf <- rbind(rfkr,rfus)
      sp <- rbind(SPKR,SPUS)
      GDPCPI <- rbind(c(RTF1),c(RTF2))
      KRGDP23 <- GDPCPIF%>%select(STD_DT,KRGDP23)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      KRGDP24 <- GDPCPIF%>%select(STD_DT,KRGDP24)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      KRCPI23 <- GDPCPIF%>%select(STD_DT,KRCPI23)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      KRCPI24 <- GDPCPIF%>%select(STD_DT,KRCPI24)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      WRGDP23 <- GDPCPIF%>%select(STD_DT,WRGDP23)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      WRGDP24 <- GDPCPIF%>%select(STD_DT,WRGDP24)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      WRCPI23 <- GDPCPIF%>%select(STD_DT,WRCPI23)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      WRCPI24 <- GDPCPIF%>%select(STD_DT,WRCPI24)%>%filter(STD_DT==STDDT)%>%select(-STD_DT)
      WRGDP = (2.6 + 2.7)/2
      KRGDP = (1.2 + 2.2)/2
      WRCPI = (5.5 + 3.5)/2
      KRCPI = (3.3 + 2  )/2
      
      WRGDP+WRCPI+2.24
      KRGDP+KRCPI+2.22
      rt_eqwr23 <- WRGDP23+WRCPI23+2
      rt_eqwr24 <- WRGDP24+WRCPI24+2
      
      rt_eqkr23 <- KRGDP23+KRCPI23+1.5
      rt_eqkr24 <- KRGDP24+KRCPI24+1.5
      
      rt_fiwr23 <- rf[2,1]+sp[2]
      rt_fiwr24 <- rf[2,2]+sp[2]
      
      rt_fikr23 <- rf[1,1]+sp[1]
      rt_fikr24 <- rf[1,2]+sp[1]
      
      REG5 <- lm(WRINFRA  ~ 0+ WORLD+WRBOND,data=retm)%>% summary 
      REG6 <- lm(WREPRA~ 0+WORLD+WRBOND,data=retm)%>% summary 
      REG7 <- lm(GSCI~  0+WORLD+WRBOND,data=retm)%>% summary 
      REG8 <- lm(HFRI~  0+WORLD+WRBOND,data=retm)%>% summary 
      sum(REG5$coefficients[-1,1] * data.frame(rt_eqwr23,rt_fiwr23))
      sum(REG6$coefficients[-1,1]* data.frame(rt_eqwr23,rt_fiwr23))
      sum(REG7$coefficients[-1,1]* data.frame(rt_eqwr23,rt_fiwr23))
      
      
      mu23        <- matrix(0,1,6)
      mu23[1]     <- (WRGDP23+WRCPI23)%>%as.numeric()+2.24
      mu23[2]     <- (KRGDP23+KRCPI23)%>%as.numeric()+2.23
      mu23[3]     <-  rt_fiwr23
      # mu[4]<- 0.0267  +SP[3]/100 - 0.012
      mu23[4]     <-  rt_fikr23
      mu23[5]     <- (sum(REG5$coefficients[,1] * data.frame(rt_eqwr23,rt_fiwr23))+sum(REG6$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23))+sum(REG8$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23)))/3
      mu23[6]     <-  sum(REG7$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23))
      mu23        <- mu23/100
      
      #cov <- retm%>%filter(STD_DT>"2010-01-01") %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cov    
     # cov23 <- covEstimation(retm%>%filter(STD_DT>STDDT-years(5)&STD_DT<=STDDT) %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%as.matrix, control = list(type = 'ewma', lambda = 0.94))
   
      STD_DT>STDDT-years(5)
      
      mu24    <-  matrix(0,1,6)
      mu24[1] <- (WRGDP+WRCPI)%>%as.numeric()+2.24
      mu24[2] <- (KRGDP+KRCPI)%>%as.numeric()+2.23+1
      mu24[3] <-  rt_fiwr24
      # mu[4]<- 0.0267  +SP[3]/100 - 0.012
      mu24[4] <-  rt_fikr24
      mu24[5] <- (sum(REG5$coefficients[,1] * data.frame(rt_eqwr24,rt_fiwr24))+sum(REG6$coefficients[,1]* data.frame(rt_eqwr24,rt_fiwr24))+sum(REG8$coefficients[,1]* data.frame(rt_eqwr24,rt_fiwr24)))/3
      mu24[6] <-   sum(REG7$coefficients[,1] * data.frame(rt_eqwr24,rt_fiwr24))
      mu24    <- mu24/100
      #cov24   <- retm%>%filter(STD_DT>STDDT-years(10)&STD_DT<=STDDT) %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cov    
  
      cov24   <- retm%>%filter(STD_DT>STDDT-years(10)&STD_DT<=STDDT) %>%select(WORLD2,MSKR,WRBOND2,KRBOND,AL2,GSCI2)%>%cov    
      retm%>%filter(STD_DT>STDDT-years(10)&STD_DT<=STDDT) %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cor
      retm%>%filter(STD_DT>STDDT-years(10)&STD_DT<=STDDT)  %>%select(WORLD2,MSKR,WRBOND2,KRBOND,AL2,GSCI2)%>%cor
      
      CMA <- rbind(mu23,mu24,(cov24%>%diag*12)^0.5)
      rownames(CMA) <- c("return","vol","SR")
      STDDT-years(5)
      COVLONG  <- retm%>%filter(STD_DT>(STDDT-years(10))) %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cor
#      COVSHORT <-  covEstimation(retm%>%filter(STD_DT>(STDDT-years(5)))  %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%as.matrix, control = list(type = 'ewma', lambda = 0.94))
   #     
   #    write.xlsx(CMA ,"c:/work/MP.xlsx", sheetName="CMA",append=T)
   #    write.xlsx(COVLONG ,"c:/work/MP.xlsx", sheetName="COVLONG",append=T)
   # #   write.xlsx(COVSHORT ,"c:/work/MP.xlsx", sheetName="COVSHORT",append=T)
   # 
   #    
      