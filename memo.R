std <-   cbind(eq,fi[,-1],al[,-1],feps[,-1])  %>% filter(STD_DT==(STDDT))
week<-   cbind(eq,fi[,-1],al[,-1],feps[,-1])  %>% trans_rt("week") %>%dt_trans %>% filter(substr(STD_DT,1,7)==substr(STDDT,1,7))%>%tail(n=1)
month <- cbind(eq,fi[,-1],al[,-1],feps[,-1])%>% trans_rt("month")%>%dt_trans %>% filter(substr(STD_DT,1,7)==substr(STDDT,1,7))%>%exname
year  <- cbind(eq,fi[,-1],al[,-1],feps[,-1]) %>% trans_rt("year") %>%dt_trans %>% filter(substr(STD_DT,1,4)==substr(STDDT,1,4))%>%exname

retm%>%dplyr::select(STD_DT,INF,RINTEREST,WTI)%>%filter(STD_DT>"2010-01-01")%>%cuml%>%cplot
lm(PEF ~ GROWTH+INF+CREDIT+RINTEREST+GSCI,data=retm%>%filter(STD_DT>"2017-01-01"))%>% summary 

retm%>%dplyr::select(STD_DT,INF,RINTEREST,WTI)%>%filter(STD_DT>"2010-01-01")%>%cuml%>%dt_trans%>%left_join(RAWDATA%>%dpylr::select(STD_DT,USBR),by="STD_DT")

RES1 <- RAWDATA%>%filter(STD_DT>"2018-01-01")%>%select(STD_DT,KOSPI,FEPS_KOSPI,USDKRW,USBR,KRBR)
RES2 <- RAWDATA%>%filter(STD_DT>"2018-01-01")%>%select(STD_DT,USBR,KRBR,EUBR,UKBR,US2Y,US10Y,USLIB)
tmp  <- RAWDATA%>%select(STD_DT,WORLD,WRBOND,KOSPI,KRBOND) %>%trans_rt("week")%>%dt_trans%>%na.omit
tmp2 <- RAWDATA%>%select(STD_DT,WORLD,WRBOND,KOSPI,KRBOND) %>%trans_rt("month")%>%dt_trans%>%na.omit
RES3 <- data.table(STD_DT=tmp$STD_DT,ROLLIMG6M = roll_cor(tmp$WORLD, tmp$WRBOND, width = 27),ROLLIMG6M = roll_cor(tmp$KOSPI, tmp$KRBOND, width = 27))
RES4 <- data.table(STD_DT=tmp2$STD_DT,ROLLING3Y = roll_cor(tmp2$WORLD, tmp2$WRBOND, width = 36),ROLLING3Y = roll_cor(tmp2$KOSPI, tmp2$KRBOND, width = 36))
RES5 <- RAWDATA%>%select(STD_DT,HYSP,IGSPAAA,MOVE,VIX,EMSP,EMX)%>%filter(STD_DT>"2019-01-01")
eq <-data.frame(bind_rows(reteq_wek,reteq_mon,reteq_ytd,reteq_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
eqeps <- data.frame(bind_rows(retfeps_wek,retfeps_mon,retfeps_ytd,retfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
fi <- data.frame(bind_rows(retfi_wek,retfi_mon,retfi_ytd,retfi_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
ai <- data.frame(bind_rows(retai_wek,retai_mon,retai_ytd,retai_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
st <- data.frame(bind_rows(retst_wek,retst_mon,retst_ytd,retst_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
steps <- data.frame(bind_rows(retstfeps_wek,retstfeps_mon,retstfeps_ytd,retstfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)

RES6 <- retm %>% select(STD_DT,WORLD,DM,EM,USGROWTH,KOSPI,MSKR,WRBOND,USIG,USHY,USLONG,EMGOVTL,KRBOND,AL,WRINFRA,WREPRA,HFRI,CRB,GSCI,WTI,GOLD) %>% filter(STD_DT>"2022-12-31")

 RES1   <-  HAA("2010-01-30","2023-08-01",n=4,nn=1,UNIV_HAA)%>%select(STD_DT,HAA)%>%left_join(
   BAA("2010-01-30","2023-07-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%left_join(
     RET_MACRO%>%select(STD_DT,MP,BM2),by="STD_DT")
 
RES7<- RES1 %>% PA
RES8<- RES1 %>% cuml
RES9<- RES1
 RET_MACRO
 RES1<- RAWDATA%>%filter(STD_DT>"2018-01-01")%>%select(STD_DT,WTI)%>%left_join(CLI%>%select(STD_DT,USA),by="STD_DT")
write.xlsx(RES3 ,"c:/work/RES1.xlsx", sheetName="FUNDAMENTAL",append=T) 
write.xlsx(month ,"c:/work/RES2.xlsx", sheetName="BRATE",append=F) 
write.xlsx(year ,"c:/work/RES3.xlsx", sheetName="RES3",append=T) 
write.xlsx(RES4 ,"c:/work/RES4.xlsx", sheetName="INFRA",append=T) 
write.xlsx(RES5 ,"c:/work/RES.xlsx", sheetName="RE3S5",append=T)
write.xlsx(RES6 ,"c:/work/RES.xlsx", sheetName="RES6",append=T)
write.xlsx(RES7 ,"c:/work/RES.xlsx", sheetName="RES7",append=T) 
write.xlsx(RES8 ,"c:/work/RES.xlsx", sheetName="RES8",append=T)
write.xlsx(RES9 ,"c:/work/RES.xlsx", sheetName="RES9",append=T)
write.xlsx(CLI ,"c:/work/RES.xlsx", sheetName="CLI",append=T) 
write.xlsx(eq ,"c:/work/RES2.xlsx", sheetName="eq",append=F) 
write.xlsx(eqeps ,"c:/work/RES2.xlsx", sheetName="eqeps",append=T) 
write.xlsx(fi ,"c:/work/RES2.xlsx", sheetName="fi",append=T) 
write.xlsx(ai ,"c:/work/RES2.xlsx", sheetName="ai",append=T) 
write.xlsx(st ,"c:/work/RES2.xlsx", sheetName="st",append=T) 
write.xlsx(steps ,"c:/work/RES2.xlsx", sheetName="steps",append=T) 

write.xlsx(mu24 ,"c:/work/RES.xlsx", sheetName="wei",append=T) 

RAWDATA%>%filter(STD_DT>"2015-01-01")%>%select(STD_DT,KOSPI,USREIT,WRREIT)%>%cplot
RAWDATA%>%filter(STD_DT>"2015-01-01")%>%select(STD_DT,KOSPI,USREIT,WRREIT)%>%trans_rt("week")%>%dt_trans%>%cuml%>%cplot
RAWDATA%>%filter(STD_DT>"2015-01-01")%>%select(STD_DT,KOSPI,USREIT)
RAWDATA%>%filter(STD_DT>"2015-01-01")%>%select(STD_DT,KOSPI,USREIT)

tmp<- cbind(TQQQ$TQQQ.Adjusted,TMF$TMF.Adjusted)
colnames(tmp)<-c("TQQQ","TMF")
RAWDATA%>%select(STD_DT,USBOND,USDKRW)%>%mutate(USBONDH=USBOND*USDKRW)%>%trans_rt("week")%>%dt_trans%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

tmp  <- RAWDATA%>%select(STD_DT,SP500,USGOVT) %>%trans_rt("week")%>%dt_trans%>%na.omit
RES3<- data.table(STD_DT=tmp$STD_DT,ROLLIMG14week = roll_cor(tmp$SP500, tmp$USGOVT, width = 26))

tmp2  <- RAWDATA%>%select(STD_DT,SP500,USGOVT) %>%trans_rt("month")%>%dt_trans%>%na.omit
data.table(STD_DT=tmp2$STD_DT,ROLLIMG14week = roll_cor(tmp2$SP500, tmp2$USGOVT, width = 36))%>%cplot


TQQQ$TQQQ.Adjusted%>%dt_trans%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))


EMLC$EMLC.Adjusted%>%dt_trans%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

RAWDATA%>%select(STD_DT,NASDAQ,USGOVT)%>%mutate(BM=0.6*NASDAQ+0.4*USGOVT)%>%trans_rt("year")%>%dt_trans

RAWDATA%>%select(STD_DT,NASDAQ,USGOVT)%>%trans_rt("month")%>%dt_trans%>%mutate(BM=0.6*NASDAQ+0.4*USGOVT) %>%cuml
RAWDATA%>%select(STD_DT,DOW)%>%trans_rt("week")%>%dt_trans%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

RAWDATA%>%select(STD_DT,NASDAQ,USGOVT)%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2018-01-01"&STD_DT<"2020-01-01")%>%mutate(BM=0.6*NASDAQ+0.4*USGOVT)%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

g1 <- RAWDATA %>%select(STD_DT,USBR,US10Y)%>%na.omit%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

g2 <- retm %>%select(STD_DT,USLONG,USGOVT)%>%cuml%>%na.omit%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
grid.arrange( g1,g2,nrow=2)

RAWDATA%>%select(STD_DT,WTI,CRBTR,COP)%>%filter(STD_DT>"2015-01-01")
RAWDATA%>%select(STD_DT,WTI,COP)%>%filter(STD_DT>"2021-03-30")%>%trans_rt("week")%>%dt_trans%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
  
graphbar(reteq_wek,"red","Weekly Return")
g1<- RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRB,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("week")%>%dt_trans%>%tail(n=2) %>%.[1,]%>%graphbar("red","month Return")
g2<- RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRB,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("month")%>%dt_trans%>%tail(n=2)%>%.[1,]%>%graphbar("red","quarter Return")
g3<- RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRB,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("year")%>%dt_trans%>%tail(n=1) %>%.[1,]%>%graphbar("red","year Return")
grid.arrange( g1,g2,g3,ncol=3)

RAWDATA%>%filter(STD_DT>"2011-05-30")%>%select(STD_DT,WRINFRA,WREPRA,CRB,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("week")%>%dt_trans%>%tail(n=2)%>%.[1,]%>%graphbar("red","month Return")

retm%>%filter(STD_DT>"2014-05-30")%>%select(CRB,SIL,WTI,GOLD,COP,WORLD,INF)%>%na.omit%>%cor
retm%>%select(STD_DT,WTI)%>%left_join(CLI%>%select(STD_DT,USA),by="STD_DT")%>%na.omit%>%select(-STD_DT)%>%cor
temp<- RAWDATA%>%select(STD_DT,CRB,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("month")%>%data.frame

temp[temp>0,]%>%na.omit%>%cor
temp[temp<0,]%>%na.omit%>%cor

retm%>%select(STD_DT,WTI)%>%left_join(CLI%>%select(STD_DT,USA),by="STD_DT")%>%na.omit%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))




 RAWDATA%>%select(STD_DT,WREPRA,WRINFRA,WORLD)%>%filter(STD_DT>"2021-03-30")%>%trans_rt("week")%>%dt_trans%>%cov
  

  
  tmp  <- RAWDATA%>%select(STD_DT,WORLD,WRBOND) %>%trans_rt("week")%>%
  tmp2 <- data.table(STD_DT=tmp$STD_DT,ROLLIMG14week = roll_cor(tmp$WORLD, tmp$WRBOND, width = 14))
  
  tmp3 %>%full_join(tmp2,by="STD_DT")%>%na.omit%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
    geom_line(size=1)

  retm %>% select(STD_DT,EMGOVT,EMGOVTL)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))


RES7 %>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

 retm %>% select(STD_DT,SP500,USHY,WRHY,WORLD)%>%na.omit%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))

 retm %>% select(STD_DT,WRTIP,USTIP)%>%na.omit%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))
 RAWDATA %>% select(STD_DT,EMGOVTL,EMGOVT,DXY)%>%filter(STD_DT>"2021-12-31")%>%trans_rt("day")%>%dt_trans%>%na.omit%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))
 
 RAWDATA %>% select(STD_DT,EMSP,EMX)%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))
 
 retm %>% select(STD_DT,BM)%>%PA

 RAWDATA %>%filter(STD_DT>"2020-12-31")%>% select(STD_DT,CNBR,BRBR,KRBR,INBR)%>%fillf%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))
 
 RAWDATA %>%filter(STD_DT>"2020-12-31")%>% select(STD_DT,CNBR,BRBR,KRBR,INBR)%>%fillf%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))
 
 RAWDATA %>% select(STD_DT,NASDAQ,USGOVT)%>%trans_rt("day")%>%dt_trans()%>%mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USGOVT)%>%mutate(PORT=TQQQ*0.6+TMF*0.4)%>%select(STD_DT,TMF,TQQQ,PORT)%>%na.omit %>% 
 apply.yearly(., Return.cumulative)
 
 
 RAWDATA %>% select(STD_DT,NASDAQ,USGOVT)%>%trans_rt("week")%>%dt_trans()%>%mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USGOVT)%>%mutate(PORT=TQQQ*0.6+TMF*0.4)%>%select(STD_DT,TMF,TQQQ,PORT)%>%na.omit %>% 
   cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
   ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
   geom_line()+
   theme(legend.text = element_text(size=15))

 
 
 getSymbols('TQQQ', src='yahoo')
#getSymbols('FNGU', src='yahoo')
getSymbols('TMF', src='yahoo')
getSymbols('PDBC', src='yahoo')
getSymbols('UUP', src='yahoo')
getSymbols('EMB', src='yahoo')
getSymbols('EMLC', src='yahoo')
PDBC$PDBC.Adjusted


TMP <- EMB$EMB.Adjusted%>%dt_trans%>%left_join(EMLC$EMLC.Adjusted%>%dt_trans,by="STD_DT")

TMP%>%na.omit%>%filter(STD_DT<"2024-01-01")%>%trans_rt("week")%>%dt_trans%>% cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))


TMP<- TQQQ$TQQQ.Adjusted%>%dt_trans%>%left_join(TMF$TMF.Adjusted%>%dt_trans,by="STD_DT")%>%left_join(UUP$UUP.Adjusted%>%dt_trans ,by="STD_DT")%>%left_join(FNGU$FNGU.Adjusted%>%dt_trans ,by="STD_DT")
colnames(TMP)<-c("STD_DT","TQQQ","TMF","UUP","FNGU")

TMP[,-2]%>%na.omit%>%filter(STD_DT<"2024-01-01")%>%trans_rt("week")%>%dt_trans%>%mutate(BM=0.4*FNGU+0.4*TMF+0.2*UUP) %>% cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
TMP[,-2]%>%na.omit%>%filter(STD_DT<"2024-01-01")%>%trans_rt("week")%>%dt_trans%>%mutate(BM=0.4*FNGU+0.4*TMF+0.2*UUP) 
TMP[,-4]%>%trans_rt("month")%>%dt_trans%>%filter(STD_DT<"2015-01-01")%>%mutate(BM=0.5*TQQQ+0.5*TMF) %>% cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
TMP[,-4]%>%trans_rt("month")%>%dt_trans%>%filter(STD_DT<"2015-01-01")%>%mutate(BM=0.4*TQQQ+0.6*TMF) %>% PA

RAWDATA%>%select(STD_DT,USMOM)%>%trans_rt("week")%>%dt_trans%>% cuml%>%mutate(V1=100*V1%>%log)%>%inner_join(CLI,by="STD_DT")%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
UNIV %>% left_join(CLI,by='STD_DT')%>%select(STD_DT,state,USA)%>%mutate(USA=USA/100)%>%reshape2::melt(id.vars = "STD_DT")%>%
                                                                          ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
                                                                          geom_line()+
                                                                          theme(legend.text = element_text(size=15))

                    

# lamda <- 0.99
TMP  <-  retm %>%filter(STD_DT>"2010-01-01")
REG1 <-  lm(NASDAQ ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
REG2 <-  lm(SP500~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
REG3 <-  lm(EMBOND~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
REG4 <-  lm(EMGOVTL ~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
REG5 <- lm(WRBOND~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 


coeff <<- rbind(REG1$coefficients[-1,1],
                REG2$coefficients[-1,1],
                REG3$coefficients[-1,1],
                REG4$coefficients[-1,1],
                REG5$coefficients[-1,1])                                                    
coeff

