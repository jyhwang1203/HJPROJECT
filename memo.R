RES1  <-  RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRBTR,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("week")%>%dt_trans%>%tail(n=1)  %>%exname
RES2  <-  RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRBTR,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("month")%>%dt_trans%>%tail(n=1)%>%exname
RES3  <-  RAWDATA%>%select(STD_DT,WRINFRA,WREPRA,CRBTR,PEF,HFRI,SIL,WTI,GOLD,COP,WORLD,WRBOND)%>%trans_rt("year")%>%dt_trans%>%tail(n=1) %>%exname
RES4  <-  CLI%>%select(STD_DT,USA,KOR)
RES5  <-  retm%>%filter(STD_DT>"2014-05-30")%>%select(CRB,SIL,WTI,GOLD,COP,WORLD,INF)%>%na.omit%>%cor
RES6  <-  retm %>% select(STD_DT,EMGOVT,EMGOVTL,EMBOND,WRBOND)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%cuml
RES7  <-  RAWDATA %>% select(STD_DT,MOVE,EMSP)
RES8  <-coeff
#RES4  <-  RAWDATA %>%filter(STD_DT>"2010-12-31")%>% select(STD_DT,CNBR,BRBR,KRBR,INBR)


write.xlsx(RES1 ,"c:/work/RES.xlsx", sheetName="FUNDAMENTAL",append=F) 
write.xlsx(RES2 ,"c:/work/RES.xlsx", sheetName="BRATE",append=T) 
write.xlsx(RES3 ,"c:/work/RES.xlsx", sheetName="CLI",append=T) 
write.xlsx(RES4 ,"c:/work/RES.xlsx", sheetName="INFRA",append=T) 
write.xlsx(RES5 ,"c:/work/RES.xlsx", sheetName="EMBOND",append=T) 
write.xlsx(RES7 ,"c:/work/RES2.xlsx", sheetName="EMSP",append=T) 
write.xlsx(RES8 ,"c:/work/RES2.xlsx", sheetName="DELTA",append=T) 
RAWDATA %>% select(STD_DT,SP500,USBR,USBOND)

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
 
cuml <-  function(data){http://127.0.0.1:24171/graphics/plot_zoom_png?width=1328&height=753
    return(data.table(STD_DT=data$STD_DT,(1+data%>%select(-STD_DT))%>%cumprod) )
}

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

