RES1  <-  UNIV %>% left_join(retm%>%select(STD_DT,WORLD,WRBOND))%>%filter(STD_DT>"2022-12-31")
RES2  <-  RAWDATA %>% select(STD_DT,WORLD,DM,EM,FEPS_DM,FEPS_EM,FPER_DM,FPER_EM)
RES3  <-  RAWDATA %>%filter(STD_DT>"2010-12-31")%>% select(STD_DT,CNBR,BRBR,KRBR,INBR)
RES4  <-  CLI
RES5  <-  retm %>% select(STD_DT,WREPRA,WRINFRA,WORLD,HFRI)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%cuml
RES6  <-  retm %>% select(STD_DT,EMGOVT,EMGOVTL,EMBOND,WRBOND)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%cuml
RES7  <-  RAWDATA %>% select(STD_DT,EMX,EMSP)
RES8  <-coeff
#RES4  <-  RAWDATA %>%filter(STD_DT>"2010-12-31")%>% select(STD_DT,CNBR,BRBR,KRBR,INBR)


write.xlsx(RES2 ,"c:/work/RES.xlsx", sheetName="FUNDAMENTAL",append=F) 
write.xlsx(RES3 ,"c:/work/RES.xlsx", sheetName="BRATE",append=T) 
write.xlsx(RES4 ,"c:/work/RES.xlsx", sheetName="CLI",append=T) 
write.xlsx(RES5 ,"c:/work/RES.xlsx", sheetName="INFRA",append=T) 
write.xlsx(RES6 ,"c:/work/RES.xlsx", sheetName="EMBOND",append=T) 
write.xlsx(RES7 ,"c:/work/RES.xlsx", sheetName="EMSP",append=T) 
write.xlsx(RES8 ,"c:/work/RES.xlsx", sheetName="DELTA",append=T) 
RAWDATA %>% select(STD_DT,SP500,USBR,USBOND)


retm %>% select(STD_DT,EMGOVT,EMGOVTL,EMBOND,WRBOND)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%cuml%>%reshape2::melt(id.vars = "STD_DT")%>%
  ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
  geom_line()+
  theme(legend.text = element_text(size=15))
retm %>% select(STD_DT,WREPRA,WRINFRA,WORLD,HFRI)%>%na.omit%>%filter(STD_DT>"2010-01-01")%>%select(-STD_DT)%>%cor

RAWDATA %>% select(STD_DT,EMX,EMSP)%>%reshape2::melt(id.vars = "STD_DT")%>%
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

