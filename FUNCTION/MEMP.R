TMP <- RAWDATA %>% filter(variable=="DXY"|variable=="GOLD"|variable=="TIP10Y")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"2000-01-01")
TMP %>%cplot
write.xlsx(res ,"c:/work/monthly.xlsx", sheetName="res",append=F) 

getSymbols('EWY', src='yahoo')
getSymbols('SPY', src='yahoo')
getSymbols('QQQ', src='yahoo')
getSymbols('VGK', src='yahoo')
getSymbols('EWJ', src='yahoo')
getSymbols('ASHR', src='yahoo')
getSymbols('EMXC', src='yahoo')
getSymbols('IUSB', src='yahoo')
getSymbols('FLOT', src='yahoo')
getSymbols('EMB', src='yahoo')
getSymbols('VCSH', src='yahoo')
getSymbols('BKLN', src='yahoo')
getSymbols('OIH', src='yahoo')
getSymbols('REET', src='yahoo')


TEMP <- RAWDATA %>% filter(variable=="WORLD")%>%dcast(STD_DT~variable)%>%full_join(
  EWY$EWY.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
  SPY$SPY.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
    QQQ$QQQ.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
      VGK$VGK.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
        EWJ$EWJ.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
         
            ASHR$ASHR.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
              EMXC$EMXC.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                IUSB$IUSB.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                  FLOT$FLOT.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                    EMB$EMB.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                      VCSH$VCSH.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                        BKLN$BKLN.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                          OIH$OIH.Adjusted %>%dt_trans(), by="STD_DT")%>% left_join(
                            REET$REET.Adjusted %>%dt_trans(), by="STD_DT")%>%as.data.frame%>%fillf
STDDT <- ("2024-01-23")%>%as.Date()
std <- TEMP %>% filter(STD_DT==(STDDT))

month <- TEMP %>% filter(STD_DT==(STDDT %m-%months(1)))
month2<- TEMP %>% filter(STD_DT==(STDDT%m-%months(3)))
month3<- TEMP %>% filter(STD_DT==(STDDT%m-%months(6)))
year <- TEMP %>% filter(STD_DT==(STDDT%m-%years(1)))
year2 <- TEMP %>% filter(STD_DT==(STDDT%m-%years(3)))


ret_mon <- ((std[-1]/month[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame%>%t
ret_mon2 <- ((std[-1]/month2[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame%>%t
ret_mon3 <- ((std[-1]/month3[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame%>%t
ret_yea <- ((std[-1]/year[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame%>%t
ret_yea2 <- ((std[-1]/year2[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame%>%t

res <- cbind(ret_mon,ret_mon2,ret_mon3,ret_yea,ret_yea2)%>%as.data.frame

VCSH$VCSH.Adjusted %>%dt_trans()%>% left_join(
  IUSB$IUSB.Adjusted %>%dt_trans(), by="STD_DT") %>% na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2011-01-01")%>%cuml%>%cplot("MM")
AGG$AGG.Adjusted %>%dt_trans()%>% left_join(
  VCSH$VCSH.Adjusted %>%dt_trans(), by="STD_DT") %>% na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2011-01-01")%>%cuml%>%cplot("MM")

  
TBIL$TBIL.Adjusted%>%dt_trans()%>%trans_rt("week")%>%dt_trans%>%cuml%>%cplot
  BIL$BIL.Adjusted %>%dt_trans()%>%trans_rt("week")%>%dt_trans%>%cuml%>%cplot
TLT$TLT.Adjusted %>%dt_trans()%>%trans_rt("week")%>%dt_trans%>%cuml%>%cplot
  
      QTMP<- RAWDATA%>%filter(variable=="OILINV")%>%dcast(STD_DT~variable) %>%
  mutate(rolling_avg = rollmean(OILINV, k=26, fill=NA, align='right'))
RAWDATA%>%filter(variable=="OILINV")%>%dcast(STD_DT~variable) %>%
  mutate(rolling_avg = rollmean(OILINV, k=26, fill=NA, align='right'))%>%cplot

rrrr<-  TMP %>% inner_join(RAWDATA%>%filter(variable=="SP500")%>%dcast(STD_DT~variable),by="STD_DT")

write.csv(TTMP ,"c:/work/rrrr.csv")    

RAWDATA%>%filter(variable=="GOLD"|variable=="SP500")%>%dcast(STD_DT~variable)%>%trans_rt("day")%>%na.omit%>%dt_trans%>%arrange(STD_DT)%>%cuml%>%
  left_join(RAWDATA%>%filter(variable=="USBR")%>%dcast(STD_DT~variable),by="STD_DT")%>%cplot


RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT"|variable=="CRBTR")%>%dcast(STD_DT~variable)%>%trans_rt("day")%>%dt_trans%>%
  mutate(USGOVT=USGOVT)%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%mutate(BM2=0.5*SP500+0.3*USGOVT+0.2*CRBTR)%>%na.omit%>%arrange(STD_DT)%>%cuml%>%cplot


RAWDATA%>%filter(variable=="NASDAQ"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"2010-01-01")%>%trans_rt("day")%>%dt_trans%>% filter(STD_DT>"2015-07-01")%>%
  mutate(USGOVT=3*USGOVT)%>%mutate(SP500=3*NASDAQ)%>%mutate(BM=0.7*SP500+0.4*USGOVT)%>%na.omit%>%arrange(STD_DT)%>%cuml%>%cplot
CLI%>%filter(variable=="USA")%>%dcast(STD_DT~variable)%>%cplot
  
RAWDATA%>%filter(variable=="DXY"|variable=="GSCI"|variable=="SPER"|variable=="SPAR"|variable=="GOLD"|variable=="SIL"|variable=="COP"|variable=="FNG"|variable=="ENG") %>%dcast(STD_DT~variable)%>%
    trans_rt("week")%>%round(4) %>%dt_trans%>% filter(STD_DT>"2023-07-01")
  
  dplyr::select(STD_DT,DXY,GSCI,SPEN,SPAR,WTI,GOLD,)
  RAWDATA%>%filter(variable=="USCPIYOY")
  RAWDATA%>%filter(variable=="WTI") %>% reshape2::dcast(STD_DT~variable)
  ttmp <- RAWDATA%>%filter(variable=="WTI"|variable=="USCPIYOY"|variable=="USCORECPIYOY"|variable=="USA")%>% dcast(STD_DT~variable)
  RAWDATA%>%filter(variable=="USCPIYOY"|variable=="WTI")%>% reshape2::dcast(STD_DT~variable)
  RAWDATA%>%filter(variable=="USCPIYOY"|variable=="WTI")%>%na.omit%>%
    ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
    geom_line(size=1)
  write.xlsx(ttmp ,"c:/work/monthly.xlsx", sheetName="COVERDCALL",append=F)     
  
  RAWDATA
RAWDATA%>%dplyr::select(STD_DT,BEI10Y)
RAWDATA%>%dplyr::select(STD_DT,EMGOVT,WRBOND,TIP)%>%na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2022-01-01")%>%cuml%>%inner_join(RAWDATA %>% dplyr::select(STD_DT,bei10y), by="STD_DT")%>%cplot
RAWDATA%>%select(STD_DT,USBOND,USGOVT,WRTIP,USTIP)%>%na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2000-01-01")%>%cuml%>%cplot
RAWDATA%>%dplyr::select(STD_DT,MSKR,KOSPI,KOSDAQ)%>%na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT>"2010-01-01")%>%cuml%>%cplot
TMP <- RAWDATA%>%dplyr::select(STD_DT,DAX)%>%na.omit%>%trans_rt("day")%>%dt_trans%>%na.omit%>%cuml%>%right_join(RAWDATA %>% dplyr::select(STD_DT,DEU)%>%na.omit, by="STD_DT")
# 패키지 불러오기
library(depmixS4)
retm %>% dplyr::select(STD_DT,INF)
data1 <-  RAWDATA%>%filter(variable=="USA")%>%dcast(STD_DT~variable)%>%dplyr::select(USA)%>%na.omit%>%t%>%c
data2 <- retm %>% dplyr::select(INF)%>%na.omit%>%t%>%c

# 모델 정의
mod2 <- depmix(response = data2 ~ 1, family = gaussian(), nstates = 2, data = data.frame(data = data2))
mod1 <- depmix(response = data1 ~ 1, family = gaussian(), nstates = 2, data = data.frame(data = data1))


# 모델 적합
fit.mod1 <- fit(mod1)
fit.mod2 <- fit(mod2)
summary(fit.mod1)
summary(fit.mod2)
retm %>% dplyr::select(STD_DT,INF)%>%cuml%>%cplot
ttmp <-RAWDATA%>%filter(variable=="USCPIYOY")%>% dcast(STD_DT~variable) %>% full_join(retm %>% dplyr::select(STD_DT,INF)%>%cuml,by="STD_DT")%>%filter(STD_DT>"2010-01-01")
library("depmixS4")
data("speed")
set.seed(1)
  mod <- depmix(response = rt ~ 1, data = speed, nstates = 2, trstart = runif(4))
