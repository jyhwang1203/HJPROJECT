
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("data.table","PerformanceAnalytics","writexl","plyr","quantmod","lubridate","gdata","ggplot2","xlsx","stocks","tseries")
ipak(pkg)
source("c:/work/UDF.r")

RAWDATA           <-        read.csv("c:/work/RAWDATA.csv",stringsAsFactors = FALSE,header=T) %>% select(-X) %>%as.data.frame()%>%
mutate(STD_DT=STD_DT%>%as.Date())
TEMP      <-  readxl::read_excel("c:/work/universe.xlsx",sheet="Sheet2")
#TEMP      <-  readxl::read_excel("c:/work/PAA.xlsx",sheet="Sheet2")
STD_DT             <-  TEMP[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
TEMP            <-  cbind(STD_DT,TEMP[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
TEMP2               <-  apply(TEMP%>%select(-STD_DT), 2, as.numeric)


PAA           <-  data.frame(STD_DT,TEMP2)%>%mutate(STD_DT=as.Date(STD_DT))%>%fillf
PAA %>% select(STD_DT,SPY,IEF)%>%filter(STD_DT>"2000-01-01")%>%trans_rt("yeare")
CANARY <- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%fillf%>%trans_rt("month")%>%dt_trans()
DATE <- CANARY
"2001-05-30"
"2012-06-30"
START <- "2007-01-01"%>%as.Date
END  <- "2023-04-30"%>%as.Date

DATE <- DATE %>%  filter(STD_DT>=START&STD_DT<=END)
DATE <- DATE$STD_DT

ll<- DATE%>%length

RES<- lapply(c(13:ll),function(T){

  STD_DT <-DATE[T]
  p0 <- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%filter(STD_DT==DATE[T])%>%.[-1]   
  p1 <- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%filter(STD_DT==DATE[T-1]) %>%.[-1]   
  p3 <- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%filter(STD_DT==DATE[T-3]) %>%.[-1]   
  p6 <- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%filter(STD_DT==DATE[T-6]) %>%.[-1]   
  p12<- PAA%>%select(STD_DT,AGG,SPY,VWO,VEA)%>%filter(STD_DT==DATE[T-12])%>%.[-1]   

  mom<- 12*(p0/p1-1)+4*(p0/p3-1)+2*(p0/p6-1)+(p0/p12-1)
       
  res<- data.frame(STD_DT,mom)
return(res)
})

RES2 <- do.call(rbind,RES)
RES2[is.na(RES2)] <- 0
RES2%>%str
#RES2<- RES2%>%na.omit

  UNIV<- data.frame(rbind(RES2%>%filter(AGG<0|SPY<0|VWO<0|VEA<0)%>%mutate(state=0),
  RES2%>%filter(AGG>=0&SPY>=0&VWO>=0&VEA>=0)%>%mutate(state=1)))%>%arrange(STD_DT)%>%mutate(STD_DT_L=lead(STD_DT,1))


#수익률이 아니라 가격임
#sma filter
SMA <- CANARY%>%select(STD_DT)%>%left_join(PAA,by="STD_DT")%>%mutate(cash=100*1)%>%mutate(cash2=100*1)%>%mutate(cash3=100)%>%mutate(BIL2=BIL)%>%mutate(BIL3=BIL)
STDDT<- UNIV$STD_DT


PDEF <-SMA %>%select(STD_DT,TIP,DBC,IEF,TLT,LQD,AGG,BIL,UUP,BIL3,BIL2)
POFF <-SMA %>%select(STD_DT,SPY,QQQ,IWM,VGK,EWJ,VWO,VNQ,GLD,DBC,TLT,HYG,LQD)

TEMP <- SMA%>%trans_rt("month")%>%dt_trans()
l <- STDDT%>%length
#안전자산
RES3 <- lapply(c(1:l),function(t){

  
  SMA12 <-    (PDEF%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)/
               PDEF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%select(-STD_DT) %>%sapply(mean,1))%>%sort


   tmp<- SMA12%>%as.numeric-1
   names(tmp) <- SMA12%>%names
   INDEX<- tmp%>%utils::tail(n=3)%>%names
  
   res <- TEMP%>%dplyr::select(STD_DT,INDEX[1],INDEX[2],INDEX[3])%>%as.data.frame%>%dplyr::filter(STD_DT==UNIV$STD_DT_L[t])%>%melt(id.vars="STD_DT")

 
  
   DEF <- (res$value%>%sum)/3
   DEF<-  data.frame(STD_DT=UNIV$STD_DT_L[t],DEF)

return(DEF)

})

RES3 <- do.call(rbind,RES3)
#RES4<-RES3%>%dcast(STD_DT~variable)

#공격자산
RES5<- lapply(c(1:l),function(t){

  SMA12 <- (POFF%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)/
              POFF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%select(-STD_DT) %>%sapply(mean,1))%>%sort

  tmp<- SMA12%>%as.numeric
  names(tmp) <- SMA12%>%names
  
  # INDEX<- tmp%>%utils::tail(n=1)%>%names
  # res <- TEMP%>%select(STD_DT,INDEX[1])%>%as.data.frame%>%dplyr::filter(STD_DT==UNIV$STD_DT_L[t])%>%melt(id.vars="STD_DT")
  # OFF <-  res$value
  # OFF <-  data.frame(STD_DT=STDDT[t],OFF) 

  INDEX<- tmp%>%utils::tail(n=6)%>%names
  res <-  TEMP%>%select(STD_DT,INDEX[1],INDEX[2],INDEX[3],INDEX[4],INDEX[5],INDEX[6])%>%as.data.frame%>%dplyr::filter(STD_DT==UNIV$STD_DT_L[t])%>%melt(id.vars="STD_DT")
  OFF <-  res$value%>%mean
  OFF <-  data.frame(STD_DT=UNIV$STD_DT_L[t],OFF)
return(OFF)

})

#SSSS
INDEXDEF<- lapply(c(1:l),function(t){
  
  
  SMA12 <-   (PDEF%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)/
              PDEF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%select(-STD_DT) %>%sapply(mean,1))%>%sort
  
  tmp<- SMA12%>%as.numeric
  names(tmp) <- SMA12%>%names
  INDEX<- tmp%>%utils::tail(n=3)%>%names
  INDEX <-data.frame(STD_DT=STDDT[t],INDEX)
  
  return(INDEX)
  
})
INDEXDEF <- do.call(rbind,INDEXDEF)


INDEXOFF<- lapply(c(1:l),function(t){
  
  SMA12 <- (POFF%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)/
            POFF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%select(-STD_DT) %>%sapply(mean,1))%>%sort

  
  tmp<- SMA12%>%as.numeric
  names(tmp) <- SMA12%>%names
  
  INDEX<- tmp%>%utils::tail(n=3)%>%names
  INDEX <-data.frame(STD_DT=STDDT[t],INDEX)

  
  # INDEX<- tmp%>%utils::tail(n=6)%>%names
  # res <- TEMP%>%select(STD_DT,INDEX[1],INDEX[2],INDEX[3],INDEX[4],INDEX[5],INDEX[6])%>%as.data.frame%>%dplyr::filter(STD_DT==UNIV$STD_DT_L[t])%>%melt(id.vars="STD_DT")


  
  return(INDEX)
  
})

INDEXOFF <- do.call(rbind,INDEXOFF)
INDEXDEF<- UNIV_FINAL%>%filter(state==0)%>%select(STD_DT_L)%>%left_join(INDEXDEF,by=c("STD_DT_L"="STD_DT"))
INDEXOFF<- UNIV_FINAL%>%filter(state==1)%>%select(STD_DT_L)%>%left_join(INDEXOFF,by=c("STD_DT_L"="STD_DT"))
INDEX<- rbind(INDEXDEF,INDEXOFF)%>%arrange(STD_DT_L)

RES5 <- do.call(rbind,RES5)
#RES5 <- RES5%>%dcast(STD_DT~variable)
UNIV_FINAL%>%filter(state==0)%>%select(STD_DT_L,DEF)%>%dplyr::rename(RT="DEF")
UNIV_FINAL<- UNIV%>%left_join(RES3,by=c("STD_DT_L"="STD_DT"))%>%left_join(RES5,by=c("STD_DT_L"="STD_DT"))

BAA_RT<- data.frame(rbind(UNIV_FINAL%>%filter(state==0)%>%select(STD_DT_L,DEF)%>%dplyr::rename(RT="DEF"),
UNIV_FINAL%>%filter(state==1)%>%select(STD_DT_L,OFF)%>%dplyr::rename(RT="OFF")))%>%arrange(STD_DT_L)%>%na.omit


INDEX %>% filter(STD_DT=="2023-04-30")



RT_BAA<- cuml(retm%>%select(STD_DT,BM) %>% left_join(BAA_RT,by=c("STD_DT"="STD_DT_L"))%>%na.omit)
RET_BAA <- retm%>%select(STD_DT,BM) %>% left_join(BAA_RT,by=c("STD_DT"="STD_DT_L"))
cuml(retm%>%select(STD_DT,BM) %>% left_join(BAA_RT,by=c("STD_DT"="STD_DT_L"))%>%na.omit) %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line(size=1)+ 
  ggtitle("누적수익률") +
  theme(legend.text = element_text(size=15))

PA_BAA<- RT_BM %>% left_join(BAA_RT,by=c("STD_DT"="STD_DT_L"))%>%na.omit %>%PA   

write.xlsx(RT_BAA ,"c:/work/BAA.xlsx", sheetName="RT_BAA",append=F)
write.xlsx(RET_BAA ,"c:/work/BAA.xlsx", sheetName="RET_BAA",append=T)
write.xlsx(INDEX ,"c:/work/BAA.xlsx", sheetName="INDEX",append=T)
write.xlsx(PA_BAA ,"c:/work/BAA.xlsx", sheetName="PA_BAA",append=T)

