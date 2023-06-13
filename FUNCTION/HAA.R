
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("data.table","writexl","plyr","quantmod","lubridate","gdata","ggplot2","xlsx","stocks","tseries","dplyr")
ipak(pkg)
getSymbols('TIP', src='yahoo')

TIP <- TIP$TIP.Adjusted
colnames(TIP)<-"TIP"
PAA            <-  RAWDATA%>%select(STD_DT,DBC,UUP,LQD,AGG,IEF,BIL,SPY,QQQ,IWM,VEA,VGK,EWJ,VNQ,VWO,GLD,DBC,TLT,HYG,LQD,WRTIP)%>%left_join(TIP%>%dt_trans,by="STD_DT")
CANARY <- retm
DATE <- CANARY
#slippage cost 0.2~0.5
# START <- "2007-01-01"%>%as.Date
# END  <- "2023-06-30"%>%as.Date

#DATE <- DATE %>%  filter(STD_DT>=START&STD_DT<=END)
DATE <- DATE$STD_DT

ll <- DATE%>%length

RES <- lapply(c(13:ll),function(T){
  
  STD_DT <-DATE[T]
  p0 <- PAA %>%select(STD_DT,SPY,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP)%>%filter(STD_DT==DATE[T])%>%.[-1]   
  p1 <- PAA %>%select(STD_DT,SPY,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP)%>%filter(STD_DT==DATE[T-1]) %>%.[-1]   
  p3 <- PAA %>%select(STD_DT,SPY,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP)%>%filter(STD_DT==DATE[T-3]) %>%.[-1]   
  p6 <- PAA %>%select(STD_DT,SPY,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP)%>%filter(STD_DT==DATE[T-6]) %>%.[-1]   
  p12<- PAA %>%select(STD_DT,SPY,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP)%>%filter(STD_DT==DATE[T-12])%>%.[-1]   
  
  mom<- ((p0/p1-1)+(p0/p3-1)+(p0/p6-1)+(p0/p12-1))/4
  #mom<- 6*(p0/p1-1)+3*(p0/p3-1)+2*(p0/p6-1)+(p0/p12-1)
  res<- data.frame(STD_DT,mom)
  return(res)
})


RES2 <- do.call(rbind,RES)%>%mutate(cash=100*1)%>%mutate(cash2=100*1)%>%mutate(cash3=100)%>%mutate(BIL2=BIL)%>%mutate(BIL3=BIL)
RES2[is.na(RES2)] <- 0


UNIV <- data.frame(rbind(RES2%>%filter(WRTIP < 0)%>%mutate(state=0),
                         RES2%>%filter(WRTIP >=0)%>%mutate(state=1)))%>%arrange(STD_DT)

UNIV%>%filter(STD_DT>"2010-01-01")
STDDT <- UNIV$STD_DT

PDEF <- RES2 %>%select(STD_DT,BIL,IEF)%>%as.data.frame%>%fillf
POEF <- RES2 %>%select(STD_DT,SPY,QQQ,VWO,VEA,VNQ,DBC,TLT,IEF)%>%as.data.frame%>%fillf

TEMP <- SMA%>%trans_rt("month")%>%dt_trans()%>%mutate(STD_DT_L1=lag(STD_DT,1))
l    <- STDDT%>%length
#안전자산



DEF <-lapply(c(1:l),function(t){
  MOM <- RES2 %>%select(STD_DT,BIL,IEF)%>%as.data.frame%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)
  
  data.frame(STD_DT=UNIV$STD_DT[t],MOM )%>%reshape2::melt(id.vars="STD_DT")
})
DEF <- do.call(rbind,DEF)
DEF[is.na(DEF)]<-0
OFF   <- lapply(c(1:l),function(t){
  MOM <- RES2 %>%select(STD_DT,SPY,VWO,QQQ,VEA,VNQ,DBC,TLT,IEF)%>%as.data.frame%>% filter(STD_DT==STDDT[t])%>%select(-STD_DT)
  data.frame(STD_DT=UNIV$STD_DT[t],MOM)%>%melt(id.vars="STD_DT")
  #%>%mutate(rank=order(value,decreasing = T)) 
})
OFF<- do.call(rbind,OFF)
OFF[is.na(OFF)]<-0


UNIV_BAA  <-   rbind(DEF%>%left_join(UNIV%>%select(STD_DT,state),by="STD_DT")%>%filter(state==0),
                     OFF%>%left_join(UNIV%>%select(STD_DT,state),by="STD_DT")%>%filter(state==1))%>%arrange(STD_DT)

colnames(UNIV_BAA) <- c("STD_DT","variable","sma","state")


##input
STDDT <- "2007-01-01"%>%as.Date()


BAA <- function(date,date2,state,n,nn,univ){
  
  UNIV_BAA <- rbind(DEF%>%left_join(UNIV%>%select(STD_DT,state),by="STD_DT")%>%filter(state==0),
                    OFF%>%left_join(UNIV%>%select(STD_DT,state),by="STD_DT")%>%filter(state==1))%>%arrange(STD_DT)
  colnames(UNIV_BAA) <- c("STD_DT","variable","sma","state")

  
  STDDT <- date%>%as.Date()
  STDDT2 <- date2%>%as.Date()
  # STDDT <- "2012-06-30"%>%as.Date()
  # STDDT2 <- "2022-06-30"%>%as.Date()
  # # 
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% filter(STD_DT>"2012-01-01")
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% filter(STD_DT>"2001-01-01")
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%
  
  
    RET_OFF <- UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% 
               dplyr::filter(rank <= n )
    
    
    tmp     <- RET_OFF %>% filter(sma <0) %>%left_join(RES2%>%select(STD_DT,IEF,BIL),by="STD_DT")%>%select(STD_DT,IEF,BIL)%>%melt(id.vars="STD_DT")%>%group_by(STD_DT)%>%
            dplyr::mutate(rank=order(value,decreasing=T))%>%dplyr::arrange(STD_DT)%>%filter(rank==1)
    
    RET_OFF <- rbind(RET_OFF%>%select(STD_DT,sma) %>% filter(sma <0) %>%left_join(tmp%>%select(STD_DT,variable),by="STD_DT")%>%dplyr::arrange(STD_DT),
            RET_OFF %>% filter(sma>=0)%>%dplyr::arrange(STD_DT)%>%select(STD_DT,sma,variable))%>%dplyr::arrange(STD_DT)%>%
            left_join(TEMP[-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
  
    RET_DEF <- UNIV_BAA%>%filter(state==0)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%filter(rank<=nn)%>%
               left_join(TEMP[-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))%>%select(STD_DT,sma,variable,value)
    
    UNIV_BAA2 <<- rbind(RET_DEF,RET_OFF)%>%dplyr::arrange(STD_DT)%>%
    left_join(retm%>%mutate(STD_DT_L=lag(STD_DT))%>%select(STD_DT_L,WORLD), by=c("STD_DT"="STD_DT_L"))%>%mutate(WORLD=WORLD%>%round(2))
    colnames(UNIV_BAA2)[4]<<-"Return"
  
    RT <-  rbind(RET_OFF%>%aggregate(value~STD_DT,mean),RET_DEF%>%aggregate(value~STD_DT,mean))%>%dplyr::arrange(STD_DT)%>%mutate(STD_DT=lead(STD_DT))%>%
    left_join(retm%>%select(STD_DT,BM,WORLD),by="STD_DT")%>%na.omit%>%filter(STD_DT>=STDDT&STD_DT<STDDT2)
    colnames(RT)[2]<-"BAA"
    RT[,-1]<- RT[,-1] %>%round(3)
  return(RT)
}
2.10^(12/121)

#  res  <- BAA("2008-01-01","2023-06-30","G12",n=12,nn=2,"UUP")%>%cuml
BAA("2007-12-31","2023-07-01","G12",n=4,nn=1,"UUPX")%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
   geom_line(size=1)+
   ggtitle("누적수익률") +
   theme(legend.text = element_text(size=15))
BAA("2012-12-31","2023-01-01","G12",n=4,nn=1,"UUPX")%>%PA
BAA("2012-12-31","2023-01-01","G12",n=4,nn=1,"UUPX")%>%cuml
BAA("2012-11-30","2022-11-30","G12",n=4,nn=1,"UUPX")%>%PA
BAA("2013-01-01","2023-01-01","G12",n=4,nn=1,"UUPX")%>%PA
# RES1  <-   UNIV%>%filter(STD_DT>"2022-12-31")
# write.xlsx(RT_BAA ,"c:/work/BAA.xlsx", sheetName="RT_BAA",append=F)  
#   UNIV%>%select(-state)%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#     geom_line(size=1)+
#     ggtitle("누적수익률") +
#     theme(legend.text = element_text(size=15))
#   
#   UNIV%>%filter(state==0)
#   UNIV%>%filter(state==1)
#   
#   data.frame(rep(1,12),1,1)
#   BAA("2012-06-30","2022-06-30","G12",n=12,nn=2,"UUP")
#     UNIV%>%select(-state)%>%select(STD_DT,WORLD,MY) %>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#   geom_line(size=1)+
#   ggtitle("누적수익률") +
#   theme(legend.text = element_text(size=15))
#   data.frame(rep(1,12),1,1)
#   RT%>%left_join(UNIV,by="STD_DT")
#   data.frame(RET_DEF$variable%>%unique,
#   RET_OFF%>%filter(variable=="SPY"|variable=="QQQ"|variable=="VGK"|variable=="VWO")%>%.$variable%>%unique,
#   RET_OFF$variable%>%unique)
# RT%>% select(WORLD) %>%filter(WORLD>0)%>%nrow/RT %>% select(WORLD) %>%nrow  
# RT%>% select(BM) %>%filter(BM>0)%>%nrow/RT %>% select(BM) %>%nrow  

# write.xlsx(res ,"c:/work/BAA.xlsx", sheetName="RT_BAA",append=F)
# write.xlsx(RET_BAA ,"c:/work/BAA.xlsx", sheetName="RET_BAA",append=T)
# write.xlsx(INDEX ,"c:/work/BAA.xlsx", sheetName="INDEX",append=T)
# write.xlsx(PA_BAA ,"c:/work/BAA.xlsx", sheetName="PA_BAA",append=T)
