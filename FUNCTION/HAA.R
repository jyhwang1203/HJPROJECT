
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("data.table","writexl","plyr","quantmod","lubridate","gdata","ggplot2","xlsx","stocks","reshape2","tseries","dplyr")
ipak(pkg)

# 
DATE <- retm
#slippage cost 0.2~0.5
DATE <- DATE$STD_DT

ll <- DATE%>%length

mom_13612u <- lapply(c(13:ll),function(T){
  
  STD_DT <-DATE[T]
  p0 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,VNQ,WRTIP,SP500,RUSS,WREPRA,CRB,USLONG,USMID,BIL,USREIT,BIL)%>%filter(STD_DT==DATE[T])%>%.[-1]   
  p1 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,VNQ,WRTIP,SP500,RUSS,WREPRA,CRB,USLONG,USMID,BIL,USREIT,BIL)%>%filter(STD_DT==DATE[T-1]) %>%.[-1]   
  p3 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,VNQ,WRTIP,SP500,RUSS,WREPRA,CRB,USLONG,USMID,BIL,USREIT,BIL)%>%filter(STD_DT==DATE[T-3]) %>%.[-1]   
  p6 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,VNQ,WRTIP,SP500,RUSS,WREPRA,CRB,USLONG,USMID,BIL,USREIT,BIL)%>%filter(STD_DT==DATE[T-6]) %>%.[-1]   
  p12<- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,VNQ,WRTIP,SP500,RUSS,WREPRA,CRB,USLONG,USMID,BIL,USREIT,BIL)%>%filter(STD_DT==DATE[T-12])%>%.[-1]   
  
  
  # p0 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,SP500,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP,SP500,RUSS,DM,EM,WREPRA,CRB,USLONG,USMID,BIL,USREIT)%>%filter(STD_DT==DATE[T])%>%.[-1]   
  # p1 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,SP500,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP,SP500,RUSS,DM,EM,WREPRA,CRB,USLONG,USMID,BIL,USREIT)%>%filter(STD_DT==DATE[T-1]) %>%.[-1]   
  # p3 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,SP500,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP,SP500,RUSS,DM,EM,WREPRA,CRB,USLONG,USMID,BIL,USREIT)%>%filter(STD_DT==DATE[T-3]) %>%.[-1]   
  # p6 <- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,SP500,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP,SP500,RUSS,DM,EM,WREPRA,CRB,USLONG,USMID,BIL,USREIT)%>%filter(STD_DT==DATE[T-6]) %>%.[-1]   
  # p12<- RAWDATA2 %>%dplyr::select(STD_DT,USBOND,SP500,DM,EM,SP500,QQQ,IWM,VWO,VEA,VNQ,DBC,TLT,IEF,BIL,WRTIP,SP500,RUSS,DM,EM,WREPRA,CRB,USLONG,USMID,BIL,USREIT)%>%filter(STD_DT==DATE[T-12])%>%.[-1]   
  mom<- ((p0/p1-1)+(p0/p3-1)+(p0/p6-1)+(p0/p12-1))/4
  #mom<- 6*(p0/p1-1)+3*(p0/p3-1)+2*(p0/p6-1)+(p0/p12-1)
  res<- data.frame(STD_DT,mom)
  return(res)
})


mom_13612u <- do.call(rbind,mom_13612u)%>%mutate(cash=100*1)%>%mutate(cash2=100*1)%>%mutate(cash3=100)%>%mutate(BIL2=BIL)%>%mutate(BIL3=BIL)
mom_13612u[is.na(mom_13612u)] <- 0


UNIV_HAA0 <- data.frame(rbind(mom_13612u%>%filter(WRTIP<=0)%>%mutate(state=0),
                         mom_13612u%>%filter(WRTIP>0)%>%mutate(state=1)))%>%arrange(STD_DT)
                   

STDDT <- UNIV_HAA0$STD_DT

UNIV_HAA0 %>% filter(STD_DT>"2023-01-01")

PDEF <- mom_13612u %>%dplyr::select(STD_DT,BIL,BIL)%>%as.data.frame%>%fillf
#PDEF <- mom_13612u %>%dplyr::select(STD_DT,BIL,IEF)%>%as.data.frame%>%fillf
#POEF <- mom_13612u %>%dplyr::select(STD_DT,SPY,IWM,VWO,VEA,VNQ,DBC,TLT,IEF)%>%as.data.frame%>%fillf
POEF <- mom_13612u %>%dplyr::select(STD_DT,SP500,RUSS,DM,EM,VNQ,CRB,USLONG,USMID)%>%as.data.frame%>%fillf
TEMP <- retm
l    <- STDDT%>%length
#안전자산



DEF <-lapply(c(1:l),function(t){
  MOM <- mom_13612u %>%dplyr::select(STD_DT,BIL,USMID)%>%as.data.frame%>% filter(STD_DT==STDDT[t])%>%dplyr::select(-STD_DT)
  data.frame(STD_DT=UNIV_HAA0$STD_DT[t],MOM )%>%reshape2::melt(id.vars="STD_DT")
  })
DEF <- do.call(rbind,DEF)
DEF[is.na(DEF)]<-0
OFF   <- lapply(c(1:l),function(t){
  #  MOM <- mom_13612u %>%dplyr::select(STD_DT,SPY,VWO,QQQ,VEA,VNQ,DBC,TLT,IEF)%>%as.data.frame%>% filter(STD_DT==STDDT[t])%>%dplyr::select(-STD_DT)
    MOM <-  mom_13612u %>%dplyr::select(STD_DT,SP500,RUSS,DM,EM,VNQ,CRB,USLONG,USMID)%>%as.data.frame%>% filter(STD_DT==STDDT[t])%>%dplyr::select(-STD_DT)
    data.frame(STD_DT=UNIV_HAA0$STD_DT[t],MOM)%>%melt(id.vars="STD_DT")
  #%>%mutate(rank=order(value,decreasing = T)) 
})
OFF<- do.call(rbind,OFF)
OFF[is.na(OFF)]<-0


UNIV_HAA  <-   rbind(DEF%>%left_join(UNIV_HAA0%>%dplyr::select(STD_DT,state),by="STD_DT")%>%filter(state==0),
                     OFF%>%left_join(UNIV_HAA0%>%dplyr::select(STD_DT,state),by="STD_DT")%>%filter(state==1))%>%arrange(STD_DT)

colnames(UNIV_HAA) <- c("STD_DT","variable","sma","state")
# UNIV_HAA%>% filter(STD_DT>"2023-01-01")
#UNIV_HAA %>%filter(STD_DT>"2023-05-16")
##UNIV_HAA
# STDDT <- "2007-01-01"%>%as.Date()


HAA <- function(date,date2,n,nn,univ){


  
  STDDT <- date%>%as.Date()
  STDDT2 <- date2%>%as.Date()
  # STDDT <- "2012-06-30"%>%as.Date()
  # STDDT2 <- "2022-06-30"%>%as.Date()
  # # 
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% filter(STD_DT>"2012-01-01")
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% filter(STD_DT>"2001-01-01")
  # UNIV_BAA%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.byhttp://127.0.0.1:26497/graphics/plot_zoom_png?width=1344&height=792_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%

    RET_OFF <- univ%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% 
               dplyr::filter(rank <= n )
    
    
    tmp     <- RET_OFF %>% filter(sma <0) %>%left_join(mom_13612u%>%dplyr::select(STD_DT,USMID,BIL),by="STD_DT")%>%dplyr::select(STD_DT,USMID,BIL)%>%reshape2::melt(id.vars="STD_DT")%>%group_by(STD_DT)%>%
            dplyr::mutate(rank=order(value,decreasing=T))%>%dplyr::arrange(STD_DT)%>%filter(rank==1)
    
    RET_OFF <- rbind(RET_OFF%>%dplyr::select(STD_DT,sma) %>% filter(sma <0) %>%left_join(tmp%>%dplyr::select(STD_DT,variable),by="STD_DT")%>%dplyr::arrange(STD_DT),
            RET_OFF %>% filter(sma>=0)%>%dplyr::arrange(STD_DT)%>%dplyr::select(STD_DT,sma,variable))%>%dplyr::arrange(STD_DT)%>%
            left_join(retm[,-1]%>%reshape2::melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))%>%mutate(wei=1/4)
  
    RET_DEF <- univ%>%filter(state==0)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%filter(rank==1)%>%
               left_join(retm[,-1]%>%reshape2::melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))%>%dplyr::select(STD_DT,sma,variable,value)%>%mutate(wei=1)
   
    UNIV_HAA2 <<- rbind(RET_DEF,RET_OFF)%>%dplyr::arrange(STD_DT)%>%
    left_join(retm%>%mutate(STD_DT_L=lag(STD_DT))%>%dplyr::select(STD_DT_L,WORLD), by=c("STD_DT"="STD_DT_L"))%>%mutate(WORLD=WORLD%>%round(2))
    colnames(UNIV_HAA2)[4]<<-"Return"

    RT <-  rbind(RET_OFF%>%aggregate(value~STD_DT,mean),RET_DEF%>%aggregate(value~STD_DT,mean))%>%dplyr::arrange(STD_DT)%>%mutate(STD_DT=lead(STD_DT))%>%
    left_join(retm%>%dplyr::select(STD_DT,BM,WORLD),by="STD_DT")%>%na.omit%>%filter(STD_DT>=STDDT&STD_DT<STDDT2)
    colnames(RT)[2]<-"HAA"
    RT[,-1]<- RT[,-1] %>%round(3)
  return(RT)
    
}
# HAA("2003-01-01","2023-12-01",n=4,nn=1,UNIV_HAA)
# HAA("2003-01-01","2023-08-01",n=4,nn=1,UNIV_HAA)
# HAA("2012-11-30","2012-11-30",n=4,nn=1,UNIV_HAA)%>%PA
# BAA("2012-11-30","2023-01-01","G12",n=6,nn=3,UNIV_BAA)%>%PA
# RT_BAA %>%trans_rt("year")
RES1   <-  HAA("2020-11-30","2023-01-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
  BAA("2020-11-30","2023-01-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%PA
RES5   <-  HAA("2020-11-30","2023-01-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
  BAA("2020-11-30","2023-01-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%cuml
# 
# RT_BAA <- BAA("2003-01-01","2023-07-01","G12",n=6,nn=4,UNIV_BAA)
# RT_HAA <- HAA("2003-01-01","2023-07-01",n=4,nn=1,UNIV_HAA)
# RES2 <- UNIV_HAA0%>%dplyr::select(STD_DT,state,WRTIP)%>%left_join(UNIV_BAA0%>%dplyr::select(STD_DT,DM,EM,SP500,USBOND,state),by="STD_DT")
# RES3 <- RT_BAA%>%left_join(RT_HAA%>%dplyr::select(STD_DT,HAA),by="STD_DT")%>%cuml()%>%right_join(RAWDATA2%>%dplyr::select(STD_DT,US10Y))
# HAA("2003-01-01","2023-08-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
# BAA("2003-01-01","2023--01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%filter(STD_DT>"2023-01-01")
# BAA("2003-01-01","2023-08-01","G12",n=6,nn=3,UNIV_BAA)
# 
# 
# 
# 
# RES6 <- HAA("2003-01-01","2023-07-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
#   BAA("2003-01-01","2023-07-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT") %>% Drawdowns
# 
# HAA("2007-12-31","2023-07-01",n=4,nn=1,UNIV_HAA)%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#    geom_line(size=1)+
#    ggtitle("누적수익률") +
#    theme(legend.text = element_text(size=15))
# 
# HAA("2002-12-30","2023-07-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
# BAA("2002-12-30","2023-07-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#   geom_line(size=1)+
#   ggtitle("누적수익률") +
#   theme(legend.text = element_text(size=15))
# 
# HAA("2002-11-30","2023-12-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
#   BAA("2002-11-30","2023-12-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%PA
# HAA("2012-12-31","2023-01-01","G12",n=4,nn=1,UNIV_HAA)%>%PA
# BAA("2008-01-01","2023-06-30","G12",n=12,nn=3,"UUP")%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#   geom_line(size=1)+
#   ggtitle("누적수익률") +
#   theme(legend.text = element_text(size=15))
# HAA("2012-12-31","2023-01-01","G12",n=4,nn=1,"UUPX")%>%PA
# HAA("2012-12-31","2023-01-01","G12",n=4,nn=1,"UUPX")%>%cuml
# HAA("2012-11-30","2022-11-30","G12",n=4,nn=1,"UUPX")%>%PA
# HAA("2013-01-01","2023-01-01","G12",n=4,nn=1,"UUPX")%>%PA
# RT_BAA<- BAA("2008-01-01","2023-06-30","G12",n=12,nn=4,"UUP")
# RT_HAA<- HAA("2007-12-31","2023-07-01","G12",n=4,nn=1,"UUPX")
# RT_FINAL <- RT_BAA%>%left_join(RT_HAA%>%dplyr::select(STD_DT,BAA),by="STD_DT")
# RT_FINAL %>% cuml
# HAA("2007-12-31","2023-07-01","G12",n=4,nn=1,"UUPX")%>% cuml
# 
# UNIV_HAA%>%filter(STD_DT>"2023-01-01")
# UNIV_BAA%>%filter(STD_DT>"2023-01-01")
# 
# 
# RES1 <- HAA("2003-01-01","2023-07-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
#   BAA("2003-01-01","2023-07-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%left_join(
#     RET_MACRO,by="STD_DT")
# RES2 <- HAA("2003-01-01","2023-07-01",n=4,nn=1,UNIV_HAA)%>%dplyr::select(STD_DT,HAA)%>%left_join(
#   BAA("2003-01-01","2023-07-01","G12",n=6,nn=3,UNIV_BAA),by="STD_DT")%>%left_join(
#     RET_MACRO,by="STD_DT")%>%na.omit()%>%cuml
# write.xlsx(RES1 ,"c:/work/HAA.xlsx", sheetName="UNIV_HAA",append=F)
# write.xlsx(RES2 ,"c:/work/HAA.xlsx", sheetName="UNIV_BAA",append=T)
# write.xlsx(RES3 ,"c:/work/HAA.xlsx", sheetName="RT",append=T)
# write.xlsx(RES4 ,"c:/work/HAA.xlsx", sheetName="cum",append=T)
# write.xlsx(RES5 ,"c:/work/HAA.xlsx", sheetName="PA_BAA",append=T)
# write.xlsx(RES6 ,"c:/work/HAA.xlsx", sheetName="PABAA",append=T)

