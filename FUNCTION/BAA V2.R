
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("data.table","writexl","plyr","quantmod","lubridate","gdata","ggplot2","xlsx","stocks","tseries","dplyr")
ipak(pkg)

# RAWDATA22           <-        read.csv("c:/work/RAWDATA2.csv",stringsAsFactors = FALSE,header=T) %>% dplyr::select(-X) %>%as.data.frame()%>%
# mutate(STD_DT=STD_DT%>%as.Date())
# TEMP      <-  readxl::read_excel("c:/work/universe.xlsx",sheet="ETF")
# 
# #TEMP      <-  readxl::read_excel("c:/work/PAA.xlsx",sheet="Sheet2")
# STD_DT             <-  TEMP[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
# TEMP            <-  cbind(STD_DT,TEMP[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
# TEMP2               <-  apply(TEMP%>%dplyr::select(-STD_DT), 2, as.numeric)
# # 
# 
# #PAA           <-  data.frame(STD_DT,TEMP2)%>%mutate(STD_DT=as.Date(STD_DT))%>%fillf
# PAA           <-  RAWDATA2%>%dplyr::select(STD_DT,TIP,DBC,UUP,LQD,AGG,IEF,BIL,SPY,QQQ,IWM,VGK,EWJ,VNQ,VWO,GLD,DBC,TLT,HYG,LQD)

#CANARY <- PAA%>%dplyr::select(STD_DT,AGG,SPY,VWO,VEA)%>%fillf%>%trans_rt("month")%>%dt_trans()
CANARY <- retm
DATE <- CANARY

# START <- "2007-01-01"%>%as.Date
# END  <- "2023-06-30"%>%as.Date

#DATE <- DATE %>%  filter(STD_DT>=START&STD_DT<=END)
DATE <- DATE$STD_DT

ll <- DATE%>%length

RES <- lapply(c(13:ll),function(T){

  STD_DT <-DATE[T]
  p0 <- RAWDATA2%>%dplyr::select(STD_DT,USBOND,SP500,DM,EM)%>%filter(STD_DT==DATE[T])%>%.[-1]   
  p1 <- RAWDATA2%>%dplyr::select(STD_DT,USBOND,SP500,DM,EM)%>%filter(STD_DT==DATE[T-1]) %>%.[-1]   
  p3 <- RAWDATA2%>%dplyr::select(STD_DT,USBOND,SP500,DM,EM)%>%filter(STD_DT==DATE[T-3]) %>%.[-1]   
  p6 <- RAWDATA2%>%dplyr::select(STD_DT,USBOND,SP500,DM,EM)%>%filter(STD_DT==DATE[T-6]) %>%.[-1]   
  p12<- RAWDATA2%>%dplyr::select(STD_DT,USBOND,SP500,DM,EM)%>%filter(STD_DT==DATE[T-12])%>%.[-1]   
  
   mom<- 12*(p0/p1-1)+4*(p0/p3-1)+2*(p0/p6-1)+(p0/p12-1)
   #mom<- 6*(p0/p1-1)+3*(p0/p3-1)+2*(p0/p6-1)+(p0/p12-1)
  res<- data.frame(STD_DT,mom)
return(res)
})

RES2 <- do.call(rbind,RES)
RES2[is.na(RES2)] <- 0
 
  
UNIV_BAA0 <- data.frame(rbind(RES2%>%filter(USBOND<0|SP500<0|DM<0|EM<0)%>%mutate(state=0),
                         RES2%>%filter(USBOND>=0&SP500>=0&DM>=0&EM>=0)%>%mutate(state=1)))%>%arrange(STD_DT)

  
 #수익률이 아니라 가격임
 #sma filter
  SMA   <- RAWDATA2%>%mutate(BIL2=US3M)%>%mutate(BIL3=US3M)
  STDDT <- UNIV_BAA0$STD_DT

  PDEF <- SMA %>%dplyr::select(STD_DT,TIP,CRB,USIG,USBOND,USMID,USLONG,US3M)%>%as.data.frame%>%fillf
  #PDEF <-SMA %>%dplyr::select(STD_DT,IEF,UUP,BIL,TIP,BIL2)%>%as.data.frame%>%fillf
  #POFF <- SMA %>%dplyr::select(STD_DT,SPY,QQQ,IWM,VGK,EWJ,VNQ,VWO,GLD,DBC,TLT,HYG,LQD)%>%as.data.frame%>%fillf
  POFF <- SMA %>%dplyr::select(STD_DT,SP500,NASDAQ,RUSS,MSEU,EM,EWJ,VNQ,GOLD,CRB,USLONG,USHY,USIG)%>%as.data.frame%>%fillf
  #POFF2 <-SMA %>%dplyr::select(STD_DT,QQQ,VWO,VEA,AGG)
  TEMP <- retm
  l    <- STDDT%>%length
 #안전자산
  


  DEF <-lapply(c(1:l),function(t){
 
    SMA12 <-  (PDEF %>% filter(STD_DT==STDDT[t])%>%dplyr::select(-STD_DT)/
                PDEF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%dplyr::select(-STD_DT) %>%sapply(mean,1))
    data.frame(STD_DT=UNIV_BAA0$STD_DT[t],SMA12)%>%reshape2::melt(id.vars="STD_DT")
   })
  DEF <- do.call(rbind,DEF)
  DEF[is.na(DEF)]<-0
  OFF   <- lapply(c(1:l),function(t){
    
  SMA12 <-     (POFF%>% filter(STD_DT==STDDT[t])%>%dplyr::select(-STD_DT)/
                POFF %>% filter(STD_DT<=STDDT[t]&STD_DT>=(STDDT[t]%m-%years(1)))%>%dplyr::select(-STD_DT) %>%sapply(mean,1))

  data.frame(STD_DT=UNIV_BAA0$STD_DT[t],SMA12)%>%melt(id.vars="STD_DT")
  #%>%mutate(rank=order(value,decreasing = T)) 
  })
  OFF<- do.call(rbind,OFF)
  OFF[is.na(OFF)]<-0

  UNIV_BAA <-   rbind(DEF%>%left_join(UNIV_BAA0%>%dplyr::select(STD_DT,state),by="STD_DT")%>%filter(state==0),
                      OFF%>%left_join(UNIV_BAA0%>%dplyr::select(STD_DT,state),by="STD_DT")%>%filter(state==1))%>%arrange(STD_DT)
  
  colnames(UNIV_BAA) <- c("STD_DT","variable","sma","state")
  # date  <- "2008-01-01"
  # date2 <- "2023-06-30"
  # UNIV_BAA %>%filter(STD_DT>"2023-05-03")
  # ##input
  # STDDT <- "2007-01-01"%>%as.Date()


  BAA <- function(date,date2,ss,n,nn,univ){
    
    
    
    STDDT <- date%>%as.Date()
    STDDT2 <- date2%>%as.Date()
    # STDDT <- "2012-06-30"%>%as.Date()
    # STDDT2 <- "2022-06-30"%>%as.Date()
    # univ<- UNIV_BAA
    # n <-3
    RET_OFF <- univ%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>% 
      dplyr::filter(rank <= n )
    
    
    tmp     <- RET_OFF %>% filter(sma <1) %>%left_join(retm%>%dplyr::select(STD_DT,BIL),by="STD_DT")%>%dplyr::select(STD_DT,BIL)%>%reshape2::melt(id.vars="STD_DT")%>%group_by(STD_DT)%>%
      dplyr::mutate(rank=order(value,decreasing=T))%>%dplyr::arrange(STD_DT)%>%dplyr::select(STD_DT,variable)
    RET_OFF <-rbind(tmp,RET_OFF %>% filter(sma>=1)%>%dplyr::arrange(STD_DT)%>%dplyr::select(STD_DT,variable))%>%
      left_join(retm[,-1]%>%reshape2::melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))%>%mutate(wei=1/n)
    
  
    

    
    {if(ss=="G12"){
        RET_OFF <- univ%>%filter(state==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%
        dplyr::filter(rank <= n )%>%left_join(TEMP[,-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
    }
    # else  if(state2=="G12T4"){
    #     RET_OFF <- UNIV_BAA%>%filter(STD_DT>STDDT)%>%filter(state2==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%
    #     dplyr::filter(rank <= 4 )%>%left_join(TEMP[-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
    # }
    # else  if(state2=="G12T6"){
    #     RET_OFF <- UNIV_BAA%>%filter(STD_DT>STDDT)%>%filter(state2==1)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%
    #       dplyr::filter(rank <= 4 )%>%left_join(TEMP[-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
    #   }
    else if(ss=="G4"){
        RET_OFF <- univ%>%filter(state==1)%>%filter(variable=="SPY"|variable=="QQQ"|variable=="VGK"|variable=="VWO")%>%group_by(STD_DT)%>%
                   dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%filter(rank<=1)%>%
                   left_join(TEMP[,-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
         
    }}
    

  RET_DEF <- univ%>%filter(state==0)%>%group_by(STD_DT)%>%dplyr::arrange(sma,decreasing=F,.by_group=TRUE)%>%group_by(STD_DT)%>%dplyr::mutate(rank=order(sma,decreasing=T)) %>%filter(rank<=nn)%>%
             left_join(TEMP[,-1]%>%melt(id.vars="STD_DT_L1"),by=c("STD_DT"="STD_DT_L1","variable"))
  UNIV_BAA2 <<- rbind(RET_DEF,RET_OFF)%>%dplyr::arrange(STD_DT)%>%mutate(STD_DT=(STD_DT%m+%months(1)))%>%
    left_join(retm%>%dplyr::select(STD_DT,WORLD),by="STD_DT")%>%mutate(WORLD=WORLD%>%round(2))
  colnames(UNIV_BAA2)[6]<<-"Return"
  RT <-  rbind(RET_OFF%>%aggregate(value~STD_DT,mean),RET_DEF%>%aggregate(value~STD_DT,mean))%>%dplyr::arrange(STD_DT)%>%mutate(STD_DT=lead(STD_DT))%>%
        left_join(retm%>%dplyr::select(STD_DT,BM,WORLD),by="STD_DT")%>%na.omit%>%filter(STD_DT>=STDDT&STD_DT<STDDT2)
  colnames(RT)[2]<-"BAA"
  RT[,-1]<- RT[,-1] %>%round(3)
return(RT)
  }
 #  res  <- BAA("2008-01-01","2023-06-30","G12",n=12,nn=2,"UUP")%>%cuml

  # BAA("2008-01-01","2023-06-30","G12",n=12,nn=3,UNIV_BAA)%>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
  #   geom_line(size=1)+
  #   ggtitle("누적수익률") +
  #   theme(legend.text = element_text(size=15))
  # BAA("2012-11-30","2023-01-01","G12",n=,nn=3,UNIV_BAA)%>%PA
# RES1  <-   UNIV%>%filter(STD_DT>"2022-12-31")
# write.xlsx(RT_BAA ,"c:/work/BAA.xlsx", sheetName="RT_BAA",append=F)  
#   UNIV%>%dplyr::select(-state2)%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#     geom_line(size=1)+
#     ggtitle("누적수익률") +
#     theme(legend.text = element_text(size=15))
#   
#   UNIV%>%filter(state2==0)
#   UNIV%>%filter(state2==1)
#   
#   data.frame(rep(1,12),1,1)
#   BAA("2012-06-30","2022-06-30","G12",n=12,nn=2,"UUP")
#     UNIV%>%dplyr::select(-state2)%>%dplyr::select(STD_DT,WORLD,MY) %>%cuml%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +
#   geom_line(size=1)+
#   ggtitle("누적수익률") +
#   theme(legend.text = element_text(size=15))
#   data.frame(rep(1,12),1,1)
#   RT%>%left_join(UNIV,by="STD_DT")
#   data.frame(RET_DEF$variable%>%unique,
#   RET_OFF%>%filter(variable=="SPY"|variable=="QQQ"|variable=="VGK"|variable=="VWO")%>%.$variable%>%unique,
#   RET_OFF$variable%>%unique)
# RT%>% dplyr::select(WORLD) %>%filter(WORLD>0)%>%nrow/RT %>% dplyr::select(WORLD) %>%nrow  
# RT%>% dplyr::select(BM) %>%filter(BM>0)%>%nrow/RT %>% dplyr::select(BM) %>%nrow  

 # write.xlsx(res ,"c:/work/BAA.xlsx", sheetName="RT_BAA",append=F)
 # write.xlsx(RET_BAA ,"c:/work/BAA.xlsx", sheetName="RET_BAA",append=T)
 # write.xlsx(INDEX ,"c:/work/BAA.xlsx", sheetName="INDEX",append=T)
 # write.xlsx(PA_BAA ,"c:/work/BAA.xlsx", sheetName="PA_BAA",append=T)
   