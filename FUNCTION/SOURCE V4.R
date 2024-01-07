
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr","openxlsx","DT",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2","depmixS4",
        "shiny","shinythemes", "shinydashboard","ggplot2","quantmod","dynlm","vars","bvartools")
 
ipak(pkg)

      #oecd경기선행지수수
      CLI  <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T)
      CLI  <-  CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1)) 

     
     #  RAWDATA5<- readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FEPS")
     #  RAWDATA6<- readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="TPER")
     #  colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
     #  colnames(RAWDATA6) <- paste0("TPER_",colnames(RAWDATA6))
     #  TMP <- rbind(
     # # RAWDATA%>%na.omit
     #   readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="EQ")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="AL")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FX")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FI")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,RAWDATA5%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,RAWDATA6%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="RATE")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="RISK")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable!="HYSP")
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="ETF") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FORECAST")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
     #  ,readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet2")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable=="EM")
     #  ,RAWDATA%>%filter(variable!="EM")
     #  ,CLI%>%na.omit()
     #  )%>%as.data.table%>%mutate(variable==as.character(variable))
     #  write.csv(TMP ,"c:/work/RAWDATA2024-01.csv")
      
      
      RAWDATA <- read.csv("c:/work/RAWDATA2024-01.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
      RAWDATA <- RAWDATA %>% filter(STD_DT<"2023-01-01")
      RAWDATA%>%filter(variable=="MSUS")%>%arrange(STD_DT)
      RAWDATA2 <-     RAWDATA%>%filter(variable=="USBOND"|variable=="SP500"|variable=="DM"|variable=="EM"|variable=="US3M"|variable=="CRB"|variable=="USIG"|variable=="USMID"|variable=="USLONG"|variable=="WRTIP"|variable=="USTIP"|
                                         variable=="WRBOND"|variable=="TIP"|variable=="WORLD"|variable=="NASDAQ"|variable=="BIL"|variable=="RUSS"|variable=="MSEU"|variable=="EWJ"|variable=="VNQ"|variable=="GOLD"|variable=="USHY"|
                                         variable=="VNQ"|variable=="CRB"|variable=="WREPRA"|variable=="GOLD"|variable=="USREIT"|variable=="USREIT"|variable=="USREIT"|variable=="USREIT"|variable=="USREIT"|variable=="USREIT"|
                                         variable=="WRINFRA"| variable=="USGOVT"|variable=="HFRI"|variable=="WRGOVT"|variable=="WRIG"|variable=="USDKRW"|variable=="GSCI"|variable=="KRBOND"|variable=="MSKR"|variable=="KOSPI"|
                                       variable=="IEF"| variable=="USMID")%>%
                                          dcast(STD_DT~variable)%>%mutate(AL=(WREPRA+WRINFRA+HFRI)/3)%>%mutate(KRBONDH=KRBOND*USDKRW)%>%
        mutate(WORLD2 =WORLD/USDKRW)%>%
        mutate(WRBOND2=WRBOND/USDKRW)%>%
        mutate(AL2    =AL/USDKRW)%>%
        mutate(GSCI2  =GSCI/USDKRW)%>%
        mutate(MSKR2  =MSKR*USDKRW)
     
      retm <- RAWDATA2%>%filter(STD_DT>"2000-01-01") %>%trans_rt("month")%>%dt_trans()%>%mutate(STD_DT_L1=lag(STD_DT,1))%>%mutate(BM=0.6*WORLD+0.4*WRBOND)%>%mutate(BM=0.6*WORLD+0.4*WRBOND)%>%mutate(MY=3*(0.4*NASDAQ+0.6*USGOVT))%>%
              mutate(INF=WRGOVT-WRTIP)%>%mutate(CREDIT=WRIG-WRGOVT)%>%mutate(RINTEREST=WRTIP)%>%mutate(GROWTH=DM)%>%mutate(FX=USDKRW)%>%mutate(emerging=DM-EM)%>%mutate(STD_DT_L1=lag(STD_DT,1))
      
      DD