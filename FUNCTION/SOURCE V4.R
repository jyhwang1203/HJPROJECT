
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr","openxlsx","DT",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2","depmixS4",
        "shiny","shinythemes", "shinydashboard","ggplot2","quantmod")
 
ipak(pkg)

      #oecd경기선행지수수
CLI  <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T)
CLI  <-  CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1)) 
     
      CLI  <-  dcast(CLI,STD_DT ~LOCATION,  value.var = "Value")%>%melt(id.vars="STD_DT")%>%na.omit


      ESGETF             <-  readxl::read_excel("c:/work/ESG_update.xlsx",sheet="Sheet2")
      ESGINDEX           <-  readxl::read_excel("c:/work/ESG_update.xlsx",sheet="Sheet1")
      ESGETF             <-  ESGETF%>%handling
      ESGINDEX           <-  ESGINDEX%>%handling
      MPWEIGHT           <-  readxl::read_excel("c:/work/MPWEIGHT.xlsx",sheet="Sheet2")
      readxl::read_excel("c:/work/macro.xlsx",sheet="quarter")%>%handling
     
      RAWDATAFULL <- read.csv("c:/work/RAWDATAFULLV2.csv")%>%dplyr::select(-X)%>% mutate(STD_DT=as.Date(STD_DT))
      RAWDATAFULL <- RAWDATAFULL%>%filter(variable!="USGOVT"&variable!="WRBOND"&variable!="IGSPBAA"&variable!="IGSPAAA"&variable!="US30Y"&variable!="SP500"&variable!="NASDAQ")%>%na.omit
      
      write.csv(RAWDATAFULL ,"c:/work/RAWDATAFULLV3.csv")
      RAWDATAFULL <- read.csv("c:/work/RAWDATAFULLV3.csv")%>%dplyr::select(-X)%>% mutate(STD_DT=as.Date(STD_DT))
      readxl::read_excel("c:/work/macro.xlsx",sheet="quarter")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable=="USGDPY")
      RAWDATA5<- readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FEPS")
      RAWDATA6<- readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="TPER")
      colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      colnames(RAWDATA6) <- paste0("TPER_",colnames(RAWDATA6))

      TMP <- rbind(
     # RAWDATA%>%na.omit
       readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="EQ")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="AL")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FX")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FI")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,RAWDATA5%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,RAWDATA6%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="RATE")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="RISK")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable!="HYSP")
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="ETF") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="FORECAST")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,RAWDATAFULL%>%na.omit
      ,readxl::read_excel("c:/work/macro.xlsx",sheet="weekly")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/macro.xlsx",sheet="monthly")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/macro.xlsx",sheet="quarter")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="monthly")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit()
      ,readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet1")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit()
      ,CLI%>%na.omit()
      )%>%as.data.table%>%mutate(variable==as.character(variable))
      write.csv(TMP ,"c:/work/RAWDATA2023-09.csv")

      readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet1")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit()%>%filter(variable=="ISMPMIS")
      # rbind(RAWDATAFULL%>%filter(variable=="HYSP"),
      # readxl::read_excel("c:/work/RAWDATA_ver5.xlsx",sheet="RATE")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable=="HYSP"))%>%dcast(STD_DT~variable)
      # RAWDATA%>%filter(variable=="HYSP")%>%dcast(STD_DT~variable)
      RAWDATA <-  read.csv("c:/work/RAWDATA2023-09.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
      RAWDATA<- rbind(RAWDATA,JEPI$JEPI.Adjusted %>%dt_trans()%>%melt(id.vars="STD_DT"))
      
      #RAWDATA <-  rbind(RAWDATA%>%filter(variable!="EMSP"),RAWDATA%>%filter(variable=="EMSP")%>%mutate(value=value/100))
      # RAWDATA%>%filter(variable=="USA")
      #  write.csv(TMP %>% filter(variable!="USINFRA"&variable!="EUINFRA"&variable!="WRREIT"&variable!="IGSPAA"&variable!="KSPAA"&variable!="KSPAAA"&variable!="EMBI"&variable!="HYSP"&
      #                                 variable!="bei10y"&variable!="USREIT"&variable!="EXEURO")%>%na.omit ,"c:/work/RAWDATA2023-08V2.csv")
      # 
      #  RAWDATA <-  read.csv("c:/work/RAWDATA2023-08V2.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%as.data.table%>%
      #  mutate(STD_DT=as.Date(STD_DT))
      # 
      #  TMP <- rbind(RAWDATA
      #              ,readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet1")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")
      #              ,readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet2")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")
      #              )
      # 
      # write.csv(TMP ,"c:/work/RAWDATA2023-08.csv")
      
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
      
      
      