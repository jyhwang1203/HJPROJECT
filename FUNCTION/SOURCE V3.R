
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr","openxlsx",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2")
 
ipak(pkg)
# 
#       RAWDATA1           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FI")
#       RAWDATA2           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="EQ")[,-1]
#       RAWDATA3           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="AL")[,-1]
#       RAWDATA4           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FX")[,-1]
#       RAWDATA5           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FEPS")[,-1]
#       RAWDATA6           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="TPER")[,-1]
#       RAWDATA7           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="RATE")[,-1]
#       RAWDATA8           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="RISK")[,-1]
#       RAWDATA9           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="ETF")[,-1]
# 
#       colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
#       colnames(RAWDATA6) <- paste0("TPER_",colnames(RAWDATA6))
#       RAWDATA<- cbind(RAWDATA1,RAWDATA2,RAWDATA3,RAWDATA4,RAWDATA5,RAWDATA6,RAWDATA7,RAWDATA8,RAWDATA9)
#       STD_DT            <-  RAWDATA[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
#       RAWDATA           <-  cbind(STD_DT,RAWDATA[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
#       TEMP              <-  apply(RAWDATA%>%dplyr::select(-STD_DT), 2, as.numeric)%>%fillf
#       RAWDATAFULL       <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))%>%melt(id.vars="STD_DT")
# 
#       write.csv(RAWDATAFULL,"c:/work/RAWDATAFULL.csv")
# 

      ESGETF           <-  readxl::read_excel("c:/work/ESG_update.xlsx",sheet="Sheet2")
      ESGINDEX           <-  readxl::read_excel("c:/work/ESG_update.xlsx",sheet="Sheet1")
      ESGETF<-ESGETF%>%handling
      ESGINDEX<-ESGINDEX%>%handling
      MPWEIGHT           <-  readxl::read_excel("c:/work/MPWEIGHT.xlsx",sheet="Sheet2")
      MONTH              <-  readxl::read_excel("c:/work/LONG.xlsx",sheet="month")%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.frame
      STD_DT <-(as.Date(1950-01-01)+c(1:36500)*days(1))%>%as.data.frame
      names(STD_DT)<-"STD_DT"
      RAWDATA1           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FI")
      RAWDATA2           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="EQ")[,-1]
      RAWDATA3           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="AL")[,-1]
      RAWDATA4           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FX")[,-1]
      RAWDATA5           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FEPS")[,-1]
      RAWDATA6           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="TPER")[,-1]
      RAWDATA7           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="RATE")[,-1]
      RAWDATA8           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="RISK")[,-1]
      RAWDATA9           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="ETF")[,-1]
      RAWDATA10           <- readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FORECAST")[,-1]
      colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      colnames(RAWDATA6) <- paste0("FPER_",colnames(RAWDATA6))
      RAWDATA<- cbind(RAWDATA1,RAWDATA2,RAWDATA3,RAWDATA4,RAWDATA5,RAWDATA6,RAWDATA7,RAWDATA8,RAWDATA9)
      handling <- function(data){
      STD_DT            <-  data[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
      RAWDATA           <-  cbind(STD_DT,data[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
      TEMP              <-  apply(RAWDATA%>%dplyr::select(-STD_DT), 2, as.numeric)%>%fillf
      RAWDATA2           <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))
      #%>%melt(id.vars="STD_DT")
      return(RAWDATA2)
      }
      RAWDATAFULL<- read.csv("c:/work/RAWDATAFULL.csv")%>%select(-X)%>%
      mutate(STD_DT=as.Date(STD_DT))
      RAWDATA3    <- rbind(RAWDATAFULL,RAWDATA2)
      RAWDATA     <- RAWDATA3%>%dcast(STD_DT~variable)
      write.csv(RAWDATA,"c:/work/RAWDATA.csv")
      CLI               <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T,fileEncoding = "UCS-2LE")
      CLI  <- dcast(CLI,TIME ~LOCATION,  value.var = "Value")
      CLI  <- CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1)) %>%
      dplyr::select(STD_DT,USA,KOR,DEU,GBR,JPN,CHN)
      # CPI               <-  read.csv("c:/work/OECDCPI.csv",stringsAsFactors = FALSE,header=T,fileEncoding = "UCS-2LE")
      # CPI  <-               dcast(CPI,TIME ~LOCATION,  value.var = "Value")

      CLI %>%filter(STD_DT>"2023-01-01")



     
     
        
      RAWDATA <-  read.csv("c:/work/RAWDATA.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%
        mutate(STD_DT=as.Date(STD_DT))%>%mutate(AL=(WREPRA+WRINFRA+GSCI)/3)%>%mutate(KRBONDH=KRBOND*USDKRW)%>%
        mutate(WORLD2 =WORLD*USDKRW)%>%
        mutate(WRBOND2=WRBOND*USDKRW)%>%
        mutate(AL2    =AL*USDKRW)%>%
        mutate(GSCI2  =GSCI*USDKRW)
        
        
        RAWDATA <-STD_DT %>% left_join(RAWDATA,by="STD_DT") %>% left_join(CLI,by="STD_DT")
      #RAWDATA<- BDIND %>% left_join(RAWDATA%>%select(STD_DT,DXY,WORLD), by="STD_DT")
    # HG           <-  readxl::read_excel("c:/work/HEDGEFUND.xlsx",sheet="Sheet2")
    # STD_DT            <-  HG[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
    # HG           <-  cbind(STD_DT,HG[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
    # TEMP              <-  apply(HG%>%select(-STD_DT), 2, as.numeric) 
    # HG           <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))
    # HG <-  HG %>%fillf
    # MON <- HG %>%fillf%>%trans_rt()%>% apply.monthly(., Return.cumulative)
    # YTN <- HG%>%trans_rt()%>% apply.yearly(., Return.cumulative)%>%round(4)
    # 
    # RAWDATA%>%filter(STD_DT>"2022-12-31")
    # 
    # LONG           <-  readxl::read_excel("c:/work/LONG.xlsx",sheet="Sheet1")
    # STD_DT            <-  LONG[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
    # LONG           <-  cbind(STD_DT,LONG[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
    # TEMP              <-  apply(LONG%>%select(-STD_DT), 2, as.numeric) 
    # LONG           <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))
    # 
    # 

      macro  <- RAWDATA%>%select(STD_DT,UK10Y,DEM10Y,KR10Y,CN10Y,FR10Y,JP10Y,MOVE,HYSP,VIX)
      eq     <- RAWDATA%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS,KOSDAQ,
                               USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSEX,MSSZ,MSSM,MSBG)
      fi   <- RAWDATA%>%select(STD_DT,WRBOND,WRGOVT,WRIG,WRHY,USGOVT,USBOND,USIG,USHY,EUBOND,EUIG,EUHY,EUGOVT,KRBOND,EMBOND,EMGOVT,EMGOVTL,CNBOND,USLONG,USSHORT,USMID)
      al   <- RAWDATA%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC,SIL,COP)
      #style   <- RAWDATA%>%select(STD_DT,WRVALE,WRGROWTH)%>%fillf
      feps <- RAWDATA%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                               FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS,
                               FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,
                               FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG
                               )

      # fi %>% trans_rt("month")%>%dt_trans %>% filter(substr(STD_DT,1,7)==substr(STDDT,1,7))
      std <- cbind(eq,fi[,-1],al[,-1],feps[,-1]) %>% filter(STD_DT==(STDDT))
      week<- cbind(eq,fi[,-1],al[,-1],feps[,-1]) %>% filter(STD_DT==(STDDT%m-%weeks(1)))
      month <- cbind(eq,fi[,-1],al[,-1],feps[,-1]) %>% filter(STD_DT==(STDDT %m-%months(1)))
      year <- cbind(eq,fi[,-1],al[,-1],feps[,-1]) %>% filter(STD_DT==(STDDT%m-%years(1)))

    
      
      ret_wek <- ((std[-1]/week[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame
      ret_mon <- ((std[-1]/month[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame
      ret_yea <- ((std[-1]/year[-1]-1) %>%round(4)* 100)%>%cbind(STD_DT=STDDT)%>%data.frame
      ret_ytd <-  cbind(eq,fi[,-1],al[,-1],feps[,-1])%>% trans_rt("year")%>%tail(n=1)%>%.[1] %>%round(4)* 100
      ret2_ytd <-  feps%>% trans_rt("year")%>%tail(n=1)%>%.[1] %>%round(4)* 100
      ret_ytm<- cbind(eq,fi[,-1],al[,-1],feps[,-1])%>% trans_rt("month")%>%tail(n=1)%>%.[1] %>%round(4)* 100
      
     
      RAWDATA <-  RAWDATA %>%filter(STD_DT!="2008-02-29")%>%filter(STD_DT!="2012-02-29")%>%filter(STD_DT!="2016-02-29")%>%filter(STD_DT!="2020-02-29")
      retm    <-  RAWDATA %>%trans_rt("month")%>%dt_trans()%>%mutate(BM=0.6*WORLD+0.4*WRBOND)%>%mutate(MY=3*(0.4*NASDAQ+0.6*USGOVT))%>%mutate(BM2=0.5*WORLD+0.3*WRBOND+0.2*AL)%>%
                  mutate(INF=WRGOVT-WRTIP)%>%mutate(CREDIT=WRIG-WRGOVT)%>%mutate(RINTEREST=WRTIP)%>%mutate(GROWTH=DM)%>%mutate(FX=USDKRW)%>%mutate(emerging=DM-EM)%>%mutate(STD_DT_L1=lag(STD_DT,1))
 
      
      # retm    <-  RAWDATA %>%trans_rt("quater")%>%dt_trans()%>%mutate(BM=0.6*WORLD+0.4*WRBOND)%>%mutate(MY=3*(0.4*NASDAQ+0.6*USGOVT))%>%mutate(BM2=0.5*WORLD+0.3*WRBOND+0.2*AL)%>%
      #   mutate(INF=WRGOVT-WRTIP)%>%mutate(CREDIT=WRIG-WRGOVT)%>%mutate(RINTEREST=WRTIP)%>%mutate(GROWTH=DM)%>%mutate(FX=USDKRW)%>%mutate(emerging=DM-EM)%>%mutate(STD_DT_L1=lag(STD_DT,1))
      # 
      retsy     <-    eq%>%left_join(al,by="STD_DT")%>%trans_rt("year") %>%dt_trans%>%mutate(STD_DT=(paste0(substr(STD_DT,1,4),"-12-31"))%>%as.Date)
      reteq     <-    eq%>% trans_rt("month")%>% dt_trans
      
      reteq_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS)%>% exname()
      reteq_yea<- ret_yea%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS)%>% exname()
      reteq_mon<- ret_mon%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS)%>% exname()
      reteq_wek<- ret_wek%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS)%>% exname()
      retfeps_ytd<- ret2_ytd %>% dt_trans%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                                                   FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_NIFTY,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS)%>% exname()
      retfeps_yea<- ret_yea%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                                        FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_NIFTY,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS)%>% exname()
      retfeps_mon<- ret_mon%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                                     FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_NIFTY,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS)%>% exname()
      retfeps_wek<- ret_wek%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                                     FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_NIFTY,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS)%>% exname()
      
      retst_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSSZ,MSSM,MSBG)%>% exname()
      retst_yea<- ret_yea%>%select(STD_DT,USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSSZ,MSSM,MSBG)%>% exname()
      retst_mon<- ret_mon%>%select(STD_DT,USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSSZ,MSSM,MSBG)%>% exname()
      retst_wek<- ret_wek%>%select(STD_DT,USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSSZ,MSSM,MSBG)%>% exname()
      
      retstfeps_ytd<- ret2_ytd %>% dt_trans%>%select(STD_DT,FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE
                                                     ,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG)%>% exname()
      retstfeps_yea<- ret_yea %>%select(STD_DT,FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG)%>% exname()
      retstfeps_mon<- ret_mon %>%select(STD_DT,FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG)%>% exname()
      retstfeps_wek<- ret_wek %>%select(STD_DT,FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG)%>% exname()
      

      retfi_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,EMGOVT,EMGOVTL,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_yea<- ret_yea%>%           select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,EMGOVT,EMGOVTL,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_mon<- ret_mon%>%           select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,EMGOVT,EMGOVTL,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_wek<- ret_wek%>%           select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,EMGOVT,EMGOVTL,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      
      retai_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC,SIL,COP)%>% exname()
      retai_yea<- ret_yea%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC,SIL,COP)%>% exname()
      retai_mon<- ret_mon%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC,SIL,COP)%>% exname()
      retai_wek<- ret_wek%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC,SIL,COP)%>% exname()
    
      