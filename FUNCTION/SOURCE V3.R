
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2")
 
ipak(pkg)

      # RAWDATA1           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FI")
      # RAWDATA2           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="EQ")[,-1]
      # RAWDATA3           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="AL")[,-1]
      # RAWDATA4           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FX")[,-1]
      # RAWDATA5           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="FEPS")[,-1]
      # RAWDATA6           <-  readxl::read_excel("c:/work/RAWDATA_HI.xlsx",sheet="MACRO")[,-1]
      # colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      # RAWDATA<- cbind(RAWDATA1,RAWDATA2,RAWDATA3,RAWDATA4,RAWDATA5,RAWDATA6)
      # STD_DT            <-  RAWDATA[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
      # RAWDATA           <-  cbind(STD_DT,RAWDATA[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
      # TEMP              <-  apply(RAWDATA%>%select(-STD_DT), 2, as.numeric)%>%fillf
      # RAWDATAFULL       <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))%>%melt(id.vars="STD_DT")
      # 
      # write.csv(RAWDATAFULL,"c:/work/RAWDATAFULL.csv")

      RAWDATA1           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FI")
      RAWDATA2           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="EQ")[,-1]
      RAWDATA3           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="AL")[,-1]
      RAWDATA4           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FX")[,-1]
      RAWDATA5           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="FEPS")[,-1]
      RAWDATA6           <-  readxl::read_excel("c:/work/RAWDATA_ver4.xlsx",sheet="MACRO")[,-1]
      colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      RAWDATA<- cbind(RAWDATA1,RAWDATA2,RAWDATA3,RAWDATA4,RAWDATA5,RAWDATA6)%>%fillf

      STD_DT            <-  RAWDATA[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
      RAWDATA           <-  cbind(STD_DT,RAWDATA[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
      TEMP              <-  apply(RAWDATA%>%select(-STD_DT), 2, as.numeric)
      RAWDATA2           <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))%>%melt(id.vars="STD_DT")

      RAWDATAFULL<- read.csv("c:/work/RAWDATAFULL.csv")%>%select(-X)%>%
      mutate(STD_DT=as.Date(STD_DT))
      RAWDATA3    <- rbind(RAWDATAFULL,RAWDATA2)
      RAWDATA <- RAWDATA3%>%dcast(STD_DT~variable)
      write.csv(RAWDATA,"c:/work/RAWDATA.csv")
      # CLI               <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T,fileEncoding = "UCS-2LE")
      # CLI  <- dcast(CLI,TIME ~LOCATION,  value.var = "Value")
      # CLI<- CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1)) %>%       
      # dplyr::select(STD_DT,USA,KOR) 
      # 
      # 
      # CLI%>%View

      # 
      # write.csv(CLI,"c:/work/CLI2.csv")
      # 
      # 
      RAWDATA <-  read.csv("c:/work/RAWDATA.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%
      mutate(STD_DT=as.Date(STD_DT))%>%mutate(AL=(WREPRA+WRINFRA+HFRI)/3)%>%mutate(KRBONDH=KRBOND/USDKRW)
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

      macro1 <- RAWDATA%>%select(STD_DT,UK10Y,DEM10Y,KR10Y,CN10Y,FR10Y,JP10Y,MOVE)
      eq     <- RAWDATA%>%select(STD_DT,WORLD,DM,EM,DOW,SP500,NASDAQ,KOSPI,EURO50,FTSE,NIKKEI,NIFTY,DAX,CAC,TSX,BVSP,NIFTY,SHANGHAI,CSI300,HANGS,KOSDAQ,
                               USGROWTH,USVALUE,USLVOL,USHDIV,USMOM,USQUAL,KRGROWTH,KRVALUE,MSEX,MSSZ,MSSM,MSBG)
      fi   <- RAWDATA%>%select(STD_DT,WRBOND,WRGOVT,WRIG,WRHY,USGOVT,USBOND,USIG,USHY,EUBOND,EUIG,EUHY,EUGOVT,KRBOND,EMBOND,CNBOND,USLONG,USSHORT,USMID)
      al   <- RAWDATA%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC)
      #style   <- RAWDATA%>%select(STD_DT,WRVALE,WRGROWTH)%>%fillf
      feps <- RAWDATA%>%select(STD_DT,FEPS_WORLD,FEPS_DM,FEPS_EM,FEPS_DOW,FEPS_SP500,FEPS_NASDAQ,FEPS_KOSPI,FEPS_EURO50,FEPS_FTSE,FEPS_NIKKEI,FEPS_NIFTY,FEPS_DAX,
                               FEPS_CAC,FEPS_TSX,FEPS_BVSP,FEPS_SHANGHAI,FEPS_CSI300,FEPS_HANGS,
                               FEPS_USGROWTH,FEPS_USVALUE,FEPS_USLVOL,FEPS_USHDIV,FEPS_USMOM,
                               FEPS_USQUAL,FEPS_KRGROWTH,FEPS_KRVALUE,FEPS_MSSZ,FEPS_MSSM,FEPS_MSBG
                               )
      getSymbols('VIXCLS', src='FRED')
      getSymbols('FEDFUNDS', src='FRED')
      getSymbols('DGS30', src='FRED')
      getSymbols('DGS10', src='FRED')
      getSymbols('DGS2', src='FRED')
      getSymbols('BAMLC0A0CM', src='FRED')
      getSymbols('DFII10', src='FRED')

      getSymbols('BAMLH0A0HYM2', src='FRED')
      getSymbols('DTWEXBGS', src='FRED')
      getSymbols('DEXKOUS', src='FRED')

      macro2<- cbind(DGS30,DGS10,DGS2,BAMLC0A0CM,BAMLH0A0HYM2,VIXCLS,DTWEXBGS,DEXKOUS,DFII10,FEDFUNDS)%>%dt_trans%>%exname()

      macro <- macro1%>%full_join(macro2,by="STD_DT")%>%fillf

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
      
     

      retm <-  RAWDATA %>%trans_rt("month")%>%dt_trans()%>% mutate(AL=1/3*(WRINFRA+WREPRA+CRBTR))%>%mutate(BM=0.6*WORLD+0.4*WRBOND)%>%mutate(MY=3*(0.4*NASDAQ+0.6*USGOVT))%>%
               mutate(INF=WRGOVT-TIP)%>%mutate(CREDIT=WRIG-WRGOVT)%>%mutate(RINTEREST=TIP)%>%mutate(GROWTH=DM)%>%mutate(FX=USDKRW)%>%mutate(emerging=DM-EM)
      
      
      
      
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
      

      retfi_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_yea<- ret_yea%>%select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_mon<- ret_mon%>%select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      retfi_wek<- ret_wek%>%select(STD_DT,WRBOND,USBOND,EUBOND,EMBOND,KRBOND,USGOVT,USSHORT,USMID,USLONG,WRIG,USIG,EUIG,WRHY,USHY,EUHY)%>% exname()
      
      retai_ytd<- ret_ytd%>%dt_trans%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC)%>% exname()
      retai_yea<- ret_yea%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC)%>% exname()
      retai_mon<- ret_mon%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC)%>% exname()
      retai_wek<- ret_wek%>%select(STD_DT,PEF,USREIT,EUCREIT,ASIACREIT,USCREIT,WRCINFRA,GSCI,CRBTR,WRINFRA,WREPRA,HFRI,HMACRO,GOLD,WTI,FNG1,ENGM1,BITC)%>% exname()
    
      