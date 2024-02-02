
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr","openxlsx","DT",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2","depmixS4",
        "shiny","shinythemes", "shinydashboard","ggplot2","quantmod","dynlm")
 
ipak(pkg)
remove.packages("bvartools")

      #oecd경기선행지수수
      CLI  <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T)
      CLI  <- dcast(CLI,TIME ~LOCATION,  value.var = "Value")
      CLI  <- CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1))
     
      RAWDATA5<- readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FEPS")
      RAWDATA6<- readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="TPER")
      colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      colnames(RAWDATA6) <- paste0("TPER_",colnames(RAWDATA6))
      TMP <- rbind(
       readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="EQ")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="AL")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FX")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FI")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,RAWDATA5%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,RAWDATA6%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="RATE")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="RISK")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable!="HYSP")
      ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FORECAST")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/MACROH.xlsx",sheet="weekly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,read.csv("c:/work/RAWDATA2024-02.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>% filter(STD_DT<"2024-01-01")%>%na.omit
      
      )%>%as.data.table%>%mutate(variable==as.character(variable))
      write.csv(TMP ,"c:/work/RAWDATA2024-03.csv")
    
      RAWDATA <- read.csv("c:/work/RAWDATA2024-03.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
      #write.csv(RAWDATA ,"c:/work/RAWDATA2024-02.csv")
      RAWDATA <- RAWDATA%>%filter(variable!="WORLDT")
      TMP%>%filter(variable=="WORLDT")%>%dcast(STD_DT~variable)%>%arrange(STD_DT,decreasing=T)
      TMP<- rbind(
      RAWDATA,
      readxl::read_excel("c:/work/test.xlsx",sheet="Sheet1") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      )
      TMP <- rbind(
      readxl::read_excel("c:/work/MACROH.xlsx",sheet="weekly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/MACROH.xlsx",sheet="monthly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/MACROH.xlsx",sheet="quarter") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      )%>%filter(STD_DT>"2022-12-31")
TMP <- rbind(RAWDATA,TMP)
