
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
      RAWDATA <- read.csv("c:/work/RAWDATA2024V1.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
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
      ,RAWDATA
      
      )%>%as.data.table%>%mutate(variable==as.character(variable))
      write.csv(TMP ,"c:/work/RAWDATA2024V2.csv")
      RAWDATA <- read.csv("c:/work/RAWDATA2024V2.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
      

      RAWDATA%>%filter(variable=="WORLDT")%>%dcast(STD_DT~variable)%>%arrange(STD_DT,decreasing=T)
      TMP<- rbind(
      TMP,
      readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="Sheet1") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      )
      TMP <- rbind(
      readxl::read_excel("c:/work/MACROH.xlsx",sheet="weekly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/MACROH.xlsx",sheet="monthly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      ,readxl::read_excel("c:/work/MACROH.xlsx",sheet="quarter") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
      )%>%filter(STD_DT>"2022-12-31")
TMP <- rbind(RAWDATA,TMP)
