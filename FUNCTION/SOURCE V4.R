
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("readxl","data.table","dplyr","zoo","writexl","plyr","openxlsx","DT",
        "lubridate","xlsx","gridExtra","quantmod","reshape","reshape2","depmixS4","matlib",
        "shiny","shinythemes", "shinydashboard","ggplot2","quantmod","dynlm")
 
ipak(pkg)
remove.packages("bvartool")
      #oecd경기선행지수수
      CLI  <-  read.csv("c:/work/CLI.csv",stringsAsFactors = FALSE,header=T)
      CLI <- dcast(CLI,TIME~LOCATION,value.var = "Value")
      CLI  <-  CLI %>% as.data.frame() %>% mutate(STD_DT=paste0(CLI$TIME,"-01") %>%ymd+months(1)-days(1))
        colnames(CLI)[which(colnames(CLI)=="G-20")]<-"G20"
     
      RAWDATA5<- readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FEPS")
      RAWDATA6<- readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="TPER")
      colnames(RAWDATA5) <- paste0("FEPS_",colnames(RAWDATA5))
      colnames(RAWDATA6) <- paste0("TPER_",colnames(RAWDATA6))

     
      TMP2 <- rbind(
        readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="EQ")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="AL")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FX")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FI")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,RAWDATA5%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,RAWDATA6%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="RATE")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="RISK")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable!="HYSP")
        ,readxl::read_excel("c:/work/RAWDATA2024.xlsx",sheet="FORECAST")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/MACRO.xlsx",sheet="weekly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,readxl::read_excel("c:/work/MACRO.xlsx",sheet="monthly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        #,readxl::read_excel("c:/work/MACRO.xlsx",sheet="quarter") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit
        ,TMP

      )%>%as.data.table%>%mutate(variable==as.character(variable))

      write.csv(TMP2 ,"c:/work/RAWDATA2024-05.csv")
      RAWDATA <- read.csv("c:/work/RAWDATA2024-05.csv",stringsAsFactors = FALSE)%>%dplyr::select(-X)%>%mutate(STD_DT=as.Date(STD_DT))%>%as.data.table%>%na.omit
        #RAWDATA <- RAWDATA %>% filter(STD_DT<"2024-01-01")
      #write.csv(RAWDATA ,"c:/work/RAWDATA2024-02.csv")
      # RAWDATA <- RAWDATA%>%filter(variable!="MSTW"&variable!="MSJP"&variable!="MSIN"&variable!="MSDE"&variable!="MSFR")

      RAWDATA%>%filter(variable=="KRBONDH")%>%dcast(STD_DT~variable)%>%arrange(STD_DT)
     
      

      RAWDATA%>%filter(variable=="KRBONDH")%>%arrange(STD_DT)
      TMP2<- rbind(RAWDATA%>%
                     filter(variable!="EMEXCN"),
      readxl::read_excel("c:/work/RAWDATATMP.xlsx",sheet="weekly") %>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%
        filter(variable=="N"))%>%filter(STD_DT<"2024-01-01")


