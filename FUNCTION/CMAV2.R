    RAWDATA%>%
      filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSUS"|variable=="MSEU"|variable=="MSJP"|variable=="MSCN"|variable=="EM"|variable=="MSKR"|
             variable=="KRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="WREPRA"|variable=="WTI"|variable=="GOLD")%>%dcast(STD_DT~variable)%>%
             trans_rt("year")%>%round(4)%>%View

    RAWDATA%>%
      filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSUS"|variable=="MSEU"|variable=="MSJP"|variable=="MSCN"|variable=="EM"|variable=="MSKR"|
               variable=="KRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="WREPRA"|variable=="WTI"|variable=="GOLD")%>%dcast(STD_DT~variable)%>%na.omit%>%
      trans_rt("month")%>%cor%>%round(4)%>%View
    
    RAWDATA%>%
      filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSUS"|variable=="MSEU"|variable=="MSJP"|variable=="MSCN"|variable=="EM"|variable=="MSKR"|
               variable=="KRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="WREPRA"|variable=="WTI"|variable=="GOLD")%>%dcast(STD_DT~variable)%>%na.omit%>%
      trans_rt("month")%>%dt_trans%>%PA
    
    RAWDATA%>%
      filter(variable=="EM")
    