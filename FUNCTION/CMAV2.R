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
    
    ####
    RAWDATA%>%
      filter(variable=="WORLD"|variable=="WRBOND"|variable=="WRIG"|variable=="WRGOVT"|variable=="USIG")%>%
      dcast(STD_DT~variable)%>%
      trans_rt("day")%>%dt_trans%>%
      mutate(BM=0.7*WORLD+0.3*WRBOND)%>%mutate(PORT1=0.7*WORLD+0.3*WRIG)%>%mutate(PORT2=0.7*WORLD+0.3*WRGOVT)%>%
      dplyr::select(STD_DT,BM,PORT1,PORT2)%>%cuml
    
      RAWDATA%>%
      filter(variable=="WORLD"|variable=="WRBOND"|variable=="WRIG"|variable=="WRGOVT"|variable=="USIG")%>%
      dcast(STD_DT~variable)%>%filter(STD_DT>"2013-01-01")%>%
      trans_rt("day")%>%dt_trans%>%
      mutate(BM=0.7*WORLD+0.3*WRBOND)%>%mutate(PORT1=0.7*WORLD+0.3*WRIG)%>%mutate(PORT2=0.7*WORLD+0.3*USIG)%>%
      dplyr::select(STD_DT,BM,PORT1,PORT2)%>%apply.yearly(., Return.cumulative)%>%na.omit
    
      