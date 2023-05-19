server <- function(input, output){
  ###### WEEK, MONTH, YEAR, YTD####bar plot
  output$eqrtw <- renderPlot({graphbar(reteq_wek,"red","Weekly Return")})
  output$eqrtm <- renderPlot({graphbar(reteq_mon,"blue","Monthly Return")})
  output$eqrty <- renderPlot({graphbar(reteq_yea,"green","Yearly Return")})
  output$eqytd <- renderPlot({graphbar(reteq_ytd,"orange","YTD")})
  output$fepsrtw <- renderPlot({graphbar(retfeps_wek,"red","Weekly(EPS) Return")})  
  output$fepsrtm <- renderPlot({graphbar(retfeps_mon,"blue","Monthly(EPS) Return")})   
  output$fepsrty <- renderPlot({graphbar(retfeps_yea,"green","Yearly(EPS) Return")})   
  output$fepsytd <- renderPlot({graphbar(retfeps_ytd,"orange","YTD(EPS)")})  
  output$firtw <- renderPlot({graphbar(retfi_wek,"red","Weekly Return")})  
  output$firtm <- renderPlot({graphbar(retfi_mon,"blue","Monthly Return")})   
  output$firty <- renderPlot({graphbar(retfi_yea,"green","Yearly Return")})   
  output$fiytd <- renderPlot({graphbar(retfi_ytd,"orange","YTD")})  
  output$airtw <- renderPlot({graphbar(retai_wek,"red","Weekly(EPS) Return")})  
  output$airtm <- renderPlot({graphbar(retai_mon,"blue","Monthly(EPS) Return")})   
  output$airty <- renderPlot({graphbar(retai_yea,"green","Yearly(EPS) Return")})   
  output$aiytd <- renderPlot({graphbar(retai_ytd,"orange","YTD(EPS)")})  
  output$strtw <- renderPlot({graphbar(retst_wek,"red","Weekly Return")})  
  output$strtm <- renderPlot({graphbar(retst_mon,"blue","Monthly Return")})   
  output$strty <- renderPlot({graphbar(retst_yea,"green","Yearly Return")})   
  output$stytd <- renderPlot({graphbar(retst_ytd,"orange","YTD")})  
  output$stfepsrtw <- renderPlot({graphbar(retstfeps_wek,"red","Weekly(EPS) Return")})
  output$stfepsrtm <- renderPlot({graphbar(retstfeps_mon,"blue","Monthly(EPS) Return")})   
  output$stfepsrty <- renderPlot({graphbar(retstfeps_yea,"green","Yearly(EPS) Return")})   
  output$stfepsytd <- renderPlot({graphbar(retstfeps_ytd,"orange","YTD(EPS)")})  
  
  
  output$tbl <- renderDT({
    if(input$table=="EPS"){
      data.frame(bind_rows(retfeps_wek,retfeps_mon,retfeps_ytd,retfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    else{
      data.frame(bind_rows(reteq_wek,reteq_mon,reteq_ytd,reteq_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    })
  
  output$tbl2 <- renderDT({
    if(input$table2=="FI"){
      data.frame(bind_rows(retfi_wek,retfi_mon,retfi_ytd,retfi_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    else{
      data.frame(bind_rows(retai_wek,retai_mon,retai_ytd,retai_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    })
  output$tbl3 <- renderDT({
    if(input$table3=="RT"){
     data.frame(bind_rows(retst_wek,retst_mon,retst_ytd,retst_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    else{
      data.frame(bind_rows(retstfeps_wek,retstfeps_mon,retstfeps_ytd,retstfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
      }
    })
  
    ##HISTORICAL

  output$eq3 <- renderPlot({
    ((1+eq[,c("STD_DT","WORLD",input$variable)]%>%trans_rt("day")%>%na.omit)%>%dt_trans%>%exname%>%filter(STD_DT >= input$dateRange[1] & STD_DT <= input$dateRange[2])%>%trans_rt2()%>%cumprod*100)%>%
      dt_trans %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line(size=1)+ 
      ggtitle("누적수익률") +
      theme(legend.text = element_text(size=15))
    })
  output$pt_graph1 <- renderPlot({
    TMP <-  eq%>%left_join(fi,by="STD_DT")%>%left_join(al,by="STD_DT")%>%.[,c("STD_DT",input$pt)]%>%trans_rt("month") %>%apply.monthly(., Return.cumulative)
    TMP2 <- cbind(TMP[,1],TMP[,2],(TMP[,1]*input$pt1+TMP[,2]*input$pt2))
    (100*cumprod(1+TMP2))%>%exname()%>%dt_trans%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line(size=1)+ggtitle("누적수익률") +
      theme(legend.text = element_text(size=15))
    })
  output$mdd <- renderPlot({
    Drawdowns(eq%>%.[,c("STD_DT","WORLD",input$variable)]%>%trans_rt("day"))%>%dt_trans%>%exname%>%filter(STD_DT >= input$dateRange[1] & STD_DT <= input$dateRange[2])%>% 
      melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
      geom_line(size=1)+ 
      ggtitle("Maximal Drawdown") +
      theme(legend.text = element_text(size=15))
      })
  output$Plot2 <- renderPlot({
    ggplot(retsy, mapping = aes_string(x = retsy$STD_DT,y = input$variableh)) +
     geom_bar(stat="identity", fill="steelblue") + 
     ggtitle("연간수익률") +
     theme(legend.text = element_text(size=15))
  })
  output$Plot4 <- renderPlot({
    retsm %>% filter(STD_DT >= input$dateRange[1] & STD_DT <= input$dateRange[2])%>%.[,-1]%>%cor %>% 
    ggcorrplot( type='lower',lab=TRUE)+
    theme(legend.text = element_text(size=15))
  })
  output$boxplot <- renderPlot({
    eq%>%.[,c("STD_DT","WORLD",input$variable)]%>%trans_rt("month")%>%apply.monthly(., Return.cumulative)%>%dt_trans()%>%exname()%>% melt(id.vars="STD_DT")%>%
    ggplot(aes(x=STD_DT, y=value, fill=variable)) +
    geom_boxplot()+
    theme(legend.text = element_text(size=15))
  })
   output$den<- renderPlot({
     reteq %>%select(STD_DT,input$variableh)%>%melt(id.vars="STD_DT") %>% 
     ggplot(aes(x=value))+
     geom_histogram(color="black", fill="lightblue", linetype="dashed")+
     theme(legend.text = element_text(size=15))
   })
   output$den2<- renderDT({
     rbind(Mean=reteq %>%select(input$variableh)%>%as.matrix()%>%mean%>%round(4)*100*12,
           Median=reteq %>%select(input$variableh)%>%as.matrix()%>%median%>%round(4)*100,
           SD=((reteq %>%select(input$variableh)%>%as.matrix()%>%var*12)^0.5)%>%round(4)*100,
           SK=reteq %>%select(input$variableh)%>%as.matrix()%>%skewness%>%round(4)*100,
           KT=reteq %>%select(input$variableh)%>%as.matrix()%>%kurtosis%>%round(4)*100
     )
   })
   output$eq3 <- renderPlot({
    ((1+eq[,c("STD_DT","WORLD",input$variable)]%>%trans_rt("day")%>%na.omit)%>%dt_trans%>%exname%>%filter(STD_DT >= input$dateRange[1] & STD_DT <= input$dateRange[2])%>%trans_rt2()%>%cumprod*100)%>%
       dt_trans %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
       geom_line(size=1)+ 
       ggtitle("누적수익률") 
   })
  # output$vixtable <- renderDT({
  #    vixtable<- macro %>% select(STD_DT,VIX)%>%trans_rt("quater")%>%dt_trans%>%
  #      left_join(HG%>%select(STD_DT,HMACRO,WORLD)%>%trans_rt("quater")%>%dt_trans,by="STD_DT")%>%na.omit%>%
  #      mutate(percent_rank =ntile(V1,4))%>%
  #      group_by(percent_rank)%>%dplyr::summarise(HMACRO=mean(HMACRO),WORLD=mean(WORLD),VIX=mean(V1)) %>%round(3)
  #   
  #   
  #   vixtable
  #   
  # })
  # output$movetable <- renderDT({
  #   movetable <- HG%>%dplyr::select(STD_DT,WORLD,HMACRO,MOVE)%>%trans_rt("quater")%>%dt_trans%>%na.omit%>%mutate(percent_rank =ntile(MOVE,4))%>%
  #      group_by(percent_rank)%>%dplyr::summarise(HMACRO=mean(HMACRO),WORLD=mean(WORLD),MOVE=mean(MOVE)) %>%round(3)
  # 
  #   # movetable <- HG %>% select(STD_DT,MOVE)%>%
  #   #   right_join(HG%>%select(STD_DT,HMACRO,WORLD)%>%trans_rt("quater")%>%dt_trans,by="STD_DT")%>%na.omit%>%
  #   #   mutate(percent_rank =ntile(MOVE,4))%>%
  #   #   group_by(percent_rank)%>%dplyr::summarise(HMACRO=mean(HMACRO),WORLD=mean(WORLD),MOVE=mean(MOVE)) %>%round(3)
  #   # 
  #   
  #   movetable
  # })
  # 


###########risk###########################################
output$USSP<- renderPlot({
  RAWDATA%>%select(STD_DT,US30Y,US2Y,USBR)%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%reshape2::melt(id.vars="STD_DT")%>%na.omit%>%ggplot(aes(x=STD_DT, y=value, group = variable))+ 
    geom_line(alpha = 1, aes(col=(variable))) 
})
output$KRSP<- renderPlot({
  RAWDATA%>%select(STD_DT,KR10Y,KR2Y,KRBR)%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%reshape2::melt(id.vars="STD_DT")%>%na.omit%>%ggplot(aes(x=STD_DT, y=value, group = variable))+ 
    geom_line(alpha = 1, aes(col=(variable))) 
})

output$GOVT<- renderPlot({
  RAWDATA%>%select(STD_DT,US10Y,UK10Y,DEM10Y,KR10Y,CN10Y,FR10Y,JP10Y)%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%reshape2::melt(id.vars="STD_DT")%>%na.omit%>%ggplot(aes(x=STD_DT, y=value, group = variable))+ 
    geom_line( aes(col=(variable))) 
})

output$SIGNAL<- renderPlot({
  sec <- with(macro, train_sec(HYSP, IGSP))
  macro%>%select(STD_DT,HYSP,IGSP) %>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%na.omit %>%
    ggplot(aes(STD_DT)) +
    geom_line(aes(y = HYSP), colour = "blue") +
    geom_line(aes(y = sec$fwd(IGSP)), colour = "red") +
    scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "IGSP"))+
    theme(
      axis.title.y = element_text(color =  "blue", size=13),
      axis.title.y.right = element_text(color = "red", size=13)
    )
  })
output$SIGNAL2<- renderPlot({
  macro%>%select(STD_DT,VIX,MOVE)%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%reshape2::melt(id.vars="STD_DT")%>%na.omit%>%ggplot(aes(x=STD_DT, y=value, group = variable))+ 
    geom_line(aes(col=(variable))) 
})

output$FX<- renderPlot({
  
  
  sec <- with(RAWDATA, train_sec(USDKRW, DXY))
  RAWDATA%>%select(STD_DT,DXY,USDKRW)%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])%>%na.omit%>%
    ggplot(aes(STD_DT)) +
    geom_line(aes(y = USDKRW), colour = "blue") +
    geom_line(aes(y = sec$fwd(DXY)), colour = "red") +
    scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "DXY"))+
    theme(
      axis.title.y = element_text(color =  "blue", size=13),
      axis.title.y.right = element_text(color = "red", size=13)
    )
  
  
})

output$rollcor1 <- renderPlot({
  
  tmp  <- RAWDATA%>%select(STD_DT,WORLD,WRBOND) %>% trans_rt("month")%>%dt_trans%>%na.omit%>%filter(STD_DT >= (input$RI[1]-years(3)) & STD_DT <= input$RI[2])
  data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEAR =roll_cor(tmp$WORLD, tmp$WRBOND, width = 36))%>%na.omit%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
    geom_line(size=1)
  

})

output$rollcor2 <- renderPlot({
  

  tmp  <- RAWDATA%>%select(STD_DT,SP500,USGOVT) %>%trans_rt("week")%>%dt_trans%>%na.omit%>%filter(STD_DT >= input$RI[1] & STD_DT <= input$RI[2])
  TTMO <- data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEAR = roll_cor(tmp$SP500, tmp$USGOVT, width = 14))%>%left_join(macro%>%select(STD_DT,VIX),by="STD_DT")
  
  sec <- with(TTMO, train_sec(ROLLIMG5YEAR, VIX))
  TTMO%>%select(STD_DT,ROLLIMG5YEAR,VIX)%>%na.omit%>%
    ggplot(aes(STD_DT)) +
    geom_line(aes(y = ROLLIMG5YEAR), colour = "blue") +
    geom_line(aes(y = sec$fwd(VIX)), colour = "red") +
    scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "VIX"))
  
 
})



output$rollcor3 <- renderPlot({
  
  
  G1 <-rollcor("SP500","KOSPI","2000-01-01",60,23)%>%left_join(
  rollcor("SHANGHAI","KOSPI","2000-01-01",60,23),by="STD_DT") %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) + geom_line(size=1)  
  
  G2 <-rollcor("SP500","EM","2000-01-01",60,23)%>%left_join(
    rollcor("SHANGHAI","EM","2000-01-01",60,23),by="STD_DT") %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) + geom_line(size=1)  
  
  grid.arrange(G1,G2, 
               ncol = 2, nrow = 1)


})

####Model portfolio
output$mp <- renderPlot({
  
  MP <- ((1+RAWDATA[,c("STD_DT","WORLD","USVALUE","MSKR","WRBOND","KRBOND","USSHORT","USIG","WREPRA","WRINFRA","GSCI","HMACRO","EMBOND","WRHY")]%>%trans_rt("day")%>%dt_trans%>%
            filter(STD_DT >= input$MP[1] & STD_DT <= input$MP[2])%>%
            mutate(MP=(23.5*WORLD+2*USVALUE+7.9*MSKR+12.1*WRBOND+2*USSHORT+2*USIG+32.3*KRBOND+3.7*WREPRA+11.7*WRINFRA+3.7*GSCI)/100))%>%mutate(BM=WORLD*0.5+0.3*KRBOND+0.2*WRBOND)%>%select(STD_DT,MP,BM)%>%
           exname%>%trans_rt2()%>%cumprod*100)%>%
           dt_trans 
  
  MP %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
    geom_line(size=1)+ 
    ggtitle("누적수익률") +
    theme(legend.text = element_text(size=15))
  
  
})

output$mp2 <- renderPlot({
  
  bar<-  data.frame(ASSET=c("글로벌주식","미국가치","한국","글로벌채권","미국단기채","미국IG","국내채권","글로벌부동산","글로벌인프라","원자재"),WEIGHT=c(23.5,2,7.9,12.1,2,2,32.3,3.7,11.7,3.7))%>%
    ggplot(aes(x=ASSET, y=WEIGHT)) +  
    geom_bar(position="dodge", stat="identity",fill="steelblue") +theme(axis.text.x = element_text(size = 20,angle=90))   
  
  bp <- data.frame(ASSET=c("글로벌주식","미국가치","한국","글로벌채권","미국단기채","미국IG","국내채권","글로벌부동산","글로벌인프라","원자재"),WEIGHT=c(23.5,2,7.9,12.1,2,2,32.3,3.7,11.7,3.7))%>% 
    melt(id.vars="ASSET")%>%ggplot(aes("", value, col = ASSET,fill=ASSET)) +             
    geom_bar(width = 1, stat = "identity")+
    theme(legend.text = element_text(size=15))
  
  pie <- bp + coord_polar("y", start=0)+
    theme(legend.text = element_text(size=15))
  
  grid.arrange(pie,bar, 
               ncol = 2, nrow = 1)
})

output$mp3 <- renderPlot({
  
  tmp <- (RAWDATA[,c("STD_DT","WORLD","USVALUE","MSKR","WRBOND","KRBOND","USSHORT","USIG","WREPRA","WRINFRA","GSCI","HMACRO","EMBOND","WRHY")]%>%trans_rt("month")%>%dt_trans%>%
            filter(STD_DT >= input$MP[1] & STD_DT <= input$MP[2])%>%
            mutate(MP=(23.5*WORLD+2*USVALUE+7.9*MSKR+12.1*WRBOND+2*USSHORT+2*USIG+32.3*KRBOND+3.7*WREPRA+11.7*WRINFRA+3.7*GSCI)/100))%>%mutate(BM=WORLD*0.5+0.3*KRBOND+0.2*WRBOND)%>%select(STD_DT,MP,BM)
  
  
  tmp%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, fill = variable)) +  
    geom_bar(position="dodge", stat="identity")+
    theme(legend.text = element_text(size=15))
  
})
output$mp4 <- renderDT({
  ret<- RAWDATA%>%trans_rt("month")%>%dt_trans
  WEI <- data.frame(STD_DT=c("2023-02-01","2023-03-01","2023-04-01"),
                    WORLD= c(15.5,15.5,15.5),
                    MSEX = c(7,7,5),
                    USVALUE = c(2,2,2),
                    MSKR = c(7.9,7.9,7.9),
                    WRBOND = c(12.1,12.1,12.1),
                    USSHORT = c(2,2,2),
                    USIG = c(2,2,2),
                    KRBOND = c(32.3,32.3,30.3),
                    WREPRA = c(3.7,3.7,3.7),
                    WRINFRA = c(11.7,11.7,9.7),
                    GSCI = c(2,2,2))
  
  TEMP <- WEI%>%mutate(STD_DT=(as.Date(STD_DT)-days(1))) %>% melt(id.vars="STD_DT") %>% left_join(ret %>% melt(id.vars="STD_DT"),by=c("STD_DT","variable"))
  colnames(TEMP)[3:4]<-c("weight","value")
  #TEMP%>%filter(STD_DT>"2023-03-22")%>%mutate(ret=weight*value)%>%select(ret)%>%sum
  tmp<- TEMP %>% mutate(rt=weight/100*value)%>%dcast(STD_DT~variable,value.var = "rt")%>%.[,-1]%>%apply(1,sum)
  TEMP2<- data.frame(STD_DT=TEMP$STD_DT%>%unique%>%as.Date(),tmp)%>%as.data.frame
  colnames(TEMP2)[2]<-c("MP")
  
  
  RES<- ret%>%mutate(BM=WORLD*0.5+0.4*WRBOND+0.1*(WRINFRA+WREPRA+GSCI)/3)%>%select(STD_DT,BM)%>%left_join(TEMP2,by="STD_DT")%>%na.omit%>%mutate(ER=MP-BM)
  RES
})

###########Factor###########################################
output$MF1<- renderPlot({
  TMP2<- retm%>%select(STD_DT,GROWTH,INF,RINTEREST,CREDIT,FX) %>%  filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])%>%trans_rt2

    (100*cumprod(1+TMP2))%>%dt_trans%>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
    geom_line(size=1)+ggtitle("누적수익률") +
    theme(legend.text = element_text(size=15))
  

})

output$MF2<- renderPlot({
  
  TMP <- MACRO %>% filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])
  

  coeff <- cbind(ftmacro(input$FA[1],"WORLD"  ),
                 ftmacro(input$FA[1],"MSUS"   ),
                 ftmacro(input$FA[1],"MSKR"   ),
                 ftmacro(input$FA[1],"WRBOND" ),
                 ftmacro(input$FA[1],"KRBOND" ),
                 ftmacro(input$FA[1],"WREPRA" ),
                 ftmacro(input$FA[1],"WRINFRA"),
                 ftmacro(input$FA[1],"CRBTR"  ),
                 ftmacro(input$FA[1],"USGOVT" ),
                 ftmacro(input$FA[1],"GSCI" )
                 )
  
  colnames(coeff) <- c("WORLD","MSUS","MSKR","WRBOND","KRBOND","WREPRA","WRINFRA","CRBTR","USGOVT","GSCI")
  
    g1 <- coeff%>%melt%>%filter(Var2=="WORLD")%>%ggplot(aes(Var1, value,fill=Var2)) +  
    geom_bar(position="dodge", stat="identity")+
    theme(legend.text = element_text(size=15))
  
    g2 <- coeff%>%melt%>%filter(Var2=="MSUS")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g3 <-coeff%>%melt%>%filter(Var2=="MSKR")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g4 <- coeff%>%melt%>%filter(Var2=="WRBOND")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g5 <- coeff%>%melt%>%filter(Var2=="KRBOND")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    
    g6 <- coeff%>%melt%>%filter(Var2=="WREPRA")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g7 <- coeff%>%melt%>%filter(Var2=="WRINFRA")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    
    g8 <- coeff%>%melt%>%filter(Var2=="CRBTR")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g9 <- coeff%>%melt%>%filter(Var2=="USGOVT")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    g10 <- coeff%>%melt%>%filter(Var2=="GSCI")%>%ggplot(aes(Var1, value,fill=Var2)) +  
      geom_bar(position="dodge", stat="identity")+
      theme(legend.text = element_text(size=15))
    
    
    grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,ncol=5)
})


output$MF3 <- renderPlot({
  TMP2<- retm %>%  filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])%>%select(STD_DT,GROWTH,INF,RINTEREST,CREDIT,FX)
  
  TMP2[,-1]%>%cor%>% ggcorrplot( type='upper',hc.order = "TRUE",lab=TRUE)+
    theme(legend.text = element_text(size=15))
  
})
output$MF4 <- renderPlot({
  TMP <- retm %>% filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])
  coeff <- ftmacro(TMP)
  tmp  <- rbind((coeff[1,]*0.6 +coeff[3,]*0.3+coeff[3,]*0.2), 
        (coeff[1,]*0.225 + coeff[2,]*0.079 + coeff[3,]*0.201 + coeff[4,]*0.303 + coeff[6,]*0.037 + coeff[5,]*0.117+coeff[7,]*0.058))
  rownames(tmp)<-c("BM","MP")
  
    tmp%>%melt()%>%ggplot(aes(x=Var2, y=value, col = Var1,fill=Var1)) +             
    geom_bar(width = 1, stat = "identity",position = "dodge")+
    theme(legend.text = element_text(size=15))
})

output$MF5<- renderDT({
  rbind((retm%>%filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])%>%select(GROWTH,INF,RINTEREST,CREDIT,FX)%>%apply(2,mean)*12)%>%round(3),
  (retm%>%filter(STD_DT >= input$FA[1] & STD_DT <= input$FA[2])%>%select(GROWTH,INF,RINTEREST,CREDIT,FX)%>%apply(2,sd)*12^0.5)%>%round(3))
  
})

}
retm%>%select(DXY,EMBOND,EM)%>%na.omit%>%cor
#     
#TMP<- RAWDATA%>%select(STD_DT,WRBOND,EMBOND,DXY)%>%na.omit%>%mutate(RATIO=WRBOND/EMBOND)%>%select(STD_DT,RATIO,DXY)
# TMP2<- retm %>%select(STD_DT,GROWTH,INF,RINTEREST,CREDIT,FX)
# 
# corr<- TMP2[,-1]%>%cov
# #
# rtvol <- rbind((retm%>%select(GROWTH,INF,RINTEREST,CREDIT,FX)%>%apply(2,mean)*12)%>%round(3),
#       (retm%>%select(GROWTH,INF,RINTEREST,CREDIT,FX)%>%apply(2,sd)*12^0.5)%>%round(3))
#    write.xlsx(rtvol ,"c:/work/MONTHLY.xlsx", sheetName="eq",append=F)
#    write.xlsx(corr ,"c:/work/MONTHLY.xlsx", sheetName="eqeps",append=T)
# eqeps <- data.frame(bind_rows(retfeps_wek,retfeps_mon,retfeps_ytd,retfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
# eq <-data.frame(bind_rows(reteq_wek,reteq_mon,reteq_ytd,reteq_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
# fi <- data.frame(bind_rows(retfi_wek,retfi_mon,retfi_ytd,retfi_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
# ai <- data.frame(bind_rows(retai_wek,retai_mon,retai_ytd,retai_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
# st <- data.frame(bind_rows(retst_wek,retst_mon,retst_ytd,retst_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)
# steps <- data.frame(bind_rows(retstfeps_wek,retstfeps_mon,retstfeps_ytd,retstfeps_yea))%>%mutate(INDEX=c("1WEEK(%)","1MONTH(%)","YTD(%)","1 YEAR(%)"))%>%relocate(INDEX)





  # RES<- ret%>%mutate(BM=WORLD*0.5+0.4*WRBOND+0.1*(WRINFRA+WREPRA+GSCI)/3)%>%select(STD_DT,BM)%>%left_join(TEMP2,by="STD_DT")%>%na.omit%>%mutate(ER=MP-BM)
  # RES
# 
#   tmp  <- RAWDATA%>%select(STD_DT,WORLD,WRBOND,SP500,USGOVT) %>% trans_rt("month")%>%dt_trans%>%na.omit
#   ROLL3Y <- data.table(STD_DT=tmp$STD_DT,ROLLIMG3YEAR =roll_cor(tmp$WORLD, tmp$WRBOND, width = 36),ROLLIMG5YEAR2 =roll_cor(tmp$SP500, tmp$USGOVT, width = 36))%>%na.omit
# 
#   tmp  <- RAWDATA%>%select(STD_DT,WORLD,WRBOND,SP500,USGOVT) %>%trans_rt("week")%>%dt_trans%>%na.omit
#   ROLL14W <- data.table(STD_DT=tmp$STD_DT,ROLL4WEEK = roll_cor(tmp$WORLD, tmp$WRBOND, width = 14),ROLL4WEEK = roll_cor(tmp$SP500, tmp$USGOVT, width = 14))
#   
# 
#   
#   write.xlsx(eq ,"c:/work/MONTHLY.xlsx", sheetName="eq",append=F)
#   write.xlsx(eqeps ,"c:/work/MONTHLY.xlsx", sheetName="eqeps",append=T)
#   write.xlsx(fi ,"c:/work/MONTHLY.xlsx", sheetName="fi",append=T)
#   write.xlsx(ai ,"c:/work/MONTHLY.xlsx", sheetName="ai",append=T)
#   write.xlsx(st ,"c:/work/MONTHLY.xlsx", sheetName="st",append=T)
#   write.xlsx(steps ,"c:/work/MONTHLY.xlsx", sheetName="steps",append=T)
#   write.xlsx(ROLL3Y ,"c:/work/MONTHLY.xlsx", sheetName="ROLL3Y",append=T)
#   write.xlsx(ROLL14W ,"c:/work/MONTHLY.xlsx", sheetName="ROLL14W",append=T)
#   write.xlsx(CLI ,"c:/work/MONTHLY.xlsx", sheetName="CLI",append=T)
  # write.xlsx(MP ,"c:/work/MONTHLY.xlsx", sheetName="MP",append=T)
  # write.xlsx(RES ,"c:/work/MONTHLY.xlsx", sheetName="RES",append=T)
   
 #   RAWDATA%>%select(STD_DT,KR2Y,US2Y,USBR,KRBR,DXY,USDKRW)%>%mutate(SP1=US2Y-KR2Y)%>%mutate(SP2=USBR-KRBR)%>%reshape2::melt(id.vars="STD_DT")%>%na.omit%>%ggplot(aes(x=STD_DT, y=value, group = variable))+ 
 #     geom_line(alpha = 1, aes(col=(variable))) 
 #   
 #   macro<- RAWDATA%>%select(STD_DT,KR2Y,US2Y,USBR,KRBR,DXY,USDKRW)%>%mutate(SP2=US2Y-KR2Y)%>%mutate(SP1=USBR)
 #   macro<- macro%>%select(STD_DT,SP1,USDKRW)
 #   sec <- with(macro, train_sec(SP1, USDKRW))
 #   macro%>%select(STD_DT,SP1, USDKRW)%>%na.omit %>%
 #     ggplot(aes(STD_DT)) +
 #     geom_line(aes(y = SP1), colour = "blue") +
 #     geom_line(aes(y = sec$fwd(USDKRW)), colour = "red") +
 #     scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "IGSP"))+
 #     theme(
 #       axis.title.y = element_text(color =  "blue", size=13),
 #       axis.title.y.right = element_text(color = "red", size=13)
 #     )
 #   
 # 
 #   
 #   coeff <- cbind(ftmacro(as.Date("2023-01-01"),"WORLD"  ),
 #                  ftmacro(as.Date("2023-01-01"),"DM"   ),
 #                  ftmacro(as.Date("2023-01-01"),"KOSPI"   ),
 #                  ftmacro(as.Date("2023-01-01"),"EM" )
 #            )
 #   
 #   
 #   colnames(coeff) <- c("WORLD","DM","KOSPI","EM")
 #   
 #   g1 <- coeff%>%melt%>%filter(X2=="WORLD")%>%ggplot(aes(X1, value,fill=X2)) +  
 #     geom_bar(position="dodge", stat="identity")+
 #     theme(legend.text = element_text(size=15))
 #   g2 <- coeff%>%melt%>%filter(X2=="DM")%>%ggplot(aes(X1, value,fill=X2)) +  
 #     geom_bar(position="dodge", stat="identity")+
 #     theme(legend.text = element_text(size=15))
 #   g3 <-coeff%>%melt%>%filter(X2=="KOSPI")%>%ggplot(aes(X1, value,fill=X2)) +  
 #     geom_bar(position="dodge", stat="identity")+
 #     theme(legend.text = element_text(size=15))
 #   g4 <- coeff%>%melt%>%filter(X2=="EM")%>%ggplot(aes(X1, value,fill=X2)) +  
 #     geom_bar(position="dodge", stat="identity")+
 #     theme(legend.text = element_text(size=15))
 #   TMP2<- retm %>%  filter(STD_DT >= "2010-01-01")%>%select(WORLD,DM,EM,KOSPI,DXY)%>%cor
 #   
 #   retm %>%  filter(STD_DT >= "2010-01-01")%>%select(WORLD,KOSPI)%>%mutate(KOSPI_L=lag(KOSPI))%>%na.omit%>%cor
 #   
 #   CLI
 #   
 #   
 #   grid.arrange(g1,g2,g3,g4,ncol=2)
 #   
 # RES1 <- RAWDATA %>% select(STD_DT,KOSPI,DXY,DM,EM) %>% trans_rt("year") %>% round(4)%>%dt_trans %>% mutate(DIFF=DM-EM)
 #   
 # RES2 <-  rollcor("EM","KOSPI","2000-01-01",60,23)%>%left_join(rollcor("SHANGHAI","KOSPI","2000-01-01",60,23),by="STD_DT")
 #   
 # RES3 <-  rollcor("DM","EM","2000-01-01",60,23)%>%left_join(rollcor("DM","KOSPI","2000-01-01",60,23),by="STD_DT")
 #   
 #   write.xlsx(RES1 ,"c:/work/MONTHLY.xlsx", sheetName="eq",append=F)
 #   write.xlsx(RES2 ,"c:/work/MONTHLY.xlsx", sheetName="eqeps",append=T)
 #   write.xlsx(RES3 ,"c:/work/MONTHLY.xlsx", sheetName="fi",append=T)
 #   write.xlsx(coeff ,"c:/work/MONTHLY.xlsx", sheetName="ai",append=T)
 #   write.xlsx(st ,"c:/work/MONTHLY.xlsx", sheetName="st",append=T)