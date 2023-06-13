
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("nloptr")
ipak(pkg)
MVO <- function(eb=eb,TMP=TMP,lamda=lamda,coeff=coeff,wl=wl,n=n){
  coeff=coeff
  
  omega  <- TMP %>%  select(GROWTH,INF,RINTEREST,CREDIT,FX) %>% cov
  # w<-rep(1/12,12)
  # lamda<- 0.99
  
  objective_mvo = function(w) {
    lamda <- lamda
    #w<-rep(1/7,7)
    #w<-  c(0.7,0,0.3,0,0,0,0)
    obj = (1-lamda)*(t(w)%*%coeff[]-eb)%*%t(t(w)%*%coeff[]-eb)+lamda*(t(w)%*%coeff[,]-eb)%*%omega%*%t(t(w)%*%coeff[]-eb)
    
    return(obj)
  }
  
  #제약조건     
  heq.objective = function(w) {
    sum <- numeric(1)
    sum[1] = sum(w)-1
    # sum[2] = t(w) %*%t(mu) - tr
    return( sum )
  }
  
  hin.objective <- function(w) {
    h <- numeric(13)
    # h[1 ] <- t(w) %*%rep(1/7,7) - 0.05
    
    #  h[13 ] <- -((t(w) %*% omega %*% w)*12)^0.5+0.05
    # h[3] <- (rep(1/7,7))%*%(coeff[c(1:7),-1])
    
    #h[8] <-  w[2]-.1
    
    h[1 ] <- w[1] 
    h[2 ] <- w[2]
    h[3 ] <- w[3]
    h[4 ] <- w[4]
    h[5 ] <- w[5]
    h[6 ] <- w[6]        
    h[7 ] <- w[7]  
    h[8 ] <- w[8] 
    h[9 ] <- w[9] 
    h[10] <- w[10] 
    h[11] <- w[11] 
    h[12] <- w[12]
 h[13] <- w[13]
    # h[13] <- -w[5]
    return(h)
  }
  
  result = slsqp( x0 = rep(1/n,n),
                  fn = objective_mvo,
                  hin = hin.objective,
                  heq = heq.objective,
                  control = list(xtol_rel = 1e-8),
                  lower = rep(wl, n),
                  upper = rep(1, n))
  #결과값
  res2 <- result$par %>%round(4)
  
  
  
  return(res2)
}

s <- which((retm$STD_DT)=="2010-01-31")
n <-(retm%>%filter(STD_DT>"2010-01-01")%>%nrow-1)
tmp <- list()

  res <- t(sapply(c(1:n),function(t){
  #t =5
  STDDT1 <- retm$STD_DT[s+t-1]

  # lamda <- 0.99
  TMP  <-  retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
 # REG1 <-  lm(MSUS ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG1 <-  lm(MSUS ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG2 <-  lm(EM~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG3 <-  lm(MSKR~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary
  REG4 <-  lm(USGOVT ~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG5 <- lm(USIG~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG6 <-  lm(USHY  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG7 <-  lm(KRBONDH ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG8 <-  lm(EMBOND ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG9 <-  lm(WRINFRA ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG10 <-  lm(WREPRA ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG11 <- lm(HFRI  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG12 <- lm(WTI ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG13 <- lm(GOLD~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  
  coeff <<- rbind(REG1$coefficients[-1,1],
                 REG2$coefficients[-1,1],
                 REG3$coefficients[-1,1],
                 REG4$coefficients[-1,1],
                 REG5$coefficients[-1,1],
                 REG6$coefficients[-1,1],
                 REG7$coefficients[-1,1],
                 REG8$coefficients[-1,1],
                 REG9$coefficients[-1,1],
                 REG10$coefficients[-1,1],
                 REG11$coefficients[-1,1],
                 REG12$coefficients[-1,1],
                 REG13$coefficients[-1,1])
 
  # coeff2 <<- rbind(REG1$r.squared,
  #                 REG2$r.squared,
  #                 REG3$r.squared,
  #                 REG4$r.squared,
  #                 REG5$r.squared,
  #                 REG6$r.squared,
  #                 REG7$r.squared,
  #                 REG8$r.squared,
  #                 REG9$r.squared,
  #                 REG10$r.squared,
  #                 REG11$r.squared,
  #                 REG12$r.squared,
  #                 REG13$r.squared)
  # coeff3 <<- rbind( REG1$adj.r.squared,
  #                  REG2$adj.r.squared,
  #                  REG3$adj.r.squared,
  #                  REG4$adj.r.squared,
  #                  REG5$adj.r.squared,
  #                  REG6$adj.r.squared,
  #                  REG7$adj.r.squared,
  #                  REG8$adj.r.squared,
  #                  REG9$adj.r.squared,
  #                 REG10$adj.r.squared,
  #                 REG11$adj.r.squared,
  #                 REG12$adj.r.squared,
  #                 REG13$adj.r.squared)
  # 
  RES <- cbind(coeff,coeff2,coeff3)
  
  rownames(coeff) <-c("MSUS","EM","MSKR","USGOVT","USIG","USHY","KRBONDH","EMBOND","WRINFRA","WREPRA","HFRI","WTI","GOLD")
  TMP2 <-retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
  REG1 <- lm(WORLD~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  REG2 <- lm(MSKR~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  REG3 <- lm(WRBOND~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  REG4 <- lm(KRBONDH~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  REG5 <- lm(AL~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  REG6 <- lm(GSCI~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary 
  
  fcoeff <- rbind(REG1$coefficients[-1,1],
                  REG2$coefficients[-1,1],
                  REG3$coefficients[-1,1],
                  REG4$coefficients[-1,1],
                  REG5$coefficients[-1,1],
                  REG6$coefficients[-1,1]
                  )
eb <<-(fcoeff[1,]*0.6+ fcoeff[3,]*0.4)
#@ eb <<-(fcoeff[1,]*0.5+ fcoeff[3,]*0.3+ fcoeff[5,]*0.2)
  #eb <<-(fcoeff[1,]*0.3+ fcoeff[2,]*0.047+ fcoeff[3,]*0.251+fcoeff[4,]*0.149+fcoeff[5,]*0.223+fcoeff[6,]*0.03)
 #eb <<-(fcoeff[1,]*0.2246+ fcoeff[2,]*0.03+ fcoeff[3,]*0.1+fcoeff[4,]*0.3+fcoeff[5,]*0.3+fcoeff[6,]*0.0454)
  n <- coeff %>% nrow
  wei<- data.frame(as.data.frame(t(MVO(eb,TMP,0.99,coeff,0.02,n))))
  tmp$wei <- cbind(STDDT1,wei)
  colnames(wei) <-c("MSUS","EM","MSKR","USGOVT","USIG","USHY","KRBONDH","EMBOND","WRINFRA","WREPRA","HFRI","WTI","GOLD")
  TMP2 <-retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
  ret2<- retm%>%filter(STD_DT==((STDDT1)))%>%melt(id.vars="STD_DT")
  ret3 <- wei%>%melt%>%left_join(ret2,by="variable")
  ret3<- sum(ret3[,2]*ret3[,4])
  tmp$ret <- data.frame(STDDT1,ret3)
  tmp$exp <- (wei%>%as.matrix)%*%coeff
  tmp$eb <-eb
  return(tmp)
}
))
#do.call(rbind,res[,2])%>%View
RT_MACRO <- do.call(rbind,res[,2])
FACTORRT <- do.call(rbind,res[,3])
wei_macro <- do.call(rbind,res[,1])
colnames(wei_macro)[-1] <-c("선진국","신흥국","한국주식","미국채권","미국하이일드","한국채권","신흥국채권","글로벌인프라","글로벌부동선","헤지펀드","원유","금")
FACTOREXP <- do.call(rbind,res[,4])
RT_MF<- retm%>%select(STD_DT,GROWTH,INF,CREDIT,RINTEREST,FX)%>%cuml
PA_MF<- retm%>%select(STD_DT,GROWTH,INF,CREDIT,RINTEREST,FX)%>%PA
ret3 <- RT_MACRO %>%left_join(retm%>%filter(STD_DT>="2010-01-01")%>%select(STD_DT,BM,BM2),by=c("STDDT1"="STD_DT"))
ret3[,-1] <- ret3[,-1]%>%round(4)
RET_MACRO <- ret3
#ret3 <- cbind(ret2,retm%>%filter(STD_DT>="2010-01-01")%>%select(STD_DT,WRBOND,WORLD,AL)%>%dt_trans%>%mutate(BM=(0.5*WORLD+0.3*WRBOND+0.2*AL))%>%.[-159,6])
colnames(RET_MACRO)<- c("STD_DT","MP","BM","BM2")
RT_MACRO <- data.frame(STD_DT=RET_MACRO$STD_DT,cumprod(1+RET_MACRO[,-1]))


  g1 <- RT_MACRO %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line(size=1)+ 
  ggtitle("누적수익률") +
  theme(legend.text = element_text(size=15))

  TARGETEXP  <- rbind( (wei_macro[  wei_macro%>%nrow,-1]%>%as.matrix)%*%coeff,eb)
  rownames(TARGETEXP)<-c("MP","BM")
  
  g2 <- TARGETEXP%>%reshape2::melt()%>%ggplot(aes(x=Var2, y=value, col = Var1,fill=Var1)) +             
       geom_bar(width = 1, stat = "identity",position = "dodge")+
       theme(legend.text = element_text(size=15))
  

  wei_macro%>%View
  pa_macro <- PA(RET_MACRO)
  
  grid.arrange( g1,g2,ncol=2)

  write.xlsx(RES ,  "c:/work/MACROPORT.xlsx", sheetName="RT_MACRO",append=F)
  write.xlsx(RET_MACRO , "c:/work/MACROPORT.xlsx", sheetName="RET_MACRO",append=T)
  write.xlsx(pa_macro ,  "c:/work/MACROPORT.xlsx", sheetName="pa_macro",append=T)
  write.xlsx(wei_macro , "c:/work/MACROPORT.xlsx", sheetName="wei_macro",append=T)
  write.xlsx(FACTORRT  , "c:/work/MACROPORT.xlsx", sheetName="FACTORRT",append=T)
  write.xlsx(FACTOREXP  ,"c:/work/MACROPORT.xlsx", sheetName="FACTOREXP",append=T)
  write.xlsx(RT_MF  , "c:/work/MACROPORT.xlsx", sheetName="RT_MF",append=T)
  write.xlsx(PA_MF  ,"c:/work/MACROPORT.xlsx", sheetName="PA_MF",append=T)
#MACRO FACTOR


# 
# 
# WEI <- data.frame(STD_DT=c("2023-02-01","2023-03-01","2023-04-01","2023-05-01"),
# WORLD= c(15.5,15.5,15.5,17.5),
# MSEX = c(7,7,5,5),
# USVALUE = c(2,2,2,2),
# MSKR = c(7.9,7.9,7.9,7.9),
# WRBOND = c(12.1,12.1,12.1,10.1),
# USSHORT = c(2,2,2,0),
# USLONG=c(0,0,0,4),
# USIG = c(2,2,2,2),
# USHY = c(0,0,2,2),
# EMBOND = c(0,0,2,2),
# KRBOND = c(32.3,32.3,30.3,24.3),
# WREPRA = c(3.7,3.7,3.7,3.7),
# WRINFRA = c(11.7,11.7,9.7,11.7),
# GSCI = c(3.8,3.8,3.8,3.8),
# HMACRO = c(0,0,2,2))
# 
# 
# TEMP <- WEI%>%mutate(STD_DT=(as.Date(STD_DT)-days(1))) %>% melt(id.vars="STD_DT") %>% left_join(ret %>% melt(id.vars="STD_DT"),by=c("STD_DT","variable"))
# colnames(TEMP)[3:4]<-c("weight","value")
# MP1<- TEMP %>% mutate(rt=weight/100*value)%>%dcast(STD_DT~variable,value.var = "rt")
# MP2<- TEMP %>% mutate(rt=weight/100*value)%>%dcast(STD_DT~variable,value.var = "value")
# 
# MP <- ((1+RAWDATA[,c("STD_DT","WORLD","USVALUE","MSKR","WRBOND","KRBOND","USSHORT","USIG","WREPRA","WRINFRA","GSCI","HMACRO","EMBOND","WRHY","CRBTR")]%>%trans_rt("day")%>%dt_trans%>%
#           filter(STD_DT >= "2022-01-01")%>%mutate(AL=(CRBTR+WREPRA+WRINFRA)/3)%>%
#           mutate(MP=(23.5*WORLD+2*USVALUE+7.9*MSKR+12.1*WRBOND+2*USSHORT+2*USIG+32.3*KRBOND+3.7*WREPRA+11.7*WRINFRA+3.7*GSCI)/100))%>%mutate(BM=WORLD*0.5+0.3*KRBOND+0.2*AL)%>%select(STD_DT,MP,BM)%>%
#          exname%>%trans_rt2()%>%cumprod*100)%>%
#        dt_trans 
# 
# MP3<- cbind(MP1[,-1]%>%apply(1,sum))
# 
# write.xlsx(MP1 ,     "c:/work/MP.xlsx", sheetName="MP1",append=F)
# write.xlsx(MP2 ,  "c:/work/MP.xlsx", sheetName="MP2",append=T)
# write.xlsx(MP ,     "c:/work/MP.xlsx", sheetName="MP",append=T)
# write.xlsx(MP3 ,     "c:/work/MP.xlsx", sheetName="MP3",append=T)
# 
# 
# WEI <- data.frame(STD_DT=c("2023-02-01","2023-03-01"),
#                   WORLD= c(15.5,15.5),
# 
#                   MSKR = c(7.9,7.9),
#                   WRBOND = c(12.1,12.1),
#                   KRBOND = c(32.3,30.3),
#                   WREPRA = c(3.7,3.7),
#                   WRINFRA = c(11.7,9.7),
#                   GSCI = c(2,2))
# 
# 
#   TMP2<- MACRO%>%select(STD_DT,GROWTH,INF,RINTEREST,CREDIT,FX) %>%trans_rt2
#   
#   TMP3<- (cumprod(1+TMP2))%>%dt_trans
#   sig       <<-  apply(TMP2,2,sd) * 12^0.5  %>% t %>%as.matrix
#   
#   res1 <- rbind(
#   (TMP3%>%tail(n=1)%>%.[,-1])^(12/TMP3%>%nrow)%>%as.matrix(),
#   sig)
#   res2 <-  MACRO%>%select(STD_DT,GROWTH,INF,RINTEREST,CREDIT,FX) %>%trans_rt2  
#   coeff
#   fcoeff
#   
#   
#   sig2       <<-  apply(ret2[,-1],2,sd) * 12^0.5  %>% t 
#   (MACROP%>%tail(n=1)%>%.[,-1])^(12/MACROP%>%nrow)
#   res3 <- rbind(
#     (MACROP%>%tail(n=1)%>%.[,-1])^(12/MACROP%>%nrow)%>%as.matrix(),
#     sig2)
#   wei
#   MACROP
#   exp
#   res4 <-  tmp
#   write.xlsx(res1 ,     "c:/work/MACRO.xlsx", sheetName="res1",append=F)
#   write.xlsx(res2 ,  "c:/work/MACRO.xlsx", sheetName="res2",append=T)
#   write.xlsx(coeff ,     "c:/work/MACRO.xlsx", sheetName="coeff",append=T)
#   write.xlsx(fcoeff ,     "c:/work/MACRO.xlsx", sheetName="fcoeff",append=T)
#   write.xlsx(sig2 ,     "c:/work/MACRO.xlsx", sheetName="sig2",append=T)
#   write.xlsx(res3 ,  "c:/work/MACRO.xlsx", sheetName="res3",append=T)
#   write.xlsx(wei , "c:/work/MACRO.xlsx", sheetName="wei",append=T)
#   write.xlsx(MACROP ,"c:/work/MACRO.xlsx", sheetName="MACROP",append=T)
#   write.xlsx(exp ,    "c:/work/MACRO.xlsx", sheetName="exp",append=T)
#   write.xlsx(res4 ,    "c:/work/MACRO.xlsx", sheetName="res4",append=T)
#   write.xlsx(RES ,    "c:/work/MACRO.xlsx", sheetName="RES",append=T)
# 
#   