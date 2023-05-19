##
  MVO_VOLTARGET  <- function(mu,cov,trate,n,wl,vol){
    vol2<<-vol
   objective_mvo = function(w) {
    obj =  0.5*((t(w) %*% cov %*% w)) -  t(w) %*% t(mu)/12
    return(obj)
  }
  
  
  heq.objective = function(w) {
   
    sum <- numeric(1)
    sum[1] = sum(w)-1
  
    return( sum )
  }

  hin.objective <- function(w) {
    h <- numeric(8)

      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
      h[6 ] <- w[6]
      h[7 ] <-  -(w[3]+w[4]) + 0.5
      h[8 ] <- -((t(w) %*% cov %*% w)*12)^0.5 + vol
    }
    
  
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(wl, n),
                   upper = rep(0.3, n))
  
 
  #결과값
  
 
  names(result$par) <-c("글로벌주식","한국주식","글로벌채권","국내채권","대체투자","원자재")
  result$par %>%round(4) 
  
  }
  mu <- matrix(c(0.06,0.02,0.03),1,3)
  
  cov <- retm %>%select(WORLD,WRBOND,GSCI)%>%cov       
  TEMP       <-        retm  %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)
  mu        <<-  apply(TEMP,2,mean) %>% t%>% as.matrix() * 12 
  mu[1]<- (WRGDP23+WRCPI23)%>%as.numeric()+2
  mu[2]<- (KRGDP23+KRCPI23)%>%as.numeric()+1.51
  mu[3]<-  rt_fiwr23
  # mu[4]<- 0.0267  +SP[3]/100 - 0.012
  mu[4]<-  rt_fikr23
  mu[5] <- (sum(REG5$coefficients[,1] * data.frame(rt_eqwr23,rt_fiwr23))+sum(REG6$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23)))/2
  mu[6]<-  sum(REG7$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23))
  mu <- mu/100
  
  #cov <- retm%>%filter(STD_DT>"2010-01-01") %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cov    
  cov <- covEstimation(retm%>%filter(STD_DT>"2010-01-01")  %>%select(WORLD,MSKR,WRBOND,KRBONDH,WREPRA,WRINFRA,GSCI)%>%as.matrix, control = list(type = 'ewma', lambda = 0.94))
  apply(retm%>%filter(STD_DT>"2014-01-01") %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI),2,sd) * 12^0.5  %>% t 
 
  
  G1<- MVO(mu,cov,trate=0,n=6,wl=0.03,0.12)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 
 G2<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.10)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G3<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.08)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G4<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.06)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G5<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.04)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 grid.arrange(G1,G2,G3,G4,G5,ncol=5)

  mu[1]<- (WRGDP24+WRCPI24)%>%as.numeric()+2
 mu[2]<- (KRGDP24+KRCPI24)%>%as.numeric()+1.51
 mu[3]<- rt_fiwr24
 # mu[4]<- 0.0267  +SP[3]/100 - 0.012
 mu[4]<-  rt_fiwr24
 mu[5] <- (sum(REG5$coefficients[,1] * data.frame(rt_eqwr23,rt_fiwr23))+sum(REG6$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23)))/2
 mu[6]<-  sum(REG7$coefficients[,1]* data.frame(rt_eqwr23,rt_fiwr23))
 mu <- mu/100
 
 cov <- retm%>%filter(STD_DT>"2010-01-01") %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI)%>%cov    
 #cov <- covEstimation(retm %>%select(WORLD,MSKR,WRBOND,KRBONDH,WREPRA,WRINFRA,GSCI)%>%as.matrix, control = list(type = 'ewma', lambda = 0.94))
 apply(retm%>%filter(STD_DT>"2014-01-01") %>%select(WORLD,MSKR,WRBOND,KRBOND,AL,GSCI),2,sd) * 12^0.5  %>% t 
 
 
 G1<- MVO(mu,cov,trate=0,n=6,wl=0.03,0.12)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G2<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.10)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G3<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.08)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G4<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.06)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 G5<- MVO_VOLTARGET(mu,cov,trate=0,n=6,wl=0.03,0.04)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
   ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
 grid.arrange(G1,G2,G3,G4,G5,ncol=5)
 