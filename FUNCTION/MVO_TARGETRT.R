##
MVO_TARGETRT <- function(mu,cov,trate,n,wl,vol){
  vol2<<-vol
  trate0 <<- trate
  wl2<<- wl
  objective_mvo = function(w) {
    obj =  0.5*((t(w) %*% cov %*% w)) -  t(w) %*% t(mu)
    return(obj)
  }
  
  
  heq.objective = function(w) {
    
    sum <- numeric(2)
    sum[1] = sum(w)-1
    sum[2] = t(w) %*%t(mu) - trate
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
    h[7 ] <-  -w[3]-w[4] + 0.4
    h[8 ] <- -((t(w) %*% cov %*% w))^0.5 + vol
    return( h )
  }
  
  
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(wl, n),
                   upper = rep(0.3, n))
  
  
  #결과값
  
  
  names(result$par) <-c("글로벌주식","한국주식","글로벌채권","한국채권","대체투자","원자재")
  result$par%>%t%>%as.data.frame %>%round(4) 
  
} 


wei1<- MVO_TARGETRT(mu24,12*cov24,trate=0.065,n=6,wl=0.03,0.08)%>%as.matrix()
wei2<- MVO_TARGETRT(mu24,12*cov24,trate=0.062,n=6,wl=0.03,0.08)%>%as.matrix()
wei3<- MVO_TARGETRT(mu24,12*cov24,trate=0.059,n=6,wl=0.03,0.08)%>%as.matrix()
wei4<- MVO_TARGETRT(mu24,12*cov24,trate=0.056,n=6,wl=0.03,0.08)%>%as.matrix()
wei5<- MVO_TARGETRT(mu24,12*cov24,trate=0.053,n=6,wl=0.03,0.08)%>%as.matrix()
wei6<- MVO_TARGETRT(mu24,12*cov24,trate=0.050,n=6,wl=0.03,0.08)%>%as.matrix()

wei1%>%as.data.frame()%>% reshape2::melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+ ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.065)) 

G1TRT <- wei1%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.065)) 
G2TRT <- wei2%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.060)) 
G3TRT <- wei3%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.055)) 
G4TRT <- wei4%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.050)) 
G5TRT <- wei5%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.045)) 
G6TRT <- wei6%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표수익률=",0.040)) 
GR_TARGETRT<- grid.arrange(G1TRT,G2TRT,G3TRT,G4TRT,G5TRT,ncol=5)
GR_TARGETRT
temp <- rbind(
  c((wei1) %*% t(mu24), ((wei1) %*%(12*cov24)%*% t(wei1))^0.5),
  c((wei2) %*% t(mu24), ((wei2) %*%(12*cov24)%*% t(wei2))^0.5),
  c((wei3) %*% t(mu24), ((wei3) %*%(12*cov24)%*% t(wei3))^0.5),
  c((wei4) %*% t(mu24), ((wei4) %*%(12*cov24)%*% t(wei4))^0.5),
  c((wei5) %*% t(mu24), ((wei5) %*%(12*cov24)%*% t(wei5))^0.5),
  c((wei6) %*% t(mu24), ((wei6) %*%(12*cov24)%*% t(wei6))^0.5))%>%data.frame

colnames(temp) <- c("mean","vol")
temp <-temp %>% mutate(SR=(mean-0.02)/vol)%>%round(4)

targetrt <-  cbind(
  target=c("6.5%","6.2%","5.9%","5.6%","5.3%","5%"),
  temp,
  rbind(
  wei1%>%data.frame,
  wei2%>%data.frame,
  wei3%>%data.frame,
  wei4%>%data.frame,
  wei5%>%data.frame,
  wei6%>%data.frame))%>%as.data.frame()
# 
# write.xlsx(targetrt ,"c:/work/MP.xlsx", sheetName="targetrt",append=T)
# 
# 
# 
# 
# 
# targetrt
