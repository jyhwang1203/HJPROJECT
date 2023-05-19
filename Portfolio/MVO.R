
  MVO  <- function(mu,cov,trate,n,wl){
    
  objective_mvo = function(w) {
    obj = (t(w) %*% cov %*% w)^0.5 - t(w) %*% t(mu)
    return(obj)
  }
  
  
  heq.objective = function(w) {
    
    sum <- numeric(2)
    sum[1] = sum(w)-1
   sum[2] = t(w) %*%t(mu) - trate
    return( sum )
  }

  hin.objective <- function(w) {
    h <- numeric(n)
    {if(n==2){
    h[1 ] <- w[1]
    h[2 ] <- w[2]
    }
    else if(n==3){
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
    }
    else if(n==4){
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
    }
    else if(n==5){
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
    }
    else if(n==6){
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
      h[6 ] <- w[6] 
    }
    else if(n==7){
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
      h[6 ] <- w[6] 
      h[7 ] <- w[7]
      
    }}
    
    
    sigma <-  (t(w) %*% cov %*% w)^0.5
    
    # set confidence level and time horizon
    conf_level <- 0.95 # 95% confidence level
    time_horizon <- 1 # 1 day time horizon
    
    # calculate parametric VaR using normal distribution
    h[n+1] <-  - qnorm(1 - conf_level) * sigma * sqrt(time_horizon) - (t(w) %*% t(mu)) * time_horizon  - 0.01
   
     
    return(h)
  }
  # w=x0
  # n=2
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(wl, n),
                   upper = rep(1, n))
  
 
  #결과값
  
  result$par %>%round(4) 
  
  
  }
  mu <- matrix(c(0.06,0.02,0.03),1,3)
    cov <- retm %>%select(WORLD,WRBOND,GSCI)%>%cov       
  MVO(mu,cov,0.05,3,0.01)
 
  
    
  