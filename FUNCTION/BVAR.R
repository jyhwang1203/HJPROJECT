  ret <- RAWDATA%>%
  filter(variable=="WORLDT"|variable=="MSUST"|variable=="MSEUT"|variable=="MSKRT"|variable=="MSJPT"|variable=="MSCNT"|variable=="EMEXCNT"|
         variable=="WRBONDT"|variable=="WRGOVTT"|variable=="WRIGT"|variable=="WRHYT"|
         variable=="SPGST"|variable=="WREPRA"|variable=="WRINFRA")%>%
         dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans

  ret
  
  ret<- RAWDATA %>% filter(variable=="USCPIYOY"|variable=="USGDPQ")%>%na.omit%>%dcast(STD_DT~variable) %>% mutate(USGDPQ=USGDPQ/100)%>%
    inner_join(ret,by="STD_DT") %>%mutate(USCPIYOY=(USCPIYOY/100)%>%as.numeric())
  
  data <- as.xts(ret[,-1],order.by = ret$STD_DT )%>% Return.calculate(method = c("discrete", "log")) %>% apply.monthly(., Return.cumulative)%>%na.omit
  data <- as.xts(ret[,-1],order.by = ret$STD_DT )%>% Return.calculate(method = c("log")) %>% apply.monthly(., Return.cumulative)%>%na.omit
  
  ret <- as.xts(ret[,-1]%>%data.frame,order.by = (ret$STD_DT)%>%as.Date )%>%data.frame
  ################################################### code chunk number 1: preliminaries
  options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
  
##
  
  ################################################### code chunk number 2: setup
  set.seed(42)
  library("BVAR")
  x <- ret
  
  ################################################### code chunk number 4: timeseries
  op <- par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
  par(mfrow = c(4, 3))

  plot(x$STD_DT, x$WORLDT, type = "l", xlab = "Time", ylab = "ACWI")
  plot(x$STD_DT, x$MSUST , type = "l", xlab = "Time", ylab = "USA")
  plot(x$STD_DT, x$MSEUT , type = "l", xlab = "Time", ylab = "EURO")
  plot(x$STD_DT, x$MSJPT , type = "l", xlab = "Time", ylab = "JAPAN")
  plot(x$STD_DT, x$MSCNT , type = "l", xlab = "Time", ylab = "CHINA")
  plot(x$STD_DT, x$MSKRT , type = "l", xlab = "Time", ylab = "KOREA")
  plot(x$STD_DT, x$EMEXCNT, type = "l", xlab = "Time", ylab = "EM(EXCHINA)")
  plot(x$STD_DT, x$WRBONDT, type = "l", xlab = "Time", ylab = "GLOBAL BOND")
  plot(x$STD_DT, x$WRGOVTT, type = "l", xlab = "Time", ylab = "GLOBAL GOVERMENTBOND")
  plot(x$STD_DT, x$WRIGT , type = "l", xlab = "Time", ylab = "GLOBAL IG")
  plot(x$STD_DT, x$WRHYT , type = "l", xlab = "Time", ylab = "GLOBAL HY")
  plot(x$STD_DT, x$SPGST , type = "l", xlab = "Time", ylab = "COMMODITY")
  par(op)
  
  
  ################################################### code chunk number 5: minnesota
  mn <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 1e-04, max = 5), 
                     alpha = bv_alpha(mode = 2), var = 1e+07)
  
  
  ################################################### code chunk number 6: dummies
  soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
  sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)
  
  
  ################################################### code chunk number 7: priors
  priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)
  
  
  ################################################### code chunk number 8: metropolis
  mh <- bv_metropolis(scale_hess = c(0.05, 1e-04, 1e-04), adjust_acc = TRUE, acc_lower = 0.25, 
                      acc_upper = 0.45)
  
  ################################################### code chunk number 9: bvar
  run <- bvar(x, lags = 3, n_draw = 50000, n_burn = 25000, n_thin = 1, priors = priors, 
              mh = mh, verbose = TRUE)
  
  ################################################### code chunk number 10: print
  print(run)
  ################################################### code chunk number 11: trace_density (eval = FALSE) plot(run) plot(run, type =
  ################################################### 'dens', vars_response = 'GDPC1', vars_impulse = 'GDPC1-lag1')
  
  ################################################### code chunk number 12: trace_density
  summary(run)
  plot(run, type = "dens", vars_response = "WORLD", vars_impulse = "WORLD-lag1")
  ################################################### code chunk number 13: betas
  ################################################### code chunk number 14: fitted
  fitted(run, type = "mean")
  
  
  ################################################### code chunk number 15: residuals
  plot(residuals(run, type = "mean"), vars = c("WORLD", "WRBOND"))
  
  
  ################################################### code chunk number 16: irf
  opt_irf <- bv_irf(horizon = 10, identification = TRUE)
  irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))
  
    ################################################### code chunk number 17: irf_cholesky
  plot(irf(run), area = TRUE, vars_impulse = c("USCPIYOY"),vars_response = c("WORLD", "WRBOND","GSCI","WREPRA"))
  plot(irf(run), area = TRUE, vars_impulse = c("USGDPQ"),vars_response = c("WORLD", "WRBOND","GSCI","WREPRA"))

  
  
  
  
  ################################################### code chunk number 18: predict
  predict(run) <- predict(run, horizon = 20, conf_bands = c(0.05, 0.16))
  pred <- predict(run)
  pred$fcast
  RES <- (pred$quants)%>%as.data.frame
  colnames(RES) <- pred$variables
  
  sapply(c(1:12),function(t){
  (sapply(c(1:12),function(i){
    pred$fcast[,i,t] %>% median
  })+1)%>%cumprod
  })
  
####################T
  ############################### code chunk number 19: predict_unconditiona
  plot(predict(run), area = TRUE, t_back = 20,vars = c("WORLDT", "KOSPIT","MSUST","MSEUT","MSJPT","MSCNT","EMEXCNT"))
  plot(predict(run), area = TRUE, t_back = 20,vars = c("WRBONDT", "WRGOVTT","WRIGT","WRHYT"))
  plot(predict(run), area = TRUE, t_back = 20,vars = c("SPGST", "WREPRA","WRINFRA"))
  ret
  
  ################################################### code chunk number 20: app_data
  y <- fred_qd[1:243, c("GDPC1", "GDPCTPI", "FEDFUNDS")]
  z <- fred_transform(y, type = "fred_qd")
  y <- fred_transform(y, codes = c(5, 5, 1), lag = 4)
  
  
  ################################################### code chunk number 21: app_timeseries
  op <- par(mfrow = c(1, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
  plot(as.Date(rownames(y)), y[, "GDPC1"], type = "l", xlab = "Time", ylab = "GDP growth")
  plot(as.Date(rownames(y)), y[, "GDPCTPI"], type = "l", xlab = "Time", ylab = "Inflation")
  plot(as.Date(rownames(y)), y[, "FEDFUNDS"], type = "l", xlab = "Time", ylab = "Federal funds rate")
  par(op)
  
  
  ################################################### code chunk number 22: app_bvar
  priors_app <- bv_priors(mn = bv_mn(b = 0))
  run_app <- bvar(y, lags = 5, n_draw = 50000, n_burn = 25000, priors = priors_app, 
                  mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE), verbose = FALSE)
  
  
  ################################################### code chunk number 23: app_dummies
  add_soc <- function(Y, lags, par) {
    soc <- if (lags == 1) {
      diag(Y[1, ])/par
    } else {
      diag(colMeans(Y[1:lags, ]))/par
    }
    X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))
    return(list(Y = soc, X = X_soc))
  }
  
  
  ################################################### code chunk number 24: app_priors
  soc <- bv_dummy(mode = 1, sd = 1, min = 1e-04, max = 50, fun = add_soc)
  priors_soc <- bv_priors(soc = soc)
  
  
  ################################################### code chunk number 25: app_coda
  library("coda")
  run_mcmc <- as.mcmc(run_app, vars = "lambda")
  geweke.diag(run_mcmc)
  
  
  ################################################### code chunk number 26: app_parallel
  library("parallel")
  n_cores <- 4
  cl <- makeCluster(n_cores)
  
  runs <- par_bvar(cl = cl, data = y, lags = 5, n_draw = 50000, n_burn = 25000, n_thin = 1, 
                   priors = priors_app, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))
  stopCluster(cl)
  
  runs_mcmc <- as.mcmc(runs, vars = "lambda")
  gelman.diag(runs_mcmc, autoburnin = FALSE)
  
  
  ################################################### code chunk number 27: app_chains
  plot(runs, type = "full", vars = "lambda")
  
  
  ################################################### code chunk number 28: app_signs
  sr <- matrix(c(1, 1, 1, -1, 1, NA, -1, -1, 1), ncol = 3)
  opt_signs <- bv_irf(horizon = 16, fevd = TRUE, identification = TRUE, sign_restr = sr)
  print(opt_signs)
  
  
  ################################################### code chunk number 29: app_irf
  irf(run_app) <- irf(run_app, opt_signs)
  
  
  ################################################### code chunk number 30: app_irf_signs
  plot(irf(run_app), vars_impulse = c(1, 3))
  
  
  ################################################### code chunk number 31: app_predict
  path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
  predict(run_app) <- predict(run_app, horizon = 16, cond_path = path, cond_var = "FEDFUNDS")
  
  
  ################################################### code chunk number 32: app_predict_conditional
  plot(predict(run_app), t_back = 16)
  
  
  FALSE
  