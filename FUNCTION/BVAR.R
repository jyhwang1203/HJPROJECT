  ret <- RAWDATA%>%
  filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSKR"|
         variable=="KRBOND"|variable=="WREPRA"|variable=="WTI"|variable=="GOLD")%>%dcast(STD_DT~variable)%>%na.omit%>%
  trans_rt("month")%>%dt_trans
 ret  <- RAWDATA %>% filter(variable=="USCPIYOY")%>%na.omit%>%dcast(STD_DT~variable) %>% inner_join(ret,by="STD_DT") %>%mutate(
   USCPIYOY=(USCPIYOY/100)%>%as.numeric()
 )
  
  ret <- ts(ret[,-1],
     start = c(2009, 3), 
     end = c(2023, 4), 
     frequency = 12)
  
  
x <- fred_qd[1:243, c("GDPC1", "PCECC96", "GPDIC1","HOANBS", "GDPCTPI", "FEDFUNDS")]
x <- fred_transform(x, codes = c(4, 4, 4, 4, 4, 1))

plot(ret)

mn <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
      alpha = bv_alpha(mode = 2), var = 1e07)

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)

mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)
ret%>%str
run <- bvar(ret, lags = 5, n_draw = 15000, n_burn = 5000, n_thin = 1, priors = priors, mh = mh, verbose = TRUE)
print(run)
plot(run)

plot(run, type = "dens", vars_response = "WRBOND", vars_impulse = "WRBOND-lag1")

fitted(run, type = "mean")
plot(residuals(run, type = "mean"), vars = c("WORLD", "WRBOND"))

opt_irf <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))
plot(irf(run), area = TRUE,vars_impulse = c("USCPIYOY"),vars_response = c(2:6))

predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))
plot(predict(run), area = TRUE, t_back = 32,vars = c(
  "WORLD", "WRBOND", "MSKR","KRBOND", "USCPIYOY", "GOLD", "WREPRA"))

