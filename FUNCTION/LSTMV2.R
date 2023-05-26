
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("keras","tensorflow", "shinydashboard","reticulate")
ipak(pkg)

#lstm(data,8,0.75,200,300)
#여자 300
#lstm(data,8,0.75,200,250)
lstm <- function(lag,ratio,epoch,unit,BASEKR) {
  TMP <- list()
  create_sequences <- function(data, seq_length) {
    xs <- ys <- list()
    for (i in seq(seq_length + 1, length(data))) {  # add 1 to seq_length to create longer input sequence
      x <- data[(i-seq_length):(i-1)]
      y <- data[i]
      xs[[i]] <- x
      ys[[i]] <- y
    }
    return(list(xs = unlist(xs), ys = unlist(ys)))
  }
    
ages.fit <- 20:70
# lstm(8,0.70,110,400,BASEKRF)
 # lag=5
 # ratio=0.65
 # epoch=100
 # unit=300
 #  BASEKR=BASEKRF
LC <- lc(link = "logit")
LCfit  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit)

kt <- LCfit$kt%>% as.numeric
data = diff(kt, differences = 1)
#Scale data
scale_factors <- c(mean(data), sd(data))
data <- (data - scale_factors[1]) / scale_factors[2]
data <- data%>%c%>%as.data.frame()
# Create training and testing data
train_size <- floor(ratio * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size+1):nrow(data), ]

# Create sequences for input and output
seq_length <- lag  # number of time steps to use for each input/output sequence
train_seq <-create_sequences(train_data, seq_length)
test_seq <- create_sequences(test_data, seq_length)

# Reshape data for LSTM model
input_shape <- c(seq_length, 1)  # input shape for LSTM model
x_train <- array(train_seq$xs, dim = c(length(train_seq$xs)/seq_length, seq_length, 1))
y_train <- array(train_seq$ys, dim = c(length(train_seq$ys), 1))
x_test <- array(test_seq$xs, dim = c(length(test_seq$xs)/seq_length, seq_length, 1))
y_test <- array(test_seq$ys, dim = c(length(test_seq$ys), 1))
# train_data <- sin(seq(0, 10*pi, length.out = 1000))
# test_data <- sin(seq(10*pi, 12*pi, length.out = 200))
# 

# Define hyperparameters
epochs <- epoch
batch_size <- 1

# Define LSTM model
model <- keras::keras_model_sequential()
model %>%
  layer_lstm(units = unit, input_shape = c(seq_length, 1)) %>%
  layer_dense(units = 1)

# Compile model
model %>% compile(
  loss = 'mse',
  optimizer = optimizer_adam(lr = 0.001)
)
# 
# 
# model %>% compile(
#   loss = 'mean_squared_error',
#   optimizer = "nadam",
#   metrics = c('accuracy')
# )

# 
# 
# history <- model %>% keras::fit(
#   x_train, y_train,
#   batch_size = batch_size,
#   epochs = epochs,verbose=1
# )
# 
# Train model
history <- model %>% keras::fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,verbose=1,
  validation_data = list(x_test, y_test))



# for(i in 1:Epochs ){
#   model %>% keras::fit(x_train, y_train, epochs=1, batch_size = batch_size, verbose=1, shuffle=FALSE)
#   model %>% reset_states()
# }

# Plot training and validation loss
plot(history)

# Generate predictions on test data
y_pred <- model %>% predict(x_test)
nn <- y_test%>%length
nnn<-length(kt)-nn
kt[c(nnn:length(kt))]
kt[c(1:nnn)]
fit <-auto.arima(kt[c(1:nnn)])
#fit <- Arima(kt[1:(N-lag+1)], order=c(1,1,1))
#drift<- kt%>%diff%>%mean
y_test <- y_test *  scale_factors[2] + scale_factors[1]
y_pred <- y_pred *  scale_factors[2] + scale_factors[1]
fit <- fit%>%forecast(h = nn)
arima <- fit$mean
real  <- kt[(nnn)]+cumsum(y_test)
lstm  <- kt[(nnn)]+cumsum(y_pred)


tmp1  <- (sum((arima - real)^2)/nn)^0.5
tmp2  <- (sum((lstm - real)^2)/nn)^0.5

mse   <-  rbind(tmp1,tmp2)
res <- cbind(STD_DT=c(1970:2021),
  arima=c(kt[c(1:nnn)],arima),
  real=c(kt[c(1:nnn)],real),
  lstm=c(kt[c(1:nnn)],lstm)
)
LCfit$kt
d_est  <- exp((LCfit$ax+LCfit$bx%*%arima))
d_est2 <- exp((LCfit$ax+LCfit$bx%*%real))
d_est3 <- exp((LCfit$ax+LCfit$bx%*%lstm))
MOR<- (BASEKR$Dxt/BASEKR$Ext)%>%.[,c(1:nnn)]



res2 <- cbind(STD_DT=c(1970:2021),arima=c(MOR[45,],d_est[25,]),
              real=c(MOR[45,],d_est2[25,]),
              #real=d_est2%>%.[25,],
              lstm=c(MOR[45,],d_est3[25,])
              
              #MOR=(BASEKR$Dxt/BASEKR$Ext)%>%.[45,]
              )



TMP$mse <- mse
TMP$y_pred <- y_pred
TMP$y_test <- y_test
TMP$arima  <- arima
TMP$res  <- res
TMP$res2  <- res2
# TMP3<- (TMP$res2)%>%as.data.frame
# colnames(TMP3)[1]<-"STD_DT"
# TMP3[-c(1:30),]%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
#   geom_line()

return(TMP)
}
TMP <- lstm(5,0.7,150,350,BASEKRM)
TMP <- lstm(3,0.65,150,350,BASEKRM)
#TOTAL: - lstm(5,0.75,200,300,BASEKR)
#FEMALE TMP <- lstm(5,0.65,150,350,BASEKRF)
  plot(TMP$y_test, type = 'l', col = 'red')
lines(TMP$y_pred, col = 'blue')
legend('topright', legend = c('Actual', 'Predicted'), col = c('red', 'blue'), lty = 1)

TMP2<- (TMP$res)%>%as.data.frame
colnames(TMP2)[1]<-"STD_DT"

g1<-TMP2[]%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line()
-c(1:30)

TMP3<- (TMP$res2)%>%as.data.frame
colnames(TMP3)[1]<-"STD_DT"

g2 <-TMP3[]%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line()

TMP$mse

grid.arrange(g1,g2, 
             ncol = 2, nrow = 1)
g2<- lckt%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line()+ggtitle("KAPPA") +
  
  theme(legend.text = element_text(size=15))
res%>%as.data.frame%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line()
res2%>%as.data.frame%>%melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line()
(BASEKRM$Dxt/BASEKRM$Ext)%>%.[45,]%>%plot(type="l")
(BASEKRF$Dxt/BASEKRF$Ext)%>%.[45,]%>%plot(type="l")
(BASEKR$Dxt/BASEKR$Ext)%>%.[45,]%>%plot(type="l")
