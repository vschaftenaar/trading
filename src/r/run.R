options(scipen = 999)
library(shiny)
source('./src/r/script/func.r')


db <- list()
pairs <- as.vector(unlist(data.table::fread(file='./data/pairs.txt',header = F)))



# prepare data ------------------------------------------------------------

sell <- 'sell'
buy  <- 'buy'
x <- c(
   'prc.change','volatility'
  ,'ema01.close','ema02.close','ema03.close','ema04.close'
  ,'ema04.ema1','ema04.ema2','ema04.ema3'
  ,'ema04.ema1.vol','ema04.ema2.vol','ema04.ema3.vol'
  ,'ema01.current.vol','ema02.current.vol','ema03.current.vol','ema04.current.vol'
  ,'rsi01','rsi02','rsi03','rsi04','rsi05','rsi06'
)

xgb.train  <- data.table::data.table()
xgb.test   <- data.table::data.table()
buy.train  <- data.table::data.table()
sell.train <- data.table::data.table()
pred       <- data.table::data.table()

for(i in pairs){
  message(i)
  coin    <- substr(i,1,gregexpr('_',i)[[1]]-1)
  value   <- substr(i,gregexpr('_',i)[[1]]+1,99)
  
  db[[i]] <- get.data(coin,value,sell,buy,x)
  
  xgb.train  <- rbind(xgb.train,db[[i]]$model.data$xgb.train)
  xgb.test   <- rbind(xgb.test,db[[i]]$model.data$xgb.test)
  buy.train  <- rbind(buy.train,db[[i]]$model.data$buy.train)
  sell.train <- rbind(sell.train,db[[i]]$model.data$sell.train)
  pred       <- rbind(pred,db[[i]]$model.data$pred)
}


xgbModel <- list()
xgbModel$buy <- xgboost::xgboost(
  data        = as.matrix(xgb.train),
  label       = as.matrix(buy.train),
  max.depth   = 2000,
  eta         = .3,
  nthread     = 16,
  nrounds     = 250,
  objective   = "binary:logistic",
  verbose     = 0,
  eval_metric = 'logloss'
)

xgbModel$sell <- xgboost::xgboost(
  data        = as.matrix(xgb.train),
  label       = as.matrix(sell.train),
  max.depth   = 2000,
  eta         = .3,
  nthread     = 16, 
  nrounds     = 250,
  objective   = "binary:logistic",
  verbose     = 0,
  eval_metric = 'logloss'
)

pred$buy_pred   <-  predict(xgbModel$buy,  as.matrix(xgb.test))
pred$sell_pred  <-  predict(xgbModel$sell, as.matrix(xgb.test))


ipPort=sample(c(1000:65535))[1]

ip <- '127.0.0.1' #local host

# run app -----------------------------------------------------------------
runApp(appDir = './src/r/script/',port = ipPort,launch.browser = shell(paste0('start msedge --app=http://',ip,':',ipPort)))
