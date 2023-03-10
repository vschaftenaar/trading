# functions

get.data <- function(coin,value,sell,buy,x){
  
  # download days 1-2000
  lnk <- paste0('https://min-api.cryptocompare.com/data/v2/histoday?fsym=',coin,'&tsym=',value,'&limit=2000' )
  options(warn=-1)
  tmp <- jsonlite::fromJSON(readLines(lnk),simplifyDataFrame=T)$Data$Data
  options(warn=0)
  
  # downloaddays 2000-4000
  toTs <- min(tmp$time)
  lnk <- paste0('https://min-api.cryptocompare.com/data/v2/histoday?fsym=',coin,'&tsym=',value,'&limit=2000&toTs=',toTs)
  options(warn=-1)
  tmp.new <- jsonlite::fromJSON(readLines(lnk),simplifyDataFrame=T)$Data$Data
  options(warn=0)
  
  tmp <- rbind(tmp,tmp.new)
  tmp$dt <- as.POSIXct(tmp$time,origin='1970-01-01')
  tmp <- tmp[tmp$close>0 & as.Date(tmp$dt)>as.Date('2017-01-01'),]
  tmp <- data.table::as.data.table(tmp[order(tmp$dt),])
  
  # add local Low and High
  tmp <- cbind(tmp,localHighLow(tmp$close,60))
  
  # add indicators
  tmp[,pair:=paste0(coin,'_',value)]
  
  tmp[,prc.change:=close/open]
  tmp[,volatility:=high /low]
  
  tmp[,ema01 := TTR::EMA(close,10)]
  tmp[,ema02 := TTR::EMA(close,50)]
  tmp[,ema03 := TTR::EMA(close,100)]
  tmp[,ema04 := TTR::EMA(close,200)]
  
  tmp[,ema01.close := ema01/close]
  tmp[,ema02.close := ema02/close]
  tmp[,ema03.close := ema03/close]
  tmp[,ema04.close := ema04/close]
  
  tmp[,ema04.ema1 := ema04/ema01]
  tmp[,ema04.ema2 := ema04/ema02]
  tmp[,ema04.ema3 := ema04/ema03]
  
  tmp[,ema01.vol := TTR::EMA(volumeto,7)]
  tmp[,ema02.vol := TTR::EMA(volumeto,14)]
  tmp[,ema03.vol := TTR::EMA(volumeto,21)]
  tmp[,ema04.vol := TTR::EMA(volumeto,28)]
  
  tmp[,ema04.ema1.vol := ema04.vol/ema01.vol]
  tmp[,ema04.ema2.vol := ema04.vol/ema02.vol]
  tmp[,ema04.ema3.vol := ema04.vol/ema03.vol]
  
  tmp[,ema01.current.vol := ema01.vol/volumeto]
  tmp[,ema02.current.vol := ema02.vol/volumeto]
  tmp[,ema03.current.vol := ema03.vol/volumeto]
  tmp[,ema04.current.vol := ema04.vol/volumeto]
  
  tmp[,rsi01 := TTR::RSI(close,7)]
  tmp[,rsi02 := TTR::RSI(close,14)]
  tmp[,rsi03 := TTR::RSI(close,21)]
  tmp[,rsi04 := TTR::RSI(close,28)]
  tmp[,rsi05 := TTR::RSI(close,35)]
  tmp[,rsi06 := TTR::RSI(close,42)]
  
  tmp <- tmp[!is.na(ema04)]
  
  nSplit <- ceiling(.80*nrow(tmp))
   
  model.data <- list(
     xgb.train  = head(tmp[,..x], nSplit)
    ,xgb.test   = tail(tmp[,..x], -nSplit)
    ,buy.train  = head(tmp[,..buy], nSplit)
    ,sell.train = head(tmp[,..sell],nSplit)
    ,pred       = tail(tmp,-nSplit)
  )
  
  
  out <- list(
    data        = tmp
    ,model.data = model.data)
  
  return(out)
}

localHighLow <- function(close,window=28){
  
  tmp <- data.table::data.table(close)
  
  tmp$localLow  <- 0
  tmp$localHigh <- 0
  
  for(i in (window+1):nrow(tmp)){
    
    localLow  <- min(tmp$close[(i-window):(i-1)])
    localHigh <- max(tmp$close[(i-window):(i-1)])
    
    if(tmp$close[i]<localLow){
      tmp$localLow[(i-window):(i-1)] <- 0
      tmp$localLow[i] <- tmp$close[i]
    }
    
    if(tmp$close[i]>localHigh){
      tmp$localHigh[(i-window):(i-1)] <- 0
      tmp$localHigh[i] <- tmp$close[i]
    }
  }
  
  tmp[,buy :=ifelse(localLow> 0,1,0)]
  tmp[,sell:=ifelse(localHigh>0,1,0)]
  return(tmp[,c('buy','sell')])
}

plt.result <- function(plot.pair='btc_usd'){
  plot(pred[pair==plot.pair,close]~ pred[pair==plot.pair,dt],type='l',frame=F,lwd=1.5,log='y',xlab='',ylab='',main=gsub('_',' / ',plot.pair))
  par(new=T)
  barplot(pred[pair==plot.pair,sell_pred],col='#D35F8060',xlab='',ylab='',axes=F,border='#D35F8060',ylim=c(0,1))
  axis(4)
  par(new=T)
  barplot(pred[pair==plot.pair,buy_pred] ,col='#78b59460',xlab='',ylab='',axes=F,border='#78b59460',ylim=c(0,1))
}
