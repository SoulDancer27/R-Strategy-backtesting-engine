# UI functions

InputControl <- function(input)
{
  
  # Quit the repeat loop
  if (input=='q' || input =='quit' || input =='stop' || input =='esc') if (is.function(Quit)) return (Quit)
  
  # Change current position
  if (!is.na (suppressWarnings (as.numeric (input)))) if (is.function(Step)) return (Step)
  
  # Trading management
  # contains() -> watch below
  # appropriate formatting is described in ProcessInput()
  if (contains('b',input) || contains('s',input) || contains('c',input))  return (Trade) 
  
}

# Changes the value of quitIndex
Quit <- function()
{
  if (quitIndex==0) quitIndex <<- 1
  if (quitIndex!=0 && quitIndex!=1) 
  {
    print("InputControl -> Quit -> invalid quitIndex value!")
    quitIndex <<- 0
    print('quitIndex was set to 0')
  }
}

# Changes current position
Step <- function()
{
  step <- as.numeric(input)
  cur <- current.position
  new <- cur + step
  # Checking if the updated position is still in the range of data
  if (new>0 && new<= L) {if(is.function(UpdatePosition)) UpdatePosition(new,cur)}
  else print('Incorrect step value, new position is out of the data range.')
}

# Makes all of the activities
UpdatePosition <- function(new, current)
{
  current.position <<- new
  if (is.function(UpdateCapital)) 
  {
    myCapital <<- UpdateCapital(new,current,myCapital)
    marketCapital <<- UpdateCapital(new,current, marketCapital)
  }
  if (is.function(UpdatePlots)) UpdatePlots(current.position, price, plot.length)
  for (i in 1:N) print(paste(prediction[current.position,i],sigma[current.position,i]))
}

# Updates Capital for manual strategy
UpdateCapital <- function(new, current, capital)
{
  result <- capital
  if (new<=forecast.window) { return(result) }
  else 
  {
    if (new < current)
    {
      result[(new+1):L,] <- 0
      return(result)
    }
    
    if (new == current) { return(result) }
    
    performances <- curPrices <- nextPrices <- vector(mode = 'numeric', length = N)
    for (t in (current+1):new) 
    {
      for (i in 1:N)
      {
        curPrices[i] <- price[[i]][t-1]
        nextPrices[i] <- price[[i]][t]
        performances[i] <- nextPrices[i]/curPrices[i]
        result[t,i] <- result[t-1,i]*performances[i]
      }
      result[t,'free'] <- result[t-1,'free']
      result[t,'total'] <- sum(result[t,1:N])+result[t,'free']
    }
    return (result)
  }
}

# Transforms input to a trading command
Trade <- function()
{
  # Returns an array with trade info 
  trade <- ProcessInput(input)
  
  # Makes a trade
  if (!is.na(trade[1])) if (is.function(MakeTrade))
  {
    curWeights <- calculateWeights(myCapital[current.position,])
    weights <- MakeTrade (curWeights, trade)
    print (weights)
    myCapital[current.position,] <<- ManageCapital (myCapital[current.position,],weights)
  }
  print(myCapital[current.position,])
}

# Checks if trade input is according to guidelines
ProcessInput <- function(input)
{
  # Guideline on appropriate formatting
  # b number1 number2 -   buys active number1, for number2 percents of current capital
  # s number1 number2 -  sells active number1, for number2 percents of current capital
  # c number          - closes position in active number
  # c a               - closes all positions
  
  act <- strsplit(input,' ')
  act <- act[[1]]
  # Close conditions
  if (length(act)==2 && act[1] == 'c' && representActive(act[2])) return (act)
  if (length(act)==2 && act[1] == 'c' && act[2]=='a') return (act)
  # Buy/Sell conditions
  if (length(act)==3 && (act[1] == 'b' || act[1]=='s' ) )
  {
    if (representActive(act[2]) && !is.na (suppressWarnings (as.numeric (act[3])))) return (act)
  }
  return (NA)
}

# Makes a trade ( changes weights vector)
MakeTrade <- function(weights, trade)
{
  newWeights <- weights
  if (trade[1] == 'c')
  {
    if (trade[2] =='a') { newWeights <- rep(0,N) }
    else newWeights[as.numeric(trade[2])] <- 0
  }
  
  if (trade[1] == 's') { trade[3]<- -as.numeric (trade[3]) }
  
  if (trade[1] == 'b' || trade[1] == 's') {newWeights[as.numeric(trade[2])] <- newWeights[as.numeric(trade[2])] + as.numeric(trade[3])/100}
  return(newWeights)
}

ManageCapital <- function (capital, weights)
{
  newCapital <- vector(length = N+2, mode = 'numeric')
  total <- capital['total']
  print(total)
  newCapital[N+2] <- total
  for (i in 1:N) newCapital[i] <- weights[i]*total
  newCapital[N+1] <- total - sum(newCapital[1:N])
  return(newCapital)
}

# Redraws plots
UpdatePlots <- function(current.position, data, plot.length)
{
  l <- length(data)
  end.position <- current.position
  start.position <-  max(current.position - plot.length + 1, 1)
  if (current.position < plot.length) print('Current position index is smaller than plot length, so graphs were truncated')
  for (i in 1:l) plot(data[[i]][start.position:end.position],type='l',ylab = i, xlab = 't', las = 1)
  plot (marketCapital[start.position:end.position,'total'],type='l',ylab = 'Market', xlab = 't', las = 1)
  plot (myCapital[start.position:end.position,'total'],type='l',ylab = 'Strategy', xlab = 't', las = 1)
}


# Technical functions

calculateWeights <- function(capital)
{
  result <- vector(length = N, mode = 'numeric')
  for ( i in 1:N)
  {
    result[i] <- capital[i]/capital['total']
  }
  return(result)
}

# Logical version of grep
contains <- function(pattern, string)
{
  if (length(grep(pattern,string))==0) return(FALSE)
  return(TRUE)
}

# Returns true if the value is an integer in [1,N]
representActive <- function(value)
{
  num <- suppressWarnings(as.numeric(value))
  if (!is.na(num))
  {
    if (num>=1 && num<=N && num%%1==0) return (TRUE)
  }
  return (FALSE)
}

percent.diff <- function(data)
{
  # Checking if an object is of class xts
  if (length(grep('xts',class(data)))==0) return (NULL)
  L <- length(data)
  diff <- vector(length = L, mode = 'numeric')
  for (t in 2:L) 
  {
    curr <- coredata(data[t])
    prev <- coredata(data[t-1])
    diff[t] <- (curr-prev)/prev*100
  }
  diff <- xts(x = diff, order.by = index(data))
}

create.rolling.window <- function(L, window.size, offset)
{
  number.of.forecasts <- (L - window.size)%/%offset
  
  forecast.starts <- seq(1, L, by = offset) 
  forecast.ends <- seq(window.size, L, by = offset)
  forecast.starts <- forecast.starts[1:number.of.forecasts]
  forecast.ends <- forecast.ends[1:number.of.forecasts]
  
  result <- list(number.of.forecasts,forecast.starts,forecast.ends)
  names(result) <- c('number','starts','ends')
  
  return(result)
}

# Fits best ARIMA model on data with d:=0, errors protected
best.fit.arma <- function(data,p_max,q_max)
{
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:p_max) for (q in 0:q_max) {
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch( arima(data, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(data, order=final.order)
      }
    } else {
      next
    }
  }
  names(final.order) <- c('p','d','q')
  final.arima$order <- final.order
  return(final.arima)
}

fit.garch <- function(data, model, out.sample = 0)
{
  fit = tryCatch(ugarchfit(model, data, solver = 'hybrid',out.sample = out.sample),
                 error=function( err ) FALSE,
                warning=function( err ) FALSE)
  return (fit)
}

best.fit.garch <- function(data, p_max, q_max, out.sample = 0)
{
  final.garch <- 0
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:p_max) for (q in 0:q_max) 
    {
      if ( p == 0 && q == 0) {next}
      spec = ugarchspec(
        variance.model=list(garchOrder=c(garch.order[1],garch.order[2])),
        mean.model=list(armaOrder=c(p, q), include.mean=T),
        distribution.model="sged")
    
      garchFit <- fit.garch(data,spec, out.sample = out.sample)
      if (!is.logical( garchFit ) )
        {
          current.aic <- infocriteria(garchFit)[1]
          if (current.aic < final.aic) 
            {
              final.aic <- current.aic
              final.order <- c(p, 0, q)
              final.garch <- garchFit
            }
        }
        else {next}
    }
  return (final.garch)
}

# Updates capital for the buy and hold strategy
UpdateMarket <- function(new, current)
{
  if (new<=forecast.window)
  {
    print('Performance values are not updated for current.position <= forecast.window! Forecasts are unavailable for this period.')
    return()
  }
  else 
  {
    if (new < current)
    {
      marketPerformance[(new+1):L] <<- 0
      return()
    }
    performances <- currentPrices <- startingPrices <- vector(length = N, mode = 'numeric')
    for (i in 1:N) startingPrices[i] <- price[[i]][forecast.window]
    for (t in current:new)
    {
      for (i in 1:N) 
      {
        currentPrices[i] <- price[[i]][t]
        performances[i] <- currentPrices[i] / startingPrices[i]
      }
      marketPerformance[t] <<- initialCapital * mean(performances)
    }
  }
  
}


