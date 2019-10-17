# Recompile this file to return all values to the default
source ('/Users/igorpanin/Desktop/Simple trading engine/Variables and presets.R')

# GARCH forecasts

forecasts <- create.rolling.window(L = L, forecast.window+offset, offset)
number.of.forecasts <-  forecasts[['number']]
forecast.starts <- forecasts[['starts']]
forecast.ends <- forecasts[['ends']]

garchModels <- vector(length = N, mode = 'list')
for (i in 1:N) 
{
  garchModels[[i]] <- vector(length = number.of.forecasts, mode = 'list')
  for (t in 1:number.of.forecasts)
  {
    print(t)
    subset <- diff[[i]][forecast.starts[t]:forecast.ends[t]]
    garchModels[[i]][t] <- best.fit.garch(subset,arima.order[1],arima.order[3],out.sample = offset)
  }
}

sigma <- prediction <- array(data = 0, dim = c(L,N))
for (i in 1:N)
{
  for (j in 1:number.of.forecasts)
  {
    g <- garchModels[[i]][j][[1]]
    pr <- ugarchforecast(g,n.ahead = 1, n=10)
    start <- (forecast.ends[j]-offset+1)
    end <- forecast.ends[j]
    prediction[start:end,i] <- rep(pr@model$pars[1,1],10)
    sigma[start:end,i] <- pr@forecast$sigmaFor[1:10]
  }
}

# Flow scheme
# Dependensies from global variables are shown in brackets ()
# Manipulations on global variables are shown in qurled brackets {}

#     repeat()
#   ->|
#     InputControl( input ) 
#         -> Quit( quitIndex ) { quitIndex }    -   -   -   -   -   -   -   -   -> Exits repeat
#         -> Step( input, current.position ) {} 
#             -> UpdatePosition( ) { current.position } 
#                 -> UpdateMarket( forecast.window, marketPerformance, initialCapital ) { marketPerformance }
#                 -> UpdatePlots( current.position, data, plot.length, marketPerformance)
#         -> Trade( weights, current.position ) { weights }
#             -> ProcessInput( input ){}
#             -> MakeTrade ( weights ) {}
#   <-|


# Main flow

# Exit the repeat loop if quitIndex is set to 1 (see InputControl -> Quit)
quitIndex <- 0

repeat
{
  input <- readline()
  action <- InputControl(input)
  if (is.function(action)) action()
  
  if (quitIndex==1) 
  {
    print('Exiting the main flow loop.')
    break
  }
}
mardiff <- mar <- myCapital[501:1563,'total']
for (t in 2:(length(mardiff)))
{
 cur <- mar[t]
 prev <- mar[t-1]
 mardiff[t] <- (cur-prev)/prev*100
}
mardiff[1] <- 0

sd(mardiff)

sd(myCapital[501:1563,'total'])
