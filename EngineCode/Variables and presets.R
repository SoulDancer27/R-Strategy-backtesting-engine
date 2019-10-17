# Essential global variables and presets for the trading simulator
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
source('/Users/igorpanin/Desktop/Simple trading engine/Trading Engine.R')

# 1. Loading data

data <- readRDS('/Users/igorpanin/Desktop/2010-2018.rds')
L <- length(Cl(data[[1]]))

# 2. Simulator presets
loadSave <- NA

if (!is.na(loadSave)) LoadSave()

# 2.1 Setting default modelling properties
if (is.na(loadSave))
{
  arima.order <- c(1,0,0)
  garch.order <- c(1,1)
  forecast.window <- 500
  offset <- 10
  current.position <- forecast.window + 1
  initialCapital <- 100000
  marketPerformance <- vector(mode = 'numeric', length = L)
  marketPerformance[1:forecast.window] <- initialCapital
}

# 2.2 Selecting data
# Number of assets
N <- 4
diff <- price <- vector(length = N, mode = 'list')
for (i in 1:N)
{
  price[[i]] <- Cl(data[[i]])
  diff[[i]] <- percent.diff(price[[i]])
  price[[i]] <- coredata(price[[i]])
  diff[[i]] <- coredata(diff[[i]])
}

# 2.3 Plots preset
layout (matrix (c (1,2,3,4,5,6), 2, 3, byrow = FALSE))
plot.length <- 200

# 2.4 Trading interface

marketCapital <- myCapital <- createCapital(L,N)
for (i in 1:N)
{
  marketCapital[current.position,i] <- marketCapital[current.position,'total']/N
  marketCapital[current.position,'free'] <- 0
}
# 1 value for money in each asset, 1 value for free money and 1 for total capital
createCapital <- function(L,N)
{
  capital <- array(data = 0, dim = c(L,N+2))
  names <- vector(mode = 'character', length = N+2)
  
  for (i in 1:N) names[i] <- as.character(i)
  names[N+1] <- 'free'
  names[N+2] <- 'total'
  colnames(capital) <- names
  
  capital[1:L,'free'] <- initialCapital
  capital[1:L,'total'] <- initialCapital
  return(capital)
}
