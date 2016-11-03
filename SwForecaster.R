# Paramters to configure

forecastW <- 15                    # Number of days to forecast
partitionByDate <- T               # TRUE if the output shall be sorted on Date
                                   # Recommended to be FALSE for large scale forecasts
nSKUs <- 2                         # Number of products to forecast for

# Set working directory where input files are

setwd("~/Documents/homeprojects/Swarovski")

# The following libraries are used:

library(forecast)
library(stats)
library(data.table)
library(doMC)

# Reading the training set as data table with fread is much faster in case of big data

training <- fread("training.csv")
testDF <- read.csv("test.csv")

# Preparing the output parameters based on test.csv

dates <- as.vector(testDF$Date[2*(1:forecastW)])
testDF$Sales <- 0
testDF <- testDF[0,]
write.table(testDF, "test_stl.csv", quote = F, 
            row.names = F, sep = ',', col.names = T)

# The forecaster function takes the SKU and forecast window as inputs 
# and returns a dataframe with the forecasts. In case of partitionByDate is false
# it writes the predictions to disk and return an empty dataframe.

forecaster <- function(sku, forecastW){
  
  trainTS <- ts(training[training$Series == sku, ]$Sales, 
                 start = 1, frequency = 7)
  
  fit <- stl(trainTS, s.window = "periodic")
  fc <- forecast(fit, h = forecastW, method = "arima" )
  
  testDF <- rbind(testDF, as.data.frame(cbind(dates, sku, 
                               round(as.vector(fc$mean), 2))))
  if (partitionByDate == F){
    write.table(testDF, "test_stl.csv", append = T, quote = F, 
                row.names = F, sep = ',', col.names = F)
    testDF <- testDF[0, ]
  }
  return(testDF)
}

# Registering multicore executor

registerDoMC() 

# Running the executor on all SKUs distributed to all cores

s <- Sys.time()
outDF <- foreach(i = 1:nSKUs, .combine=rbind) %dopar% 
  forecaster(paste("series", as.character(i), sep=""), forecastW)
e <- Sys.time()
print(e-s)

colnames(outDF) = c("Date", "Series", "Sales")

# If the output shall be partitioned by Date it needs to be saved after sorting 
# according to Date

if (partitionByDate){
  outDF <- outDF[order(outDF$Date),]
  write.table(outDF, "test_stl.csv", quote = F, row.names = F, sep = ',', col.names = T)
}

