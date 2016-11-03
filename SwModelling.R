# Set working directory where input files are

setwd("~/Documents/homeprojects/Swarovski")

# The following libraries are used:

library(forecast)
library(stats)
library(ggplot2)

# A plotting function that prints accuracy of the forecast
# The training.csv will be divided into trainTS and testTS (last 15 days)
# Different forecasting models will be evaluated on the last 15 days of data

dispW <- 356      # Timewindow for the plot function
forecastW <- 178       # Forecast window: 15 for small, 178 for big 

plotFC <- function(fc){
  trainDF$Sales <- tail(fc$fitted, dispW - forecastW)
  testDF$Sales <- fc$mean
  
  seriesDF_disp <- rbind(seriesDF, trainDF, testDF)
  
  p <- ggplot(data = seriesDF_disp, aes( Date, Sales, color=Legend  )) + geom_line() +
    ggtitle(paste("Model fitting for ", series, sep='')) +
    scale_color_manual(breaks=c("train", "test", "fitted", "forecast"),
                       values=c("black", "darkblue", "lightblue", "darkgray")) +
    scale_y_continuous(labels = comma)
  
  print(p)
  
  print(accuracy(fc, testTS))
}

training <- read.csv("training.csv")

# Transforming date for the random forest experiment

training$Date <- as.Date(training$Date, '%d/%m/%Y')
training$Year <- as.factor(year(training$Date))
training$Month <- as.factor(month(training$Date))
training$Day <- as.factor(mday(training$Date))
training$Weekday <- as.factor(wday(training$Date))

series <- "series2"

# Helper dataframes are created to support easy plotting of training, test, fitted
# and forecasted datasets

seriesDF <- training[training$Series==series, ]
seriesDF <- tail(seriesDF, dispW)
seriesDF$Legend <- "train"
seriesDF$Legend[(nrow(seriesDF) - forecastW + 1):nrow(seriesDF)] <- "test"

trainDF <- head(seriesDF, (nrow(seriesDF) - dispW - forecastW))
testDF <- tail(seriesDF, forecastW)
trainDF$Legend <- "fitted"
testDF$Legend <- "forecast"

# Creating a timeseries and then a training and a test one

seriesTS <- ts(training[training$Series==series, ]$Sales, start = 1, frequency = 7)

# Train and test in case of forecastW <- 15

trainTS <- window(seriesTS, end = c(124, 2))
testTS <- window(seriesTS, start = c(124, 3))

# Train and test in case of forecastW <- 178

#trainTS <- window(seriesTS, end = c(100, 7))
#testTS <- window(seriesTS, start = c(101, 1))


# Linear model with time series components

startTime <- Sys.time()
fit <- tslm(trainTS ~ trend + season)
endTime <- Sys.time()
print(endTime - startTime)
fc <- forecast(fit, h = forecastW)
plotFC(fc)

# Seasonal Decomposition of Time Series by Loess

startTime <- Sys.time()
fit <- stl(trainTS, s.window = "periodic")
endTime <- Sys.time()
print(endTime - startTime)
fc <- forecast(fit, h = forecastW, method = "arima" )
plotFC(fc)

# Neural Network Time Series Forecasts

startTime <- Sys.time()
fit <- nnetar(trainTS)
endTime <- Sys.time()
print(endTime - startTime)
fc <- forecast(fit, h = forecastW)
plotFC(fc)

# Best univariate ARIMA

startTime <- Sys.time()
fit <- auto.arima(trainTS)
endTime <- Sys.time()
print(endTime - startTime)
fc <- forecast(fit, h = forecastW)
plotFC(fc)

# Holt - Winters filtering
startTime <- Sys.time()
fit <- HoltWinters(trainTS)
endTime <- Sys.time()
print(endTime - startTime)
fc <- forecast(fit, h = forecastW)
plotFC(fc)

# RandomForest experiment

library(randomForest)

trainingDF <- training[training$Series==series, ][1:(nrow(seriesDF) - forecastW),]
testingDF <- training[training$Series==series, ][(nrow(seriesDF) - forecastW + 1):(nrow(seriesDF)),]

trainingDF$Date <- NULL

y <- testingDF$Sales
testingDF$Sales <- NULL
startTime <- Sys.time()
rf <- randomForest(Sales ~ ., data = trainingDF)
endTime <- Sys.time()
print(endTime - startTime)

# testRMSE

pred <- predict(rf, newdata = testingDF)
sqrt(sum((pred-y)^2)/nrow(testingDF))

# trainRMSE

pred <- predict(rf)
sqrt(sum((pred-trainingDF$Sales)^2)/nrow(trainingDF))




