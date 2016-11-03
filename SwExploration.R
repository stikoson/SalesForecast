# The working directory shall be set to the one where the training and test files are

setwd("~/Documents/homeprojects/Swarovski")

# The following libraries are required

library(forecast)
library(stats)
library(ggplot2)
library(tseries)

# Reading and filtering on the different series

training <- read.csv("training.csv")
series <- "series2"

seriesDF <- training[training$Series==series, ]
seriesDF$Date <- as.Date( seriesDF$Date, '%d/%m/%Y')

# Checking stationarity with Dickey - Fuller test

adf.test(seriesDF$Sales)

# Plotting autocorrelation

series1 <- training$Sales[training$Series=="series1"]
Acf(series1)

series2 <- training$Sales[training$Series=="series2"]
Acf(series2)

# Plotting sales data

ggplot(data = seriesDF, aes(Date, Sales)) + geom_line(color='darkblue') +
  ggtitle(paste("Sales for ", series, sep='')) +
  scale_y_continuous(labels = comma, limits=c(0, 1.2*max(seriesDF$Sales)))

