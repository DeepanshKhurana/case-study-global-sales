###### GLOBAL SUPERSTORE CASE STUDY

salesData <-
  aws.s3::s3read_using(read.csv, object = "s3://r-studio-bucket/Global Superstore.csv")

# backup <- salesData
# salesData <- backup

########## PART 0 - CREATING ENVIRONMENT

### LOADING LIBRARIES

library(tidyverse)
library(lubridate)
library(gridExtra)
library(forecast)
library(tseries)

### LOADING FILE

# salesData <-
#   read.csv("Global Superstore.csv",
#            stringsAsFactors = F,
#            header = T)

######### PART 1 - DATA PREPARATION

### CHECKING THE FILE

str(salesData)
names(salesData)

### SUBSETTING FOR RELEVANT DATA

## THESE ARE THE ONLY COLUMNS NECESSARY FOR US

# Order Date for aggregating Month-wise
# Segment
# Market
# Sales or Revenue
# Quantity
# Profit

salesData <-
  select(salesData,
         c(Date = "Order.Date", "Segment", "Market",
           "Sales", "Quantity", "Profit"))

### FIXING DATA TYPES

salesData$Date <- as.Date(salesData$Date, format = "%d-%m-%Y")
salesData$Segment <- as.factor(salesData$Segment)
salesData$Market <- as.factor(salesData$Market)

str(salesData)

### CHECK FOR NA

sum(is.na(salesData))

# There are no NAs in our subset

### CREATING THE REQUIRED 21 SEGMENTS FROM MARKET AND SEGMENT COLUMNS

salesData <-
  unite(salesData, "MarketSegment", c("Segment", "Market"))
salesData$SegmentMarket <- as.factor(salesData$MarketSegment)

str(salesData)

### ORDERING THE DATASET BY DATE

salesData <- salesData[order(salesData$Date, decreasing = F),]

salesData <-
  mutate(
    salesData,
    Month = month(salesData$Date, label = TRUE),
    Year = year(salesData$Date)
  )

segmentwise <-
  salesData %>% group_by(MarketSegment, Year, Month) %>%
  summarise(
    Sales = sum(Sales),
    Qty = sum(Quantity),
    Profit = sum(Profit)
  )

str(segmentwise)

segmentwise$MarketSegment <- as.factor(segmentwise$MarketSegment)
segments <- levels(segmentwise$MarketSegment)

### SUBSETTING THE SEGMENTWISE DATAFRAME INTO 21 SUBSETS

segment.list <- c()

for (segment in segments) {
  segment.list[[segment]] <-
    segmentwise[which(segmentwise$MarketSegment == segment), ]
}

# list2env(segment.list, .GlobalEnv)

### CREATING SOME FUNCTIONS

find.coeff.variance <- function(data) {
  return(sd(data) / mean(data))
}

### MODIFY SEGMENTWISE TO CREATE A DATE COLUMN

segmentwise$Date <-
  as.Date(paste(segmentwise$Year, as.numeric(segmentwise$Month), "01", sep = "-"))
segmentwise <- segmentwise[, c(1, 7, 2:6)]

### CREATING SOME PLOTS TO VISUALISE OUR SEGMENTS

## SUMMARISING TOP 5 FOR EACH OF OUR METRICS

top5Sales <- segmentwise[, c(1:5)] %>%
  group_by(MarketSegment) %>%
  summarise(Sales = sum(Sales)) %>%
  top_n(5) %>%
  arrange(desc(Sales))

top5Quantity <- segmentwise[, c(1:4, 6)] %>%
  group_by(MarketSegment) %>%
  summarise(Qty = sum(Qty)) %>%
  top_n(5) %>%
  arrange(desc(Qty))

top5Profit <- segmentwise[, c(1:4, 7)] %>%
  group_by(MarketSegment) %>%
  summarise(Profit = sum(Profit)) %>%
  top_n(5) %>%
  arrange(desc(Profit))

## PLOTTING TOP 5 FOR EACH OF OUR METRICS

top5Sales.plot <-
  ggplot(top5Sales, aes(
    x = reorder(MarketSegment, Sales),
    y = Sales,
    fill = MarketSegment
  )) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.95)) + labs(
          x = "",
          y = "",
          title = "Top 5 Segments",
          subtitle = "by Sales"
        ) + geom_col(width = 0.5) # + coord_flip()

top5Profit.plot <-
  ggplot(top5Profit, aes(
    x = reorder(MarketSegment, Profit),
    y = Profit,
    fill = MarketSegment
  )) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.95)) + labs(
          x = "",
          y = "",
          title = "Top 5 Segments",
          subtitle = "by Profit"
        ) + geom_col(width = 0.5) # + coord_flip()

top5Quantity.plot <-
  ggplot(top5Quantity, aes(
    x = reorder(MarketSegment, Qty),
    y = Qty,
    fill = MarketSegment
  )) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.95)) + labs(
          x = "",
          y = "",
          title = "Top 5 Segments",
          subtitle = "by Quantity"
        ) + geom_col(width = 0.5) # + coord_flip()

gridExtra::grid.arrange(top5Profit.plot, top5Sales.plot, top5Quantity.plot, ncol = 3)

## CONCLUSION:

# In terms of Profit, Consumer_APAC, Consumer_EU seem to be the top two segments.
# In terms of Sales, Consumer_APAC, Consumer_EU seem to be the top two segments.
# In terms of Quantity, Consumer_APAC, Consumer_LATAM seem to be the top two segments.

# Consumer_APAC is the top segment.

# To find the second top segment, let us take a look at the Coefficient of Variance for Profit

### COMPARING COEFFICIENT OF VARIANCE FOR Profit

profit.variance.segments <- segmentwise %>%
  group_by(MarketSegment) %>%
  summarise(CoeffVarianceProfit = find.coeff.variance(Profit)) %>%
  arrange(CoeffVarianceProfit)

top.2.segments <- profit.variance.segments %>%
  top_n(-2)

# Note: Low Coefficient of Variance is desirable

top2 <- as.character(top.2.segments$MarketSegment)

top2

# "Consumer_EU" & "Consumer_APAC" are the top two segments based on the Coefficienct of Variance
# on the basis of profit. Therefore, we can conclude that these two are the most consistent
# as well as among the most profitable segments. Therefore, we can conclude that these are our
# targeted segments

### SUBSETTING THE TOP 2 FROM OUR SEGMENTS LIST

segmentwise <-
  segmentwise[which(segmentwise$MarketSegment %in% top2),]

consumer.eu <-
  segmentwise[which(segmentwise$MarketSegment == "Consumer_EU"), ]
consumer.apac <-
  segmentwise[which(segmentwise$MarketSegment == "Consumer_APAC"), ]

### MAKING TIMESERIES FOR "Consumer_APAC"

apac.sales <-
  ts(consumer.apac$Sales,
     frequency = 12,
     start = c(2011, 01))
apac.profit <-
  ts(consumer.apac$Profit,
     frequency = 12,
     start = c(2011, 01))
apac.qty <-
  ts(consumer.apac$Qty,
     frequency = 12,
     start = c(2011, 01))

### MAKING TIMESERIES FOR "Consumer_EU"

eu.sales <-
  ts(consumer.eu$Sales,
     frequency = 12,
     start = c(2011, 01))
eu.profit <-
  ts(consumer.eu$Profit,
     frequency = 12,
     start = c(2011, 01))
eu.qty <- ts(consumer.eu$Qty,
             frequency = 12,
             start = c(2011, 01))

### PLOTTING TIMESERIES

apac.sales.plot <-
  autoplot(apac.sales, colour = "darkblue") + labs(
    x = "",
    y = "",
    title = "Consumer_APAC",
    subtitle = "Sales"
  ) + theme_minimal()
apac.sales.plot
apac.profit.plot <-
  autoplot(apac.profit, colour = "darkred") + labs(
    x = "",
    y = "",
    title = "Consumer_APAC",
    subtitle = "Profit"
  ) + theme_minimal()
apac.profit.plot
apac.qty.plot <-
  autoplot(apac.qty, colour = "darkgreen") + labs(
    x = "",
    y = "",
    title = "Consumer_APAC",
    subtitle = "Quantity"
  ) + theme_minimal()
apac.qty.plot

eu.sales.plot <-
  autoplot(eu.sales, colour = "darkblue") + labs(
    x = "",
    y = "",
    title = "Consumer_EU",
    subtitle = "Sales"
  ) + theme_minimal()
eu.sales.plot
eu.profit.plot <-
  autoplot(eu.profit, colour = "darkred") + labs(
    x = "",
    y = "",
    title = "Consumer_EU",
    subtitle = "Profit"
  ) + theme_minimal()
eu.profit.plot
eu.qty.plot <-
  autoplot(eu.qty, colour = "darkgreen") + labs(
    x = "",
    y = "",
    title = "Consumer_EU",
    subtitle = "Quantity"
  ) + theme_minimal()
eu.qty.plot

########## PART 2 - MODEL BUILDING AND EVALUATION

###### CONSUMER_APAC

apac.for.model <- consumer.apac[1:42,]

#### TIMESERIES FOR MODELLING

apac.sales.model.ts <- ts(apac.for.model$Sales)
apac.profit.model.ts <- ts(apac.for.model$Profit)
apac.qty.model.ts <- ts(apac.for.model$Qty)

### SALES

## SMOOTHING THE TIMESERIES

w <- 1
apac.sales.smooth <- stats::filter(
  apac.sales.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w + 1)),
  method = 'convolution',
  sides = 2
)

diff <- apac.sales.smooth[w + 2] - apac.sales.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  apac.sales.smooth[i] <- apac.sales.smooth[i + 1] - diff
}

n <- length(apac.sales.model.ts)

diff <- apac.sales.smooth[n - w] - apac.sales.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  apac.sales.smooth[i] <- apac.sales.smooth[i - 1] + diff
}

# PLOTTING

plot(apac.sales.model.ts)
lines(apac.sales.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

apac.sales.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(apac.sales.smooth)))
colnames(apac.sales.smooth) <- c('Month', 'Sales')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

apac.sales.lmfit <-
  lm(Sales ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = apac.sales.smooth)

apac.sales.global <- predict(apac.sales.lmfit, Month = c(1:42))

summary(apac.sales.global)

lines(c(1:42), apac.sales.global, col = "darkgreen", lwd = 2)

apac.sales.local <- apac.sales.model.ts - apac.sales.global

plot(apac.sales.local, col = "darkred", type = "l")

acf(apac.sales.local)

acf(apac.sales.local, type = "partial")

apac.sales.armafit <- auto.arima(apac.sales.local)

tsdiag(apac.sales.armafit)

apac.sales.armafit

apac.sales.residuals <-
  apac.sales.local - fitted(apac.sales.armafit)

# ADF AND KPSS TEST

adf.test(apac.sales.residuals, alternative = "stationary")

kpss.test(apac.sales.residuals)

# MAPE CALCULATION

apac.sales.for.eval <- consumer.apac[c(43:48),]

apac.sales.global.eval <-
  predict(apac.sales.lmfit, data.frame(Month = c(43:48)))

temp <- apac.sales.for.eval[, 5]

apac.sales.MAPE <-
  accuracy(apac.sales.global.eval, as.integer(temp$Sales))[, 5]

apac.sales.MAPE

## ARIMA FIT

apac.sales.auto.arima <- auto.arima(apac.sales.model.ts)
apac.sales.auto.arima

tsdiag(apac.sales.auto.arima)

plot(apac.sales.auto.arima$x)
lines(fitted(apac.sales.auto.arima), col = "darkred")

apac.sales.auto.arima.residual <-
  apac.sales.model.ts - fitted(apac.sales.auto.arima)

# ADF & KPSS TEST

adf.test(apac.sales.auto.arima.residual, alternative = "stationary")

kpss.test(apac.sales.auto.arima.residual)

# MAPE CALCULATION

apac.sales.global.auto.arima <-
  forecast(apac.sales.auto.arima, h = 6)

temp <- apac.sales.for.eval[, 5]

apac.sales.MAPE.arima <-
  accuracy(apac.sales.global.auto.arima, as.integer(temp$Sales))[, 5]

apac.sales.MAPE.arima

# MODEL NAMES:
# apac.sales.global.auto.arima : MAPE 35.78, 27.68
# apac.sales.lmfit : 31.07

### QTY

## SMOOTHING THE TIMESERIES

w <- 1
apac.qty.smooth <- stats::filter(
  apac.qty.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w +
                                   1)),
  method = 'convolution',
  sides = 2
)

diff <- apac.qty.smooth[w + 2] - apac.qty.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  apac.qty.smooth[i] <- apac.qty.smooth[i + 1] - diff
}

n <- length(apac.qty.model.ts)

diff <- apac.qty.smooth[n - w] - apac.qty.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  apac.qty.smooth[i] <- apac.qty.smooth[i - 1] + diff
}

# PLOTTING

plot(apac.qty.model.ts)
lines(apac.qty.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

apac.qty.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(apac.qty.smooth)))
colnames(apac.qty.smooth) <- c('Month', 'Qty')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

apac.qty.lmfit <-
  lm(Qty ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = apac.qty.smooth)

apac.qty.global <- predict(apac.qty.lmfit, Month = c(1:42))

summary(apac.qty.global)

lines(c(1:42), apac.qty.global, col = "darkgreen", lwd = 2)

apac.qty.local <- apac.qty.model.ts - apac.qty.global

plot(apac.qty.local, col = "darkred", type = "l")

acf(apac.qty.local)

acf(apac.qty.local, type = "partial")

apac.qty.armafit <- auto.arima(apac.qty.local)

tsdiag(apac.qty.armafit)

apac.qty.armafit

apac.qty.residuals <- apac.qty.local - fitted(apac.qty.armafit)

# ADF AND KPSS TEST

adf.test(apac.qty.residuals, alternative = "stationary")

kpss.test(apac.qty.residuals)

# MAPE CALCULATION

apac.qty.for.eval <- consumer.apac[c(43:48),]

apac.qty.global.eval <-
  predict(apac.qty.lmfit, data.frame(Month = c(43:48)))

temp <- apac.qty.for.eval[, 6]

apac.qty.MAPE <-
  accuracy(apac.qty.global.eval, as.integer(temp$Qty))[, 5]

apac.qty.MAPE

## ARIMA FIT

apac.qty.auto.arima <- auto.arima(apac.qty.model.ts)
apac.qty.auto.arima

tsdiag(apac.qty.auto.arima)

plot(apac.qty.auto.arima$x)
lines(fitted(apac.qty.auto.arima), col = "darkred")

apac.qty.auto.arima.residual <-
  apac.qty.model.ts - fitted(apac.qty.auto.arima)

# ADF & KPSS TEST

adf.test(apac.qty.auto.arima.residual, alternative = "stationary")

kpss.test(apac.qty.auto.arima.residual)

# MAPE CALCULATION

apac.qty.global.auto.arima <- forecast(apac.qty.auto.arima, h = 6)

temp <- apac.qty.for.eval[, 6]

apac.qty.MAPE.arima <-
  accuracy(apac.qty.global.auto.arima, as.integer(temp$Qty))[, 5]

apac.qty.MAPE.arima

# MODEL NAMES:
# apac.qty.global.auto.arima : MAPE 36.79, 26.24
# apac.qty.lmfit : 93.28

### PROFIT

## SMOOTHING THE TIMESERIES

w <- 1
apac.profit.smooth <- stats::filter(
  apac.profit.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w +
                                   1)),
  method = 'convolution',
  sides = 2
)

diff <- apac.profit.smooth[w + 2] - apac.profit.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  apac.profit.smooth[i] <- apac.profit.smooth[i + 1] - diff
}

n <- length(apac.profit.model.ts)

diff <- apac.profit.smooth[n - w] - apac.profit.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  apac.profit.smooth[i] <- apac.profit.smooth[i - 1] + diff
}

# PLOTTING

plot(apac.profit.model.ts)
lines(apac.profit.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

apac.profit.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(apac.profit.smooth)))
colnames(apac.profit.smooth) <- c('Month', 'Profit')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

apac.profit.lmfit <-
  lm(Profit ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = apac.profit.smooth)

apac.profit.global <- predict(apac.profit.lmfit, Month = c(1:42))

summary(apac.profit.global)

lines(c(1:42), apac.profit.global, col = "darkgreen", lwd = 2)

apac.profit.local <- apac.profit.model.ts - apac.profit.global

plot(apac.profit.local, col = "darkred", type = "l")

acf(apac.profit.local)

acf(apac.profit.local, type = "partial")

apac.profit.armafit <- auto.arima(apac.profit.local)

tsdiag(apac.profit.armafit)

apac.profit.armafit

apac.profit.residuals <-
  apac.profit.local - fitted(apac.profit.armafit)

# ADF AND KPSS TEST

adf.test(apac.profit.residuals, alternative = "stationary")

kpss.test(apac.profit.residuals)

# MAPE CALCULATION

apac.profit.for.eval <- consumer.apac[c(43:48),]

apac.profit.global.eval <-
  predict(apac.profit.lmfit, data.frame(Month = c(43:48)))

temp <- apac.profit.for.eval[, 7]

apac.profit.MAPE <-
  accuracy(apac.profit.global.eval, as.integer(temp$Profit))[, 5]

apac.profit.MAPE

## ARIMA FIT

apac.profit.auto.arima <- auto.arima(apac.profit.model.ts)
apac.profit.auto.arima

tsdiag(apac.profit.auto.arima)

plot(apac.profit.auto.arima$x)
lines(fitted(apac.profit.auto.arima), col = "darkred")

apac.profit.auto.arima.residual <-
  apac.profit.model.ts - fitted(apac.profit.auto.arima)

# ADF & KPSS TEST

adf.test(apac.profit.auto.arima.residual, alternative = "stationary")

kpss.test(apac.profit.auto.arima.residual)

# MAPE CALCULATION

apac.profit.global.auto.arima <-
  forecast(apac.profit.auto.arima, h = 6)

temp <- apac.profit.for.eval[, 7]

apac.profit.MAPE.arima <-
  accuracy(apac.profit.global.auto.arima, as.integer(temp$Profit))[, 5]

apac.profit.MAPE.arima

# MODEL NAMES:
# apac.profit.global.auto.arima : MAPE 143.64, 41.29
# apac.profit.lmfit : 49.3421

###### CONSUMER_EU

eu.for.model <- consumer.eu[1:42,]

#### TIMESERIES FOR MODELLING

eu.sales.model.ts <- ts(eu.for.model$Sales)
eu.profit.model.ts <- ts(eu.for.model$Profit)
eu.qty.model.ts <- ts(eu.for.model$Qty)

### SALES

## SMOOTHING THE TIMESERIES

w <- 1
eu.sales.smooth <- stats::filter(
  eu.sales.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w + 1)),
  method = 'convolution',
  sides = 2
)

diff <- eu.sales.smooth[w + 2] - eu.sales.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  eu.sales.smooth[i] <- eu.sales.smooth[i + 1] - diff
}

n <- length(eu.sales.model.ts)

diff <- eu.sales.smooth[n - w] - eu.sales.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  eu.sales.smooth[i] <- eu.sales.smooth[i - 1] + diff
}

# PLOTTING

plot(eu.sales.model.ts)
lines(eu.sales.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

eu.sales.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(eu.sales.smooth)))
colnames(eu.sales.smooth) <- c('Month', 'Sales')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

eu.sales.lmfit <-
  lm(Sales ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = eu.sales.smooth)

eu.sales.global <- predict(eu.sales.lmfit, Month = c(1:42))

summary(eu.sales.global)

lines(c(1:42), eu.sales.global, col = "darkgreen", lwd = 2)

eu.sales.local <- eu.sales.model.ts - eu.sales.global

plot(eu.sales.local, col = "darkred", type = "l")

acf(eu.sales.local)

acf(eu.sales.local, type = "partial")

eu.sales.armafit <- auto.arima(eu.sales.local)

tsdiag(eu.sales.armafit)

eu.sales.armafit

eu.sales.residuals <-
  eu.sales.local - fitted(eu.sales.armafit)

# ADF AND KPSS TEST

adf.test(eu.sales.residuals, alternative = "stationary")

kpss.test(eu.sales.residuals)

# MAPE CALCULATION

eu.sales.for.eval <- consumer.eu[c(43:48),]

eu.sales.global.eval <-
  predict(eu.sales.lmfit, data.frame(Month = c(43:48)))

temp <- eu.sales.for.eval[, 5]

eu.sales.MAPE <-
  accuracy(eu.sales.global.eval, as.integer(temp$Sales))[, 5]

eu.sales.MAPE

## ARIMA FIT

eu.sales.auto.arima <- auto.arima(eu.sales.model.ts)
eu.sales.auto.arima

tsdiag(eu.sales.auto.arima)

plot(eu.sales.auto.arima$x)
lines(fitted(eu.sales.auto.arima), col = "darkred")

eu.sales.auto.arima.residual <-
  eu.sales.model.ts - fitted(eu.sales.auto.arima)

# ADF & KPSS TEST

adf.test(eu.sales.auto.arima.residual, alternative = "stationary")

kpss.test(eu.sales.auto.arima.residual)

# MAPE CALCULATION

eu.sales.global.auto.arima <-
  forecast(eu.sales.auto.arima, h = 6)

temp <- eu.sales.for.eval[, 5]

eu.sales.MAPE.arima <-
  accuracy(eu.sales.global.auto.arima, as.integer(temp$Sales))[, 5]

eu.sales.MAPE.arima

# MODEL NAMES:
# eu.sales.global.auto.arima : MAPE 39.42, 28.92
# eu.sales.lmfit : 92.96

### QTY

## SMOOTHING THE TIMESERIES

w <- 1
eu.qty.smooth <- stats::filter(
  eu.qty.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w +
                                   1)),
  method = 'convolution',
  sides = 2
)

diff <- eu.qty.smooth[w + 2] - eu.qty.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  eu.qty.smooth[i] <- eu.qty.smooth[i + 1] - diff
}

n <- length(eu.qty.model.ts)

diff <- eu.qty.smooth[n - w] - eu.qty.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  eu.qty.smooth[i] <- eu.qty.smooth[i - 1] + diff
}

# PLOTTING

plot(eu.qty.model.ts)
lines(eu.qty.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

eu.qty.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(eu.qty.smooth)))
colnames(eu.qty.smooth) <- c('Month', 'Qty')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

eu.qty.lmfit <-
  lm(Qty ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = eu.qty.smooth)

eu.qty.global <- predict(eu.qty.lmfit, Month = c(1:42))

summary(eu.qty.global)

lines(c(1:42), eu.qty.global, col = "darkgreen", lwd = 2)

eu.qty.local <- eu.qty.model.ts - eu.qty.global

plot(eu.qty.local, col = "darkred", type = "l")

acf(eu.qty.local)

acf(eu.qty.local, type = "partial")

eu.qty.armafit <- auto.arima(eu.qty.local)

tsdiag(eu.qty.armafit)

eu.qty.armafit

eu.qty.residuals <- eu.qty.local - fitted(eu.qty.armafit)

# ADF AND KPSS TEST

adf.test(eu.qty.residuals, alternative = "stationary")

kpss.test(eu.qty.residuals)

# MAPE CALCULATION

eu.qty.for.eval <- consumer.eu[c(43:48),]

eu.qty.global.eval <-
  predict(eu.qty.lmfit, data.frame(Month = c(43:48)))

temp <- eu.qty.for.eval[, 6]

eu.qty.MAPE <-
  accuracy(eu.qty.global.eval, as.integer(temp$Qty))[, 5]

eu.qty.MAPE

## ARIMA FIT

eu.qty.auto.arima <- auto.arima(eu.qty.model.ts)
eu.qty.auto.arima

tsdiag(eu.qty.auto.arima)

plot(eu.qty.auto.arima$x)
lines(fitted(eu.qty.auto.arima), col = "darkred")

eu.qty.auto.arima.residual <-
  eu.qty.model.ts - fitted(eu.qty.auto.arima)

# ADF & KPSS TEST

adf.test(eu.qty.auto.arima.residual, alternative = "stationary")

kpss.test(eu.qty.auto.arima.residual)

# MAPE CALCULATION

eu.qty.global.auto.arima <- forecast(eu.qty.auto.arima, h = 6)

temp <- eu.qty.for.eval[, 6]

eu.qty.MAPE.arima <-
  accuracy(eu.qty.global.auto.arima, as.integer(temp$Qty))[, 5]

eu.qty.MAPE.arima

# MODEL NAMES:
# eu.qty.global.auto.arima : MAPE 31.57, 30.13
# eu.qty.lmfit : 30.39

### PROFIT

## SMOOTHING THE TIMESERIES

w <- 1
eu.profit.smooth <- stats::filter(
  eu.profit.model.ts,
  filter = rep(1 / (2 * w + 1), (2 * w +
                                   1)),
  method = 'convolution',
  sides = 2
)

diff <- eu.profit.smooth[w + 2] - eu.profit.smooth[w + 1]
for (i in seq(w, 1, -1)) {
  eu.profit.smooth[i] <- eu.profit.smooth[i + 1] - diff
}

n <- length(eu.profit.model.ts)

diff <- eu.profit.smooth[n - w] - eu.profit.smooth[n - w - 1]
for (i in seq(n - w + 1, n)) {
  eu.profit.smooth[i] <- eu.profit.smooth[i - 1] + diff
}

# PLOTTING

plot(eu.profit.model.ts)
lines(eu.profit.smooth, col = "salmon2", lwd = 2)

# CREATING A DATAFRAME

eu.profit.smooth <-
  as.data.frame(cbind(c(1:42), as.vector(eu.profit.smooth)))
colnames(eu.profit.smooth) <- c('Month', 'Profit')

### CLASSICAL DECOMPOSITION

## FITTING A MODEL

eu.profit.lmfit <-
  lm(Profit ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * Month) * poly(Month, 3)
     + Month,
     data = eu.profit.smooth)

eu.profit.global <- predict(eu.profit.lmfit, Month = c(1:42))

summary(eu.profit.global)

lines(c(1:42), eu.profit.global, col = "darkgreen", lwd = 2)

eu.profit.local <- eu.profit.model.ts - eu.profit.global

plot(eu.profit.local, col = "darkred", type = "l")

acf(eu.profit.local)

acf(eu.profit.local, type = "partial")

eu.profit.armafit <- auto.arima(eu.profit.local)

tsdiag(eu.profit.armafit)

eu.profit.armafit

eu.profit.residuals <- eu.profit.local - fitted(eu.profit.armafit)

# ADF AND KPSS TEST

adf.test(eu.profit.residuals, alternative = "stationary")

kpss.test(eu.profit.residuals)

# MAPE CALCULATION

eu.profit.for.eval <- consumer.eu[c(43:48),]

eu.profit.global.eval <-
  predict(eu.profit.lmfit, data.frame(Month = c(43:48)))

temp <- eu.profit.for.eval[, 7]

eu.profit.MAPE <-
  accuracy(eu.profit.global.eval, as.integer(temp$Profit))[, 5]

eu.profit.MAPE

## ARIMA FIT

eu.profit.auto.arima <- auto.arima(eu.profit.model.ts)
eu.profit.auto.arima

tsdiag(eu.profit.auto.arima)

plot(eu.profit.auto.arima$x)
lines(fitted(eu.profit.auto.arima), col = "darkred")

eu.profit.auto.arima.residual <-
  eu.profit.model.ts - fitted(eu.profit.auto.arima)

# ADF & KPSS TEST

adf.test(eu.profit.auto.arima.residual, alternative = "stationary")

kpss.test(eu.profit.auto.arima.residual)

# MAPE CALCULATION

eu.profit.global.auto.arima <- forecast(eu.profit.auto.arima, h = 6)

temp <- eu.profit.for.eval[, 7]

eu.profit.MAPE.arima <-
  accuracy(eu.profit.global.auto.arima, as.integer(temp$Profit))[, 5]

eu.profit.MAPE.arima

# MODEL NAMES:
# eu.profit.global.auto.arima : MAPE 72.59, 38.99
# eu.profit.lmfit : 241.07

########## PART 3 - FORECASTING

##### CONSUMER_APAC

## SALES

# ARIMA

apac.sales.forecast.arima <- forecast(apac.sales.auto.arima, h = 6)
apac.sales.forecast.arima
autoplot(apac.sales.forecast.arima) + theme_minimal() + labs(x = "", y = "")

# LMFIT PREDICTIONS

apac.sales.forecast.lmfit <-
  predict(apac.sales.lmfit, data.frame(Month = c(49:54)))
apac.sales.forecast.lmfit

## QTY

# ARIMA

apac.qty.forecast.arima <- forecast(apac.qty.auto.arima, h = 6)
apac.qty.forecast.arima
autoplot(apac.qty.forecast.arima) + theme_minimal() + labs(x = "", y = "")

# LMFIT PREDICTIONS

apac.qty.forecast.lmfit <-
  predict(apac.qty.lmfit, data.frame(Month = c(49:54)))
apac.qty.forecast.lmfit

## PROFIT

# ARIMA

apac.profit.forecast.arima <-
  forecast(apac.profit.auto.arima, h = 6)
apac.profit.forecast.arima
autoplot(apac.profit.forecast.arima) + theme_minimal() + labs(x = "", y = "")

# LMFIT PREDICTIONS

apac.profit.forecast.lmfit <-
  predict(apac.profit.lmfit, data.frame(Month = c(49:54)))
apac.profit.forecast.lmfit

##### CONSUMER_EU

## SALES

# ARIMA

eu.sales.forecast.arima <- forecast(eu.sales.auto.arima, h = 6)
eu.sales.forecast.arima
autoplot(eu.sales.forecast.arima) + theme_minimal() + labs(x = "", y = "")

# LMFIT PREDICTIONS

eu.sales.forecast.lmfit <-
  predict(eu.sales.lmfit, data.frame(Month = c(49:54)))
eu.sales.forecast.lmfit

## QTY

# ARIMA

eu.qty.forecast.arima <- forecast(eu.qty.auto.arima, h = 6)
eu.qty.forecast.arima
autoplot(eu.qty.forecast.arima) + theme_minimal() + labs(x = "", y = "")

  # LMFIT PREDICTIONS

eu.qty.forecast.lmfit <-
  predict(eu.qty.lmfit, data.frame(Month = c(49:54)))
eu.qty.forecast.lmfit

## PROFIT

# ARIMA

eu.profit.forecast.arima <- forecast(eu.profit.auto.arima, h = 6)
eu.profit.forecast.arima
autoplot(eu.profit.forecast.arima) + theme_minimal() + labs(x = "", y = "")

# LMFIT PREDICTIONS

eu.profit.forecast.lmfit <-
  predict(eu.profit.lmfit, data.frame(Month = c(49:54)))
eu.profit.forecast.lmfit
