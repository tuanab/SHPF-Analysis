
setwd("C:/Users/kevbu/Desktop/Shopify/rawdata")

file <- read.csv(file = "data.csv", stringsAsFactors = FALSE, na.strings = (""))
str(file)

### EXPLORATORY ANALYSIS

# Check for missing values
library(Amelia)
missmap(file, main = "missing value vs observed")
sapply(file, function(x) sum(is.na(x)))   # clean

# Histogram with original data 
binNorm <- (max(file$order_amount) - min(file$order_amount))/100
ggplot(file,aes(order_amount)) + 
  geom_histogram(binwidth = binNorm,col=I("blue")) # highly skewed

# Histogram by logs
library(ggplot2)
file$log <- log(file$order_amount)
binLog <- (max(file$log) - min(file$log))/100
ggplot(file,aes(log)) + geom_histogram(binwidth = binLog, col=I("blue"))


# Histogram with binned outliers (capping and coloring the outliers)

library(magrittr) # allows use of chain %%
library(dplyr)    # allows mutate
colors <- c(rep(I("blue"),107), rep(I("red"),1)) # isolate the outliers by red
file %>% 
  mutate(order_amt = ifelse(file$order_amount > 1000, 1000, file$order_amount)) %>% 
  ggplot(aes(order_amt)) + geom_histogram(binwidth = 8.5, fill = colors)
  
# Histogram with removed outliers 

upperbound <- as.numeric(IQR(file$order_amount) * 1.5 + quantile(file$order_amount,0.75))
lowerbound <- as.numeric(quantile(file$order_amount,0.25) - IQR(file$order_amount) * 1.5) 

file %>% 
  mutate(order_amt = ifelse(file$order_amount > upperbound | file$order_amount < lowerbound , NA, file$order_amount)) %>% 
  ggplot(aes(order_amt)) + geom_histogram(binwidth = 8.5,col=I("blue"))

# Boxplot with removed outliers
file %>% 
  mutate(order_amt = ifelse(file$order_amount > upperbound | file$order_amount < lowerbound , NA, file$order_amount)) %>% 
  ggplot(aes(x=factor(0),y=order_amt, group=1)) + 
  stat_boxplot(geom ='errorbar', width=0.2) + geom_boxplot()

### FEATURE ENGINEERING AND DATA MANIPULATION

# Creating 2 dataframes for normal and oultliers
fileNorm <- filter(file,file$order_amount <= upperbound & file$order_amount >= lowerbound)
outliers <- filter(file,file$order_amount > upperbound | file$order_amount < lowerbound)

# Price per Pair mean comparison
require(dplyr)
outliers <- mutate(outliers,price_tag = order_amount/total_items)  # outliers
file <- mutate(file,price_tag = order_amount/total_items)  # raw price
fileNorm <- mutate(fileNorm,price_tag = order_amount/total_items)  # Norm. price

PriceComparison <- as.data.frame(round(c(mean(outliers$price_tag),mean(file$price_tag),mean(fileNorm$price_tag))),digits=2)
PriceComparison$Data <- c("outliers", "file", "fileNorm")
colnames(PriceComparison)[1] <- c("Mean")

# Aggregating variables by Shop_ID and plot
SalesByShopID <- aggregate(order_amount~shop_id,data=fileNorm,FUN=sum)
colnames(SalesByShopID)[2] <- "total_sales"

FrequencyByShopID <- aggregate(user_id~shop_id,data=fileNorm,FUN=length)
colnames(FrequencyByShopID)[2] <- "order_frequency"

ItemsbyShopID <- aggregate(total_items~shop_id,data=fileNorm,FUN=sum)

ShopIDfull <- cbind(ItemsbyShopID[1:2],FrequencyByShopID[2],SalesByShopID[2])
colnames(ShopIDfull)[4] <- c("total_sales")

# Histogram for price_tag
ShopIDfull$price_tag <- c(ShopIDfull$total_sales/ShopIDfull$total_items)
binShop <- (max(ShopIDfull$price_tag) - min(ShopIDfull$price_tag))/30
ggplot(ShopIDfull,aes(price_tag)) + geom_histogram(binwidth = binShop, color=I("blue"))

# Feature engineering for TIME,DATE, and DAY INTERVAL
fileNorm$Time <- sapply(fileNorm$created_at, FUN=function(x) 
{strsplit(x, split='[ ]')[[1]][2]}) 

require(chron) # split intervals in numeric terms instead of "morning,afternoon,night"
fileNorm$Time <- times(paste0(fileNorm$Time, "")) # append a chron "times" class column
breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24) / 24 # times are internally fractions of a day
labels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
fileNorm$timeInterval <- as.factor(cut(fileNorm$Time, breaks, labels, include.lowest = TRUE))

fileNorm$Date <- sapply(fileNorm$created_at, FUN=function(x) 
{strsplit(x, split='[ ]')[[1]][1]}) 
fileNorm$Date <- as.factor(fileNorm$Date)
fileNorm$Weekday <- as.factor(weekdays(as.Date(fileNorm$Date,'%Y-%m-%d')))

fileNorm$dummydate[fileNorm$Weekday=="Monday"] <- 0
fileNorm$dummydate[fileNorm$Weekday=="Tuesday"] <- 1
fileNorm$dummydate[fileNorm$Weekday=="Wednesday"] <- 2
fileNorm$dummydate[fileNorm$Weekday=="Thursday"] <- 3
fileNorm$dummydate[fileNorm$Weekday=="Friday"] <- 4
fileNorm$dummydate[fileNorm$Weekday=="Saturday"] <- 5  
fileNorm$dummydate[fileNorm$Weekday=="Sunday"] <- 6

seasonality <- aggregate(dummydate ~ shop_id, data=fileNorm,FUN=sum)
ShopIDfull$seasonality <- seasonality$dummydate

# Find user diversity 
diversity <- read.csv(file = "pivot_SHPF.csv", stringsAsFactors = FALSE, na.strings = (""))

diversity <- diversity[-c(78),]
ShopIDfull$user_base <- diversity$user_base 
ShopIDfull <- arrange(ShopIDfull,desc(user_base))
  # Most diversified: 19,71,53
  # Least diversified: 38,85,56

# Aggregating sales by WEEKDAY
SalesbyDay <- aggregate(order_amount~Weekday,data=fileNorm,FUN=sum)

require(ggplot2)  
ggplot(SalesbyDay, aes(x = reorder(Weekday, -order_amount), y = order_amount)) + 
  geom_bar(stat="identity",position="dodge",col=I("blue"))
            # Th -> Wed -> Tue -> Sat -> Fri -> Sun -> Mon

# Find most active users
ActiveUser <- aggregate(order_amount~user_id,data=fileNorm,FUN=sum)
ActiveUser <- arrange(ActiveUser,desc(order_amount))
# Most active: 718,787,868
# Least active: 812,902,939
ggplot(ActiveUser,aes(order_amount)) + geom_density(fill = "blue")

# Detecting seasonality,trend, and noise
SalesbyDate <- aggregate(order_amount~Date,data=fileNorm,FUN=sum)
SalesbyDate$Date <- as.character(as.Date(SalesbyDate$Date,"%Y-%m-%d"),"%Y%m%d")
require(dplyr)
SalesbyDate <- arrange(SalesbyDate,!desc(Date))

require(ggplot2)
ggplot(SalesbyDate, aes(Date,order_amount,group=1)) + geom_line() + 
  ggtitle('Line plot of Date vs. Sales') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

timeSerie_sales <- SalesbyDate$order_amount

library(forecast)
par(mfcol=c(2,2))
trend_sales = ma(timeSerie_sales, order = 4, centre = TRUE)
plot(as.ts(timeSerie_sales))
lines(trend_sales)
plot(as.ts(trend_sales))  # detect the trend

detrend_sales = timeSerie_sales - trend_sales
plot(as.ts(detrend_sales))  #detect the time series with seasonality and noise

m_sales = t(matrix(data = detrend_sales, nrow = 5))
seasonal_sales = colMeans(m_sales, na.rm = TRUE)
plot(as.ts(rep(seasonal_sales,4)))  # detect the seasonality

random_sales <- timeSerie_sales - trend_sales - seasonal_sales
plot(as.ts(random_sales))  # detect the noise


### MODELING for Shop_ID items prediction

# Split data for modeling
require(caTools)
set.seed(101)  
sample = sample.split(ShopIDfull$total_items, SplitRatio = 0.70)
train = subset(ShopIDfull, sample == TRUE)
test  = subset(ShopIDfull, sample == FALSE)

# RANDOM FOREST REGRESSION (response variable = total_items)

require(randomForest)
set.seed(400)
rf <- randomForest(total_items ~ user_base + order_frequency + price_tag +seasonality,
                   data=train, importance=TRUE, ntree=2000)
print(rf)
varImpPlot(rf) 

Prediction1 <- predict(rf,train)
train$prediction <- round(as.numeric(as.character(unlist(Prediction1)),digits=0))
rmse1 <- sqrt(mean((train$total_items-train$prediction)^2))
plot(train$total_items,train$prediction,col=c("red","blue"), xlab = "Actual_train", ylab = "Predicted_train")
abline(0, 1)

Prediction2 <- predict(rf,test)
test$prediction <- round(as.numeric(as.character(unlist(Prediction2)),digits=0))
rmse2 <- sqrt(mean((test$total_items - test$prediction)^2))
plot(test$total_items,test$prediction,col=c("red","blue"), xlab = "Actual_test", ylab = "Predicted_test")
abline(0, 1)

rmse2-rmse1 #slightly overfitted

# MULTIPLE LINEAR REGRESSION (response variable = total_items)
linear <- lm(total_items ~ order_frequency + price_tag + user_base + order_frequency + seasonality , data=train)
summary(linear)  

lin_prediction1 <- predict(linear, train)
train$lin_prediction <- round(as.numeric(as.character(unlist(lin_prediction1)),digits=0))
rmse3 <- sqrt(mean((train$total_items-train$lin_prediction)^2))
plot(train$total_items,train$lin_prediction,col=c("red","blue"), xlab = "Actual_train", ylab = "Predicted_train")
abline(0,1)

lin_prediction2 <- predict(linear, test)
test$lin_prediction <- round(as.numeric(as.character(unlist(lin_prediction2)),digits=0))

rmse4 <- sqrt(mean((test$total_items-test$lin_prediction)^2))
plot(test$total_items,test$lin_prediction,col=c("red","blue"), xlab = "Actual_test", ylab = "Predicted_test")
abline(0,1)

linear.red <- resid(linear)
plot(train$total_items, linear.red, ylab="Residuals", xlab="total items")
abline(0,0)    # residual plot is good 

rmse4 - rmse3 # small rmse, just right!!!

      #Multiple regression fits better than Random Forest for Shop_ID prediction


### MODELING for daily order_amount by transaction

# Random forest (response variable = order_amount)
require(caTools)
set.seed(300) 
sample = sample.split(fileNorm$total_items, SplitRatio = 0.75)
train = subset(fileNorm, sample == TRUE)
test  = subset(fileNorm, sample == FALSE)

require(randomForest) # is it possible to create a rule for result to be multiple of price_tag??? 
set.seed(500)
rfRaw <- randomForest(order_amount ~ Date + timeInterval + price_tag +
                        shop_id,data=train, importance=TRUE, mtry=1, ntree=2000)

print(rfRaw)
varImpPlot(rfRaw)

Prediction3 <- predict(rfRaw,train)
train$prediction <- round(as.numeric(as.character(unlist(Prediction3)),digits=0))
rmse5 <- sqrt(mean((train$order_amount - train$prediction)^2))
plot(train$prediction,train$order_amount,col=c("red","blue"),xlab = "Predicted_train", ylab = "Actual_train")

Prediction4 <- predict(rfRaw,test)
test$prediction <- round(as.numeric(as.character(unlist(Prediction4)),digits=0))
rmse6 <- sqrt(mean((test$order_amount - test$prediction)^2))
plot(test$prediction,test$order_amount,col=c("red","blue"),xlab = "Predicted_test", ylab = "Actual_test")

rmse5-rmse6
          # underfitted, error is still high, need to improve


# Run and compare models at the same time 
# Poisson distribution for order frequency
# Fill missing payment_method using features
# Look into outliers dataframe