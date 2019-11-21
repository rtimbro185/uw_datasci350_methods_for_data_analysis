#### Assignment 4 ####
## 1.) Compare and test Normality of the distributions of price and log price - use both a graphical method and a formal test
## 2.) Test significance of price (log price) stratified by: a) fuel type, b) aspiration, c) rear vs. front wheel drive - use both graphical methods and the fromal test.
## 3.) Apply ANOVA to the auto price data to compare the price (or log price if closer to a Normal distribution) of autos stratified by number of doors, and body style - two sets of tests
##     -Graphically explore the differences between the price conditioned by the categories of each variable
##     -Use standard ANOVA and Tukey ANOVA to test the differences of these groups.
rm(list=ls())
cat('\014')
## Read Automobile price data
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. 
  auto.price[complete.cases(auto.price), ]
}
plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}
# Required Libraries
require(dplyr)

# Read Auto data file
auto.price = read.auto()
# Add log normal of price as new attribute
auto.price$lnprice = log(auto.price$price)

########################################################################################################
##  1.)Compare and test Normality of the distributions of price and log price
########################################################################################################
#
# Check for normality.
# Test normality of two equal samples of prices
price.s1 = sample(auto.price$price,50,replace=FALSE)
price.s2 = sample(auto.price$price,50,replace=FALSE)

# Q-Q Plot Normality of distribution of auto price sample sets
par(mfrow = c(1,2))
qqnorm(price.s1, main='Q-Q plot of sample 1 auto prices')
qqline(price.s1,col='red')
qqnorm(price.s2, main='Q-Q plot of sample 2 auto prices')
qqline(price.s2,col='blue')
par(mfrow = c(1,1))

## Test normality of two equal samples of prices
lnprice.s1 = sample(auto.price$lnprice,50,replace=FALSE)
lnprice.s2 = sample(auto.price$lnprice,50,replace=FALSE)

# Q-Q Plot Normality of distribution of log normal auto price sample sets
par(mfrow = c(1,2))
qqnorm(lnprice.s1, main='Q-Q plot of sample 1 auto log normal prices')
qqline(lnprice.s1,col='red')
qqnorm(lnprice.s2, main='Q-Q plot of sample 2 auto log normal prices')
qqline(lnprice.s2,col='blue')
par(mfrow = c(1,1))

# plot log normal price sample one distrobution against log normal price sample distribution 2
plot(sort(price.s1), sort(price.s2), main = "Plot of price sample 1 vs price sample 2",
     xlab='Quantiles of auto prices sample 1', ylab='Quantiles of auto prices sample 2')
abline(a=0.0,b=1.0,lty=2,col='blue')

# plot log normal price sample one distrobution against log normal price sample distribution 2
plot(sort(lnprice.s1), sort(lnprice.s2), main = "Plot of log normal price sample 1 vs log normal price sample 1e",
     xlab='Quantiles of log normal auto prices sample 1', ylab='Quantiles of log normal auto prices sample 2')
abline(a=0.0,b=1.0,lty=2,col='blue')

# Plot Kolmogorov-Smirnov test. K-S statistic is the maximum vertical distance between two CDFs.
plot(ecdf(lnprice.s1), col='blue', main='CDFs of Sample Log Normal Prices', 
     xlab='Value', ylab='Cumulative density')
lines(ecdf(lnprice.s2), col='red')

# Standardize the x-values of Log Normal Prices
# Have to standardize the x-values
x_seq = seq(-3,3,len=100)
y_cdf1 = sapply(x_seq, function(x){
  sum(lnprice.s1<x)/length(lnprice.s1)
})
y_cdf2 = sapply(x_seq, function(x){
  sum(lnprice.s2<x)/length(lnprice.s2)
})

plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16) 

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

##----Repeat N Times-----
N = 1000
k_s_rep = sapply(1:N, function(i){
  return(ks_stat(-3, 3, lnprice.s1, lnprice.s2))
})

hist(k_s_rep, breaks=30, freq=FALSE, xlab = 'K-S statistic',
     main = 'Histogram of k-s statistic')
lines(density(k_s_rep))

k_s_stat = ks_stat(-5,5, y_cdf1, y_cdf2)
k_s_stat
empirical_p_value = k_s_stat/1000
empirical_p_value

########################################################################################################
## 2.) Test significance
########################################################################################################
#
# Create a subset of the data frame attributes to compare
auto.price.sig.sub = auto.price[,c('price','lnprice','fuel.type','aspiration','drive.wheels')]
auto.price.sig.sub = auto.price.sig.sub %>% filter(drive.wheels == 'rwd' | drive.wheels == 'fwd')

# Auto Prices by Fuel Types
autoPricesByFuelType = auto.price.sig.sub[,c('price','lnprice','fuel.type')]
autoPricesByFuelType.group = autoPricesByFuelType %>% group_by(fuel.type) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

## Is the Average price of diesel cars different from gas cars?
diesel.lnprices = autoPricesByFuelType[autoPricesByFuelType$fuel.type == 'diesel',]$lnprice
gas.lnprices = autoPricesByFuelType[autoPricesByFuelType$fuel.type == 'gas',]$lnprice
plot.t(diesel.lnprices,gas.lnprices, cols=c('Log Normal Prices of Diesel Fueled Cars','Log Normal Prices of Gas Fueled Cars'))
t.test(diesel.lnprices,gas.lnprices, alternative = "two.sided")

# Auto Prices by Aspiration
autoPricesByAspiration = auto.price.sig.sub[,c('price','lnprice','aspiration')]
autoPricesByAspiration.group = autoPricesByAspiration %>% group_by(aspiration) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

## Is the Average price of standard cars different from turbo cars?
std.lnprices = autoPricesByAspiration[autoPricesByAspiration$aspiration == 'std',]$lnprice
turbo.lnprices = autoPricesByAspiration[autoPricesByAspiration$aspiration == 'turbo',]$lnprice
plot.t(std.lnprices,turbo.lnprices, cols=c('Log Normal Prices of Standard Cars','Log Normal Prices of Turbo Cars'))
t.test(std.lnprices,turbo.lnprices, alternative = "two.sided")

# Auto Prices by Front Wheel Drive
autoPricesByWheelDrive = auto.price.sig.sub[,c('price','lnprice','drive.wheels')]
autoPricesByFrontWheelDrive.group = autoPricesByWheelDrive %>% filter(drive.wheels == 'fwd') %>% group_by(drive.wheels) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

# Auto Prices by Rear Wheel Drive
autoPricesByRearWheelDrive.group = autoPricesByWheelDrive %>% filter(drive.wheels == 'rwd') %>% group_by(drive.wheels) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),sd.price=sd(price,na.rm=TRUE),max.price=max(price),min.price=min(price))

## Is the Average price of a front wheel drive car different then a rear wheel drive car
fwd.lnprices = autoPricesByWheelDrive[autoPricesByWheelDrive$drive.wheels == 'fwd',]$lnprice
rwd.lnprices = autoPricesByWheelDrive[autoPricesByWheelDrive$drive.wheels == 'rwd',]$lnprice
plot.t(fwd.lnprices,rwd.lnprices, cols=c('Log Normal Prices of Front Wheel Drive Cars','Log Normal Prices of Rear Wheel Drive Cars'))
t.test(fwd.lnprices,rwd.lnprices, alternative = "two.sided")


##3.) Apply ANOVA to the auto price data to compare the price grouped by number of doors and body style
auto.price.avo.sub = auto.price[,c('price','lnprice','num.of.doors','body.style')]
autoPricesByNumOfDoors = auto.price.avo.sub %>% select(-body.style) %>% filter(num.of.doors != '?')

autoPricesByNumOfDoors.group = autoPricesByNumOfDoors %>% group_by(num.of.doors) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

autoPricesByBodyStyle = auto.price.avo.sub %>% select(-num.of.doors)
autoPricesByBodyStyle.group = autoPricesByBodyStyle %>% group_by(body.style) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))



# Boxplot the number of doors prices and body style
boxplot(autoPricesByNumOfDoors$lnprice ~ autoPricesByNumOfDoors$num.of.doors)
boxplot(autoPricesByBodyStyle$lnprice ~ autoPricesByBodyStyle$body.style)

# ANOVA of Number of Doors to Log normal auto price
numOfDoors_aov = aov(autoPricesByNumOfDoors$lnprice ~ autoPricesByNumOfDoors$num.of.doors)
summary(numOfDoors_aov)
print(numOfDoors_aov)

# ANOVA of Body Style to Log Normal auto price
bodyStyle_aov = aov(autoPricesByBodyStyle$lnprice ~ autoPricesByBodyStyle$body.style)
summary(bodyStyle_aov)
print(bodyStyle_aov)


# Tukey's ANOVA: 
# Note: Only differences in means with a confidence interval not overlapping zero are considered significant
# Tukey ANOVA of Number of Doors to Prices
numOfDoors_tuk = TukeyHSD(numOfDoors_aov)
# Print model data
numOfDoors_tuk
# Plot
plot(numOfDoors_tuk)

# Tukey ANOVA of Body Style Prices
bodyStyles_tuk = TukeyHSD(bodyStyle_aov)
# Print model data
bodyStyles_tuk
# Plot
plot(bodyStyles_tuk)

