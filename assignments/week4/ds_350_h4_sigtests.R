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
auto.price$lnprice = log(auto.price$price)

## Test significance
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

