#### Assignment 5 ####
## Assignment: 
#Apply bootstrap resampling to the auto price data as follows:
# Compare the difference of the bootstrap resampled mean of the log price of autos grouped by 1) aspiration and 2) fuel type. Use both numerical and graphical methods for your comparison. Are these means different within a 95% confidence interval? How do your conclusions compare to the results you obtained using the t-test last week?
# Compare the differences of the bootstrap resampled mean of the log price of the autos grouped by body style. You will need to do this pair wise; e.g. between each possible pairing of body styles. Use both numerical and graphical methods for your comparison. Which pairs of means are different within a 95% confidence interval? How do your conclusions compare to the results you obtained from the ANOVA and Tukey’s HSD analysis you performed last week?

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
## Function to plot t
plot.t <- function(a, b, aTitle, bTitle, nbins = 80, p = 0.05){
  cols = c(aTitle,bTitle)
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  par(mfrow = c(2, 1))
  plot.hist(a, maxs, mins, cols = cols[1])
  plot.hist(b, maxs, mins, cols = cols[2])
  par(mfrow = c(1, 1))
}
## Function to plot the difference of means
plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}
## Function to plot difference of means
plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  plot.hist(a, maxs, mins, cols = cols[1])
}

# Required Libraries
if(!require(dplyr)){install.packages("deplyr")}
if(!require(HistData)){install.packages("HistData")}
if(!require(resample)){install.packages("resample")}
if(!require(simpleboot)){install.packages("simpleboot")}
if(!require(repr)){install.packages("repr")}

# Read Auto data file
auto.price = read.auto()
# Add log normal of price as new attribute
auto.price$lnprice = log(auto.price$price)

########################################################################################################
##  1.) Compare the difference of the bootstrap resampled mean of the log price of autos.
# -grouped by:
#  a.) aspiration
#  b.) fuel type

## a.) Auto Prices by Aspiration
autoPricesByAspiration = auto.price[,c('price','lnprice','aspiration')]
#summary(autoPricesByAspiration)
autoPricesByAspiration.group = autoPricesByAspiration %>% group_by(aspiration) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))
autoPricesByAspiration.group

# Bootstrap the mean of the std and of turbo cars
std.lnprices = autoPricesByAspiration[autoPricesByAspiration$aspiration == 'std',]$lnprice
mean.boot.std = one.boot(std.lnprices,mean,R=1000000)
turbo.lnprices = autoPricesByAspiration[autoPricesByAspiration$aspiration == 'turbo',]$lnprice
mean.boot.turbo = one.boot(turbo.lnprices,mean,R=1000000)

# Plot the Bootstrap mean of auto prices by STD Aspiration and TURBO Aspiration
plot.t(mean.boot.std$t,mean.boot.turbo$t,'Population STD Aspiration','Population TURBO Aspiration',nbins=80)

## Bootstrap the difference in means
options(repr.plot.width=6, repr.plot.height=4)

# Aspiration, std versus turbo
mean.two.boot.aspiration = two.boot(std.lnprices, turbo.lnprices, mean, R = 100000)
plot.diff(mean.two.boot.aspiration$t)

## The distribution of the bootstrap means do not overlap. Their differenc is significant.

## b.) Auto Prices by Fuel Types
autoPricesByFuelType = auto.price[,c('price','lnprice','fuel.type')]
autoPricesByFuelType.group = autoPricesByFuelType %>% group_by(fuel.type) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

# Bootstrap the mean of the gas and of diesal
gas.lnprices = autoPricesByFuelType[autoPricesByFuelType$fuel.type == 'gas',]$lnprice
mean.boot.gas = one.boot(gas.lnprices,mean,R=1000000)
diesel.lnprices = autoPricesByFuelType[autoPricesByFuelType$fuel.type == 'diesel',]$lnprice
mean.boot.diesel = one.boot(diesel.lnprices,mean,R=1000000)

# Plot the Bootstrap mean of auto prices by gas fuel type and diesel fuel type
plot.t(mean.boot.gas$t,mean.boot.diesel$t,'Population gas Fuel Type','Population diesel Fuel Type',nbins=80)


## Bootstrap the difference in means
options(repr.plot.width=6, repr.plot.height=4)

# Aspiration, std versus turbo
mean.two.boot.aspiration = two.boot(std.lnprices, turbo.lnprices, mean, R = 100000)
plot.diff(mean.two.boot.aspiration$t)

## Is the bootstrapped distribution Normal?
options(repr.plot.width=6, repr.plot.height=6)
qqnorm(mean.two.boot.aspiration$t, main = 'Quantiles of standard Normal \nvs.\n bookstrapped mean aspirations')

# Fuel Type, Gas versus Diesel
## Bootstrap the difference in means
options(repr.plot.width=6, repr.plot.height=4)
mean.two.boot.fueltype = two.boot(gas.lnprices, diesel.lnprices, mean, R = 100000)
plot.diff(mean.two.boot.fueltype$t)

## Is the bootstrapped distribution Normal?
options(repr.plot.width=6, repr.plot.height=6)
qqnorm(mean.two.boot.fueltype$t, main = 'Quantiles of standard Normal \nvs.\n bookstrapped mean fuel types')



########################################################################################################
## 2.) Compare the differences of the bootstrap resampled mean of the log price of the autos grouped by body style
########################################################################################################

autoPricesByBodyStyle = auto.price[,c('price','lnprice','body.style')]
autoPricesByBodyStyle.group = autoPricesByBodyStyle %>% group_by(body.style) %>% 
  summarise(count=n(),mean.price = mean(price,na.rm=TRUE),mean.lnprice = mean(lnprice,na.rm=TRUE), sd.price = sd(price,na.rm=TRUE), sd.lnprice = sd(lnprice,na.rm=TRUE), 
            max.price = max(price), max.lnprice = max(lnprice), min.price=min(price), min.lnprice=min(lnprice))

# Boxplot the body style
boxplot(autoPricesByBodyStyle$lnprice ~ autoPricesByBodyStyle$body.style)

# ANOVA of Body Style to Log Normal auto price
bodyStyle_aov = aov(autoPricesByBodyStyle$lnprice ~ autoPricesByBodyStyle$body.style)
summary(bodyStyle_aov)
print(bodyStyle_aov)

# Tukey ANOVA of Body Style Prices
bodyStyles_tuk = TukeyHSD(bodyStyle_aov)
# Print model data
bodyStyles_tuk
# Plot
plot(bodyStyles_tuk)

##### hardtop-convertible ####
# Bootstrap the mean of the hardtop and of convertable body styles
# Get the body types lnprices
hardtop.lnprices = autoPricesByBodyStyle[autoPricesByBodyStyle$body.style == 'hardtop',]$lnprice
convertible.lnprices = autoPricesByBodyStyle[autoPricesByBodyStyle$body.style == 'convertible',]$lnprice
hatchback.lnprices = autoPricesByBodyStyle[autoPricesByBodyStyle$body.style == 'hatchback',]$lnprice
sedan.lnprices = autoPricesByBodyStyle[autoPricesByBodyStyle$body.style == 'sedan',]$lnprice
wagon.lnprices = autoPricesByBodyStyle[autoPricesByBodyStyle$body.style == 'wagon',]$lnprice

# Get the body types mean boots
mean.boot.hardtop = one.boot(hardtop.lnprices,mean,R=1000000)
mean.boot.convertible = one.boot(convertible.lnprices,mean,R=1000000)
mean.boot.hatchback = one.boot(hatchback.lnprices,mean,R=1000000)
mean.boot.sedan = one.boot(sedan.lnprices,mean,R=1000000)
mean.boot.wagon = one.boot(wagon.lnprices,mean,R=1000000)

# Run the plots
##### hardtop-convertible ####
plot.t(mean.boot.hardtop$t,mean.boot.convertible$t,'Population hardtop Body Style','Population convertible Body Style',nbins=80)

##### hatchback-convertible ####
plot.t(mean.boot.hatchback$t,mean.boot.convertible$t,'Population hatchback Body Style','Population convertible Body Style',nbins=80)

##### sedan-convertible ####
plot.t(mean.boot.sedan$t,mean.boot.convertible$t,'Population sedan Body Style','Population convertible Body Style',nbins=80)

##### wagon-convertible ####
plot.t(mean.boot.wagon$t,mean.boot.convertible$t,'Population wagon Body Style','Population convertible Body Style',nbins=80)

##### hatchback-hardtop ####
plot.t(mean.boot.hatchback$t,mean.boot.hardtop$t,'Population hatchback Body Style','Population hardtop Body Style',nbins=80)

##### sedan-hardtop ####
plot.t(mean.boot.sedan$t,mean.boot.hardtop$t,'Population sedan Body Style','Population hardtop Body Style',nbins=80)

##### wagon-hardtop ####
plot.t(mean.boot.wagon$t,mean.boot.hardtop$t,'Population wagon Body Style','Population hardtop Body Style',nbins=80)

##### sedan-hatchback ####
plot.t(mean.boot.sedan$t,mean.boot.hatchback$t,'Population sedan Body Style','Population hatchback Body Style',nbins=80)

##### wagon-hatchback ####
plot.t(mean.boot.wagon$t,mean.boot.hatchback$t,'Population wagon Body Style','Population hatchback Body Style',nbins=80)

##### wagon-sedan ####
plot.t(mean.boot.wagon$t,mean.boot.sedan$t,'Population wagon Body Style','Population sedan Body Style',nbins=80)


