#### Assignment 7 ####
## Assignment: 
#

# Clear memory and console 
rm(list=ls())
cat('\014')

# Require Libraries
if(!require(HistData)){install.packages("HistData")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(MASS)){install.packages("MASS")}
if(!require(glmnet)){install.packages("glmnet")}

## FUNCTION - Read Automobile price data
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm',
               'city.mpg', 'highway.mpg', 'curb.weight', 
               'wheel.base', 'width', 'engine.size', 
               'length', 'height')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. 
  auto.price = auto.price[complete.cases(auto.price), ]
  
  # Add log normal of price as new attribute
  auto.price$lnprice = log(auto.price$price)
  
  ## Scale the numeric columns
  auto.price[, numcols] = data.frame(lapply(auto.price[, numcols], scale))
  auto.price
  
  ## Remove Symbolizing and normalized losses columns
  drop.cols = c('symboling','normalized.losses')
  auto.price = auto.price[,!(names(auto.price) %in% drop.cols)]
}

## FUNCTION - Plot svd regression
plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$lnprice)
  SST <- sum((df$lnprice - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

# Read Auto data file
auto.price = read.auto()

# Computing the model price with all features
lm.lnprice = lm(lnprice ~ ., data = auto.price)
summary(lm.lnprice)
plot(lm.lnprice)

# stepAIC function from the MASS package to perform stepwise regression starting with the linear model including all features.
lm.step.lnprice = stepAIC(lm.lnprice, direction = 'both')
lm.step.lnprice$anova # Run an ANOVA on the results
summary(lm.step.lnprice) # Summary of the best model
plot(lm.step.lnprice)

# Create the model matrix for all features

#mm.lnprice = model.matrix(lnprice ~ make + num.of.doors + drive.wheels + height + curb.weight + 
 #                           engine.type + num.of.cylinders + engine.size + fuel.system + 
#                            stroke + horsepower + peak.rpm + city.mpg + highway.mpg + 
#                            price, data = auto.price)
mm.lnprice = model.matrix(lnprice ~ . - 1,data = auto.price)

# Check the dimensions
dim(mm.lnprice) 
# Quick sanity check
head(mm.lnprice) 


#compute the SVD of the model matrix
m.svd = svd(mm.lnprice)
#a look at the singular values
m.svd$d
# plot the singular values - identify fall-off
plot.sv = function(u){
  ln = length(u)
  plot(1:ln, u, col = 'red',
       main = ('Singular values'),
       xlab = 'Singular value order',
       ylab = 'Singular value')
}
plot.sv(m.svd$d)

#verify the singular vectors form an orthonomal basis
t(m.svd$u) %*% m.svd$u
m.svd$v %*% t(m.svd$v)

## Set some singular values to 0 and create the inverse matrix
test.multiple.thresholds = function(u)
  for(i in u){
    th = i
    m.svd.sub = sapply(m.svd$d, function(x) ifelse(x>th,1/x,0))
    print(m.svd.sub)
    #plot the values
    plot.sv(m.svd.sub)
    #create the inverse
    diag.inv = diag(m.svd.sub)
    ## Compute the pseudeo inverse
    pseudo.inv = m.svd$v %*% diag.inv %*% t(m.svd$u)
    
    ## Find the model coeficients
    beta =  pseudo.inv %*% auto.price$lnprice
    print(beta)
    
    ## Compute the predictions
    pred = mm.lnprice %*% beta
    
    ## Compute and plot the residuals and compute SSE
    resid = auto.price$lnprice - pred
    plot(auto.price$lnprice, resid)
    print(sum(resid*resid))
  }

thresholds = c(1,5,0.2)
test.multiple.thresholds(thresholds)

b = as.matrix(auto.price$lnprice)
test.multiple.nlamdas = function(u){
  for(i in u){
    mod.ridge.lasso = glmnet(mm.lnprice, b, family = 'gaussian', nlambda = i, alpha = 0.5)
    plot(mod.ridge.lasso, xvar = 'lambda', label = TRUE)
    plot(mod.ridge.lasso, xvar = 'dev', label = TRUE)
    
    #create score vector using the predict method on the modal and using the original model matrix
    auto.price$score = predict(mod.ridge.lasso,newx = mm.lnprice)[,i]
    auto.price$resids = auto.price$score - auto.price$lnprice
    
    #plot summary
    plot.svd.reg(auto.price)
  }
  
}
nlamnbdas = c(40,20,15,10)
test.multiple.nlamdas(nlamnbdas)



