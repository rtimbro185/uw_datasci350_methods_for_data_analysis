#### Assignment 7 ####
## Assignment: 
#

# Clear memory and console 
rm(list=ls())
cat('\014')

# Require Libraries
if(!require(forecast)){install.packages("forecast")}
if(!require(repr)){install.packages("repr")}


## FUNCTION - Read CADairyProduction data
read.dairy = function(file = 'CADairyProduction.csv'){
  ## Read the csv file
  dairy.production <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  ## Remove cases or rows with missing values. 
  dairy.production = dairy.production[complete.cases(dairy.production), ]
  
  ## Icecream Production
  drop.cols = c('Cotagecheese.Prod','Milk.Prod')
  icecream.prod = dairy.production[,!(names(dairy.production) %in% drop.cols)]
  icecream.prod$date = as.Date(paste(icecream.prod$Year,icecream.prod$Month,01, sep='-'),"%Y-%b-%d")
  icecream.prod$Month = as.factor(icecream.prod$Month)
  return(icecream.prod)
}



icecream.prod = read.dairy()
str(icecream.prod)
#icecream.prod = icecream.prod$Icecream.Prod


ts.icecream.prod.1995 = icecream.prod[icecream.prod$Year == 1995,]
ts.icecream.prod.1996 = icecream.prod[icecream.prod$Year == 1996,]
ts.icecream.prod.1997 = icecream.prod[icecream.prod$Year == 1997,]
ts.icecream.prod.1998 = icecream.prod[icecream.prod$Year == 1998,]


sd1 = sd(ts.icecream.prod.1995$Icecream.Prod)
sd2 = sd(ts.icecream.prod.1996$Icecream.Prod)
sd3 = sd(ts.icecream.prod.1997$Icecream.Prod)
sd4 = sd(ts.icecream.prod.1998$Icecream.Prod)


ts.icecream.prod = ts(icecream.prod, start = 1995 , freq = 12)
attributes(ts.icecream.prod)
