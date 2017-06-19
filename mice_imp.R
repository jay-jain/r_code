# Using airquality dataset
data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

# Removing categorical variables
data <- airquality[-c(5,6)]
summary(data)

#-------------------------------------------------------------------------------
# Look for missing > 5% variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}

# Check each column
apply(data,2,pMiss)

# Check each row
apply(data,1,pMiss)

#-------------------------------------------------------------------------------
# Missing data pattern
library(mice)

# Missing data pattern
md.pattern(data)

library(VIM)
# Plot of missing data pattern
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Box plot
marginplot(data[c(1,2)])

#-------------------------------------------------------------------------------
# Impute missing data using mice

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

# Get imputed data (for the Ozone variable)
tempData$imp$Ozone

# Possible imputation models provided by mice() are
methods(mice)

# What imputation method did we use?
tempData$meth

# Get completed datasets (observed and imputed)
completedData <- complete(tempData,1)
summary(completedData)

#-------------------------------------------------------------------------------
# Plots
library(lattice)
# Scatterplot Ozone vs all
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

# Density plot original vs imputed dataset
densityplot(tempData)

# Another take on the density: stripplot()
stripplot(tempData, pch = 20, cex = 1.2)

#-------------------------------------------------------------------------------
# Pooling the results and fitting a linear model

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
pool(modelFit1)
summary(pool(modelFit1))

# Using more imputed datasets
tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))