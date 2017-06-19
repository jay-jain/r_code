# Jay Jain
# June 16, 2017
# The structure of this script is taken heavily from the following source: https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/ 
# TODO: troubleshoot xyplot and density plot.
# multivariate imputation by chained equations  (MICE)

setwd("/home/jay/Desktop/nasabio/")

fia_traitmeans <- read.csv('try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)
#fia_traitmeans <- fia_traitmeans[,c(15,17)]

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(fia_traitmeans,2,pMiss)
apply(fia_traitmeans,1,pMiss)

library(mice)
md.pattern(fia_traitmeans)

library(VIM)
aggr_plot <- aggr(fia_traitmeans, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(fia_traitmeans), gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(fia_traitmeans[c(2,3)])

# Note: I picked columns 15 and 17 because they had the least amount of missing values
tempData <- mice(fia_traitmeans,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

# See what method was used on the traits
tempData$meth

# View the imputed values for the two traits 
#tempData$imp$`Plant tolerance to shade`
#tempData$imp$`Seed dry mass`

xyplot(tempData)

#xyplot(tempData,`Plant tolerance to shade` ~ `Seed dry mass`,pch=18,cex=1)

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)


# Pooling

# modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
# summary(pool(modelFit1))

# tempData2 <- mice(data,m=50,seed=245435)
# modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
# summary(pool(modelFit2))