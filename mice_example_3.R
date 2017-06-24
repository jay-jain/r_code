# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

library(missForest)
data <- iris
summary(iris)

# generate 10% missing values at Random 
iris.mis <- prodNA(iris,noNA = 0.1)

# check missing values introduced in the data
summary(iris.mis)

# remove categorical variables
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

# load mice package
library(mice)

# returns a tabular form of missing value present in each variable in a data set
md.pattern(iris.mis)

# create visual to represent missing values
library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(iris.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


# impute missing values 
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# check imputed values
imputed_Data$imp$Sepal.Width

# get complete data (2nd out of 5); There are 5 imputed sets
completeData <- complete(imputed_Data,2)

# build predictive model
fit <- with(data = iris.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width)) 

# combine results of all 5 models
combine <- pool(fit)
summary(combine)