# Accessed: June 19, 2017
# mice imputation example 
# Code is from the following source:
# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html

library(mice)
library (VIM)
library(lattice)
data(nhanes)
str(nhanes)
nhanes$age = factor(nhanes$age)

md.pattern(nhanes) 

# 5 patterns observed from 2^3 possible patterns; we see for example that there are 3 cases where chl is missing whereas all the other variables are observed.

# Missingness pattern can also be visualised in VIM package by
library(VIM)
nhanes_aggr = aggr(nhanes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# This plot gives the frequencies for different combination of variables missing. For example,
# that all the three variables chl, bmi & hyp are missing is the most frequent with about 28% frequency (7 observations). Note that, blue refers to observed data and red to the missing data. 

# You can also try it in mi package by
missing.pattern.plot(nhanes, mis.col=mdc(2), obs.col=mdc(1), main="Missing Pattern")

# The margin plot of the pairs can be plotted using VIM package as
marginplot(nhanes[, c("chl", "bmi")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# blue box plots summarise the distribution of observed data given the other variable is observed, 
# and red box plots summarise the distribution of observed data given the other variable is missing.
# For example, the red box plot in the bottom margin shows that bmi is rather missing for lower cholesterol levels. 
# Note that, if data are MCAR, we expect the blue and red box plots to be identical.

## If interested, you can also use VIM for scatterplots as
# scattMiss(nhanes[,c("bmi","chl")], inEllipse=TRUE, col=mdc(1:2),alpha=.8,bty="n", interactive=TRUE, axes=TRUE, lwd=c(1.5,1.5), pch=19, cex.lab=1.1, cex.axis=.9)
# rugNA(nhanes[,c("bmi")], nhanes[,c("chl")], side=2, col=mdc(2), lwd=1.5)
# rugNA(nhanes[,c("bmi")], nhanes[,c("chl")], side=1, col=mdc(2), lwd=1.5)


# We are interested in the simple linear regression of chl on age and bmi

# The naive approach:
# Delete cases with missing values (complete case analysis), and fit a linear model,
# lm function does these two steps together, however, remember that it is not a valid method for MAR data.
fit.cc = lm(chl ~ age + bmi, data=nhanes)
summary(fit.cc)



# IMPUTATION 

# Function mice() in mice package is a Markov Chain Monte Carlo (MCMC) method that uses 
# correlation structure of the data and imputes missing values for each incomplete 
# variable m times by regression of incomplete variables on the other variables iteratively. 
imp = mice(nhanes, m=5, printFlag=FALSE, maxit = 40, seed=2525)
# The output imp contains m=5 completed datasets. Each dataset can be analysed
# using function with(), and including an expression for the statistical analysis approach
# we want to apply for each imputed dataset as follows
fit.mi = with(data=imp, exp = lm(chl ~ age + bmi))

# Next, we combine all the results of the 5 imputed datasets using the pool() function
combFit = pool(fit.mi) 
# Note that the function pool() works for any object having BOTH coef() and vcov() methods, such as lm, glm and Arima, also for lme in nlme package.
round(summary(combFit),2)


# Increase the number of imputations to m=20
imp20 = mice(nhanes, m=20, printFlag=FALSE, maxit = 30, seed=2525)
fit.mi20 = with(data=imp20, exp = lm(chl ~ age + bmi))
combFit = pool(fit.mi20)
round(summary(combFit),2)

# The results are not much changed. MI works for as low as m=5 for this example.

# You can also see the pooled adjusted R-squared as
pool.r.squared(fit.mi)

# Check for implausible imputations (values that are clearly impossible, e.g. negative values for bmi)
# The imputations, for example for bmi, are stored as
imp$imp$bmi

# Completed datasets (observed and imputed), for example the second one, can be extracted by
imp_2 = complete(imp, 2)

# We can inspect the distributions of the original and the imputed data:
## scatterplot (of chl and bmi) for each imputed dataset
xyplot(imp, bmi ~ chl | .imp, pch = 20, cex = 1.4)


# Blue represents the observed data and red shows the imputed data. These colours are consistent with what they represent from now on. 
# Here, we expect the red points (imputed data) have almost the same shape as blue points
# (observed data). Blue points are constant across imputed datasets, but red points differ
# from each other, which represents our uncertainty about the true values of missing data.

## To detect interesting differences between observed and imputed data
densityplot(imp)

# This plot compares the density of observed data with the ones of imputed data. We expect them
# to be similar (though not identical) under MAR assumption.

# Distributions of the variables as individual points can be visualised by
# stripplot(imp, pch = 20, cex = 1.2); imputations should be close to the data; we do not expect to see see 
#     impossible values like for example negative values for bmi or 1.5 for hyp

# CONVERGENCE MONITORING
# MICE runs m parallel chains, each with a certain number of iterations, and imputes
# values from the final iteration. How many iterations does mice() use and how can we make sure that
# this number is enough?
# To monitor convergence we can use trace plots, which plot estimates against the number of iteration.
plot(imp20) 

# shows mean and standard deviation of the variables through the iterations for the m 
# imputed datasets. An indicator of convergence is how well the m parallel chains mix. We can also see here for example that mice
#     has treated hyp as a continuous variable.

# See the univariate imputation model for each incomplete variable that mice() used
# for your data as default
imp$meth

# Possible imputation models provided by mice() are
methods(mice)

# For example, logreg stands for Bayesian logistic regression for binary incomplete variable.
# You can run ?mice.impute.logreg to get more information whether the imputation model is
# suitable for the incomplete variable you want to impute.

# Since hyp is binary we want to change the default; also we might want to change the imputation
# model for bmi to (Bayesian) normal linear model:
meth=imp$meth; 
meth = c("", "norm", "logreg", "pmm")
# you might get an error when running mice if the method you specify is not consistent with the 
# type of the variable specified in the dataset. So, for this specified method, you also need to
# change the type of hyp to a 2-level factor or a yes/no binary variable.
nhanes$hyp = factor(nhanes$hyp)

imp$pred

# by default mice() includes all the variables in the data; it is suggested that we use as many
# variables as possible. Do not exclude a variable as predictor from the imputation model if
# that variable is going be included in your final analysis model (either as response or predictor) in function with().

# We can specify relevant predictors as follows:
# Suppose that hyp is considered irrelevant as a predictor for bmi and chl!
pred=imp$pred;
pred[, "hyp"] = 0
pred


vis=imp$vis; vis

# mice() imputes values according to the column sequence of variables in the dataset, from left
# to right. Here, for a specifed iteration of the mice sampler, first bmi is imputed, then
# hyp is imputed using the currently imputed bmi and previously imputed chl from the previous
# iteration, and so on.

# You can choose visiting sequence by increasing order of missing data manually as
vis = c(3,2,4) 
# , or alternatively
vis="monotone"
# However, if there is no specific pattern in the missingness (we say the missingness pattern
# is general of arbitrary), we do not expect the visiting sequence to affect the results much.

# mice provides Wald test for testing models. It is also possible to do likelihood ratio test if
# the final analysis model is logistic regression. You can choose the test in pool.compare() function by "method" option.

fit1 = with(data = imp20, expr = lm(chl ~ age))
fit2 = with(data = imp20, expr = lm(chl ~ age + bmi))
# Wald test
stat = pool.compare(fit2, fit1, method = "Wald")
# P-value of the test
stat$p