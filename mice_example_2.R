# Jay Jain
# June 19, 2017
# The structure of this script is taken heavily from the following source: 
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

library(mice)
fia_traitmeans <- read.csv('/home/jay/Desktop/try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

# Remove first column (Species names)
#fia_traitmeans <- fia_traitmeans[,-c(1)]

# To see what percentage of columns are missing values:
sapply(fia_traitmeans, function(x) sum(is.na(x)))

# Most missing values
fia_traitmeans[which.max(sapply(fia_traitmeans, function(x) sum(is.na(x))))]

# Least missing values
fia_traitmeans[which.min(sapply(fia_traitmeans, function(x) sum(is.na(x))))]

#hist(sapply(fia_traitmeans, function(x) sum(is.na(x))),main='Distribution of missing values for tree traits', 
#     xlab = 'Percent missing values', ylab = 'Trait columns',border = 'black',col='pink')

# Use the following command to determine which traits have least missing values: 
sort(sapply(fia_traitmeans, function(x) sum(is.na(x))), decreasing = FALSE)

# 16: Plant height
# 18: Rooting depth
# 19: Seed dry mass
#17: Plant lifespan years

# Use the columns with the least amount of missing data: Plant height, Rooting depth, Seed dry mass 
#fia_traitmeans <- fia_traitmeans[,c(16,18,19)]

#print("The average values of columns 16, 18, and 19:")
#mean(fia_traitmeans$`Plant height`,na.rm = TRUE)
#mean(fia_traitmeans$`Rooting depth`, na.rm = TRUE)
#mean(fia_traitmeans$`Seed dry mass`, na.rm = TRUE)

init = mice(fia_traitmeans,maxit=50,method="pmm")
meth = init$method
predM = init$predictorMatrix
set.seed(103)

# Imputes missing values using Bayesian linear regression with 100 imputations
imputed <- mice(fia_traitmeans, method="pmm", m=5,predictorMatrix = predM) #predictorMatrix=predM

# converts mids object to vector form
imputed <- complete(imputed)

# Check for missing data in dataset
sapply(imputed, function(x) sum(is.na(x)))

hist(imputed, main='Distribution of missing values for tree traits after imputation', 
     xlab = 'Percent missing values', ylab = 'Trait columns', border = 'black', col='pink')

#plot(imputed)

#print("The average values of columns 16, 18, 19")
#mean(imputed$`Plant height`)
#mean(imputed$`Rooting depth`)
#mean(imputed$`Seed dry mass`)