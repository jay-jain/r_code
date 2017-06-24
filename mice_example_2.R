# Jay Jain
# June 19, 2017
# The structure of this script is taken heavily from the following source: 
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

library(mice)
fia_traitmeans <- read.csv('try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

# To see what percentage of columns are missing values:
# sapply(fia_traitmeans, function(x) sum(is.na(x)))

# Use the columns with the least amount of missing data: Plant tolerance to shade, Rooting depth, Seed dry mass 
fia_traitmeans <- fia_traitmeans[,c(15,16,17)]

print("The average values of columns 15, 16, and 17:")
mean(fia_traitmeans$`Plant tolerance to shade`,na.rm = TRUE)
mean(fia_traitmeans$`Rooting depth`, na.rm = TRUE)
mean(fia_traitmeans$`Seed dry mass`, na.rm = TRUE)


init = mice(fia_traitmeans,maxit=0)
meth = init$method
predM = init$predictorMatrix
# meth[c("Scientific Name")]=""
set.seed(103)

imputed <- mice(fia_traitmeans, method="norm")#, predictorMatrix=predM, m=100)
imputed <- complete(imputed)


# Check for missing data in dataset
sapply(imputed, function(x) sum(is.na(x)))

print("The average values of columns 15, 16, 17")
mean(imputed$`Plant tolerance to shade`)
mean(imputed$`Rooting depth`)
mean(imputed$`Seed dry mass`)