# Jay Jain
# June 19, 2017
# The structure of this script is taken heavily from the following source: https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
# TODO: troubleshoot xyplot and density plot.

fia_traitmeans <- read.csv('try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

# Use the columns with the least amount of missing data: Plant tolerance to shade, Rooting depth, Seed dry mass 
fia_traitmeans <- fia_traitmeans[,c(15,16,17)]
init = mice(fia_traitmeans,maxit=0)
meth = init$method
predM = init$predictorMatrix
# meth[c("Scientific Name")]=""
set.seed(103)

imputed <- mice(fia_traitmeans, method=meth, predictorMatrix=predM, m=5)
imputed <- complete(imputed)

# Rooting depth 


# Check for missing data in dataset
sapply(imputed, function(x) sum(is.na(x)))