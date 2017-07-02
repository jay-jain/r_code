# https://www.researchgate.net/post/Is_there_anybody_who_has_an_experience_with_yaimpute_package_for_R

library(yaImpute)
library(raster)

b <- brick(ncol=100,nrow=100,nl=10)
b[] <- rnorm(1e5)

# Training matrix and corresponding class labels:
train <- matrix(rnorm(100), 10, 10)
labels <- c(rep(1,5),rep(2,5))

# find nearest neighbours in training matrix for each pixel in b:
x <- getValues(b)
neigh <- ann(train, x, k=1)

# save classification result to new raster
result <- raster(b)
result <- setValues(result, labels[neigh$knnIndexDist[,1]])
plot(result)