# m = number of traits
# p = number of predictors
# n = # of species

# preds is a matrix of dimensions n x p
# X is a matrix of dimensions mn x m(p + 1)
# we add 1 to p to account for the intercept

makeX <- function(preds,m){
  p = ncol(preds) 
  n = nrow(preds)
  #X <- matrix(nrow=(m*nrow(preds)),ncol = m*(p+1), byrow = TRUE)
  bigList <- list()
  for(k in 1:n){
    mlist <- list()
    for(i in 1:m){
      
      X <-matrix(0,nrow=m,ncol=p+1) 
      X[i,] <- c(1,c(preds[k,]))
      mlist[[length(mlist) + 1]] <- X
    }
    ilist <- do.call(cbind,mlist)
    bigList[[k]] <- ilist
  }
  return(do.call(rbind,bigList))
}  

preds <- matrix(c(1,2,3,4),nrow=2,ncol=2,byrow = TRUE)

m = 4 # traits

test <- makeX(preds,m)

