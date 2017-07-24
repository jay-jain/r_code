data{
  int<lower=1> m;
  int<lower=1> n;
  int<lower=1> p;
  int<lower=1> q;
  matrix[m,n] A;
  matrix[p,q] B;
}
parameters{real<lower=0,upper=1> DUMMY;}
model{}
generated quantities{
  matrix[m*p,n*q] C;
  for (i in 1:m)
    for (j in 1:n)
      for (k in 1:p)
        for (l in 1:q)
          C[p*(i-1)+k,q*(j-1)+l] <- A[i,j]*B[k,l];
}
