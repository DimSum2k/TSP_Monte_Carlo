### Deuxieme Td Monte Carlo - 23/04

### Attention reflechir au sens des sd !!!!!!!

simul_normal2 <- function(n) {
  return(matrix(rnorm(2*n),nrow=n,ncol=2))
}

estimation_MC <- function(n,c, plot=F) {
  x = simul_normal2(n)
  y = abs(x[,1]*x[,2])<c
  if (plot==T) {
  plot( x[ y , 1 ] , x[ y , 2 ] , col = 'blue' , pch = '.')
  points( x[!y , 1 ] , x[!y , 2 ] , col = 'red' , pch = ',')}
  return(list(mean=mean(y),sd=sd(y),IC=c(mean(y) - 1.96 *sd(y)/sqrt(n),mean(y) + 1.96 * sd(y)/sqrt(n))))
}
n = 10^4
estimation_MC(n,0.5) #attention la standard deviation n'est pas la variance de notre estimateur

estimation_cv <- function(n,c) {
  x = simul_normal2(n)
  z = abs(x[,1]*x[,2]) - 2/pi
  y = abs(x[,1]*x[,2])<c
  beta = -cov(y,z)/var(z)
  w = y + beta*z
  return(list(mean=mean(w),sd=sd(y),IC=c(mean(y) - 1.96 *sd(y)/sqrt(n),mean(y) + 1.96 * sd(y)/sqrt(n))))
}
n = 10^4
estimation_cv(n,0.5)

estimate_sd <- function(N,n,c) {
  MC = rep(NA,N)
  CV = rep(NA,N)
  for (i in 1:N) {MC[i] = estimation_MC(n,c)$mean }
  for (i in 1:N) {CV[i] = estimation_cv(n,c)$mean }
  
  return(list(MC = sd(MC), CV = sd(CV)))
  
}
  
estimate_sd(1000,1000,0.5)
#Comparer avec resultats correction 


