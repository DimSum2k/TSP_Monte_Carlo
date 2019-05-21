### Premier Td Monte Carlo - 09/04

#### Example Méthode de rejet - loi exponentielle ###############

#Simuler selon exp(theta)
simul_exp <- function(n,theta) {
  u = runif(n)
  x = -log(u)/theta
  return(x)
}

n = 10^5
theta = 1
x = simul_exp(n,theta)
hist(x,prob=T,col="black",border="white",
     breaks=seq(min(x),max(x)+sd(x)/5,by=sd(x)/5), #?
     xlab="Valeurs de l'exponentielle",
     main="loi exp generee par methode de la fonction inverse")
lines(seq(0,max(x),0.01),theta*exp(-theta*seq(0,max(x),0.01)),col="red",lwd=4)

#### Application - loi geometrique ###############
#Simuler selon geom(p)
simulgeom <-function(n,p){
  theta = -log(1-p)
  x <- simul_exp(n,theta)
  y <- floor(x)+1
  return(y)
}
x <-simulgeom(n,0.5)
hist(x[x<10], breaks = 1:10,prob=T)
# ameliorer l'histogramme et afficher les vraies valeurs de la loi géométrique

#### Exercice 1 - RANDU #############################
#Non traiteé

#### Exercice 2 - Rejet de loi de Laplace #############################
#### Question 1 ################################################
#Simuler selon laplace
simul_laplace <- function(n) {
  u = runif(n)
  x = log(2*u)*(u<0.5)-log(2*(1-u))*(u>=0.5)
  return(x)
}
x=simul_laplace(n)
hist(x,prob=T,col="black",border="white",
     breaks=seq(min(x),max(x)+sd(x)/5,by=sd(x)/5), #?
     xlab="Valeurs de la loi de Laplace",
     main="Loi de Laplac generee par la methode de la fonction inverse")
lines(seq(min(x),max(x),0.01),0.5*exp(-abs(seq(min(x),max(x),0.01))),col="red",lwd=4)

#### Question 2 ################################################

#Simuler selon loi Normale (Acceptation rejet)
simul_norm=function(n){
  count_trial = 0
  count_trial_passed = 0
  x=rep(NA,n)
  for(i in 1:n){
    u=1;y=0
    while(u>exp(-0.5*(abs(y)-1)^2)){
      count_trial = count_trial + 1
      y=simul_laplace(1);u=runif(1)
    }
    count_trial_passed = count_trial_passed + 1
    x[i]=y
  }
  print(c("Proba d'acceptation", count_trial_passed/count_trial))
  return(x)
}
x=simul_norm(n)
hist(x,prob=T,col="black",border="white",
     breaks=seq(min(x),max(x)+sd(x)/5,by=sd(x)/5), #?
     xlab="Valeurs de la loi normale (0,1)",
     main="Loi normale (0,1) generee par la methode d'acceptation/rejet")
lines(seq(min(x),max(x),0.01),1/sqrt(2*pi)*exp(-0.5*seq(min(x),max(x),0.01)^2),col="red",lwd=4)
#A peut obtenir un résultat vectorisé en se servant de la proba d'acceptation
#mais le longueur de l'output sera aléatoire









