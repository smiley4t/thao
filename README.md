set.seed(123)
library("plyr")
N<-1000
z<-1.644854
##Tn(x-u), n=20, Bin(10, 0.1)
n<-20
tstat_MC<-replicate(n=1000,expr={x=rbinom(20,10,0.1);c((mean(x)-1)/(sqrt(var(x))/sqrt(n)))})
count(tstat_MC<=z)##965 => p=0.965

##Tn(x-u), n = 40, Bin (10, 0.1)
n<-40
tstat_MC<-replicate(n=1000,expr={x=rbinom(40,10,0.1);c((mean(x)-1))/(sqrt(var(x))/sqrt(n))})
count(Tn_MC<=z)##956 => p=0.956

##Tn(x-u), n = 20, Poi(1)
n<-20
tstat_MC<-replicate(n=1000,expr={x=rpois(20,1);c((mean(x)-1))/(sqrt(var(x))/sqrt(n))})
count(Tn_MC<=z) ##960 (constant) => p = 0.96

##Tn(X-u), n = 40. Poi (1)
n<-40
tstat_MC<-replicate(n=1000,expr={x=rpois(40,1);c((mean(x)-1))/(sqrt(var(x))/sqrt(n))})
hist(tstat_MC)
count(Tn_MC<=z) ##960 (constant) => p = 0.96

##Tn(X-u) , n =20 , Exp(1)
n<-20
tstat_MC<-replicate(n=1000,expr={x=rexp(20);c((mean(x)-1))/(sqrt(var(x))/sqrt(n))})
hist(tstat_MC)
count(Tn_MC<=z) ##960 (constant) => p = 0.96

##Tn(X-u) , n =40 , Exp(1)
n<-40
tstat_MC<-replicate(n=1000,expr={x=rexp(40);c((mean(x)-1))/(sqrt(var(x))/sqrt(n))})
hist(tstat_MC)
count(Tn_MC<=z) ##960 (constant) => p = 0.96

##Tn(X-u), n = 20, Normal (2, 9)
n<-20
tstat_MC<-replicate(n=1000,expr={x=rnorm(20,2,3);c((mean(x)-2))/(sqrt(var(x))/sqrt(n))})
hist( tstat_MC)
count(Tn_MC<=z) ##960 (constant) => p = 0.96

##Tn(X-u) , n = 40 , Normal (0,1)
n<-40
tstat_MC<-replicate(n=1000,expr={x=rnorm(40);c((mean(x)))/(sqrt(var(x))/sqrt(n))})
hist(tstat_MC)
count(Tn_MC<=z) ##960 (constant)  => p =0.96

##Gn(1), n = 20, Bin (10,0.1)
n<-20
a=rep(0,n)
b=rep(0,n)
vec.prob<-c(rep(1/n,n))
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rbinom(20,10,0.1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
 c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
count(tstat_random_MC<=z)##955 => p=0.955

##Gn(1), n = 40, Bin (10, 0.1)
n<-40
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)

tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rbinom(40,10,0.1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
count(tstat_random_MC<=z)##951 => p = 0.951

##Gn(1), n =20, Poi(1)
n<-20
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rpois(20,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z)#966 => p =0.966

##Gn(1), n =40, Poi(1)
n<-40
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rpois(40,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
count(tstat_random_MC<=z)#959 => p =0.959

##Gn(1), n =20, Exp(1)
n<-20
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rexp(20,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
count(tstat_random_MC<=z)#965 => p = 0.965

##Gn(1), n =40, Exp(1)
n<-40
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rexp(40,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z) ##956 => p = 0.956

##Gn(1), n =20, Normal (2,9)
n<-20
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rnorm(20,2,3);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z) ##948 => p =0.948

##Gn(1), n = 40, Normal (2,9)
n<-40
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rnorm(40,2,3);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z) ##954 => p =0.954

##Gn(1), n =20, Normal (0,1)
n<-20
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rnorm(20,0,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z)##957 => p =0.957

##Gn(1), n = 40, Normal (0,1)
n<-40
vec.prob<-c(rep(1/n,n))
a=rep(0,n)
b=rep(0,n)
tstat_random_MC<-replicate(n=1000,expr={w=rmultinom(1,n,vec.prob);X=rnorm(40,0,1);for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))})
hist(tstat_random_MC)
count(tstat_random_MC<=z)#950 => p =0.95

##mean G*n , n=20, Bin (10,0.1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rbinom(20,10,0.1)
  for (i in 1:100)
{w[[i]]<-rmultinom(1,n,vec.prob);
 boot100<-c(boot100,boot.compute(X,w[[i]]))}
 return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##995 => p = 0.995

##mean G*n, n=40, Bin(10,0.1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rbinom(40,10,0.1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##991 => p =0.991

##meanG*n, n = 20, Poi(1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rpois(20,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##989 => p =0.989

##meanG*n, n =40, Poi(1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rpois(40,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##992 => p= 0.992

##mean G*n, n=20, Exp(1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rexp(20,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##997 => p =0.997

##meanG*n, n=40, Exp(1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rexp(40,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)#997 => p=0.997

##meanG*n, n=20, Normal(2,9)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rnorm(20,2,3)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)#977 => p=0.977

##meanG*n, n=40, Normal(2,9)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rnorm(40,2,3)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##988 => p=0.988

##meanG*n, n=20, Normal (0,1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rnorm(20,0,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##981 => p =0.981

##meanG*n, n=40, Normal (0,1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.mean.100<-function(w){
  X<-rnorm(40,0,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(mean(boot100))}
boot_mean_MC<-replicate(n=1000,boot.mean.100(w))
count(boot_mean_MC<=z)##983 => p =0.983

##medianG*n, n=20, Bin(10,0.1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rbinom(20,10,0.1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z) #986=> p =0.986

##medianG*n, n=40, Bin(10,0.1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rbinom(40,10,0.1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z) #988=> p =0.988

##medianG*n, n=20, Poi(1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rpois(20,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##993 => p =0.993

##medianG*n, n=40, Poi(1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rpois(40,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z) ##986 => p =0.986

##medianG*n, n=20, Exp(1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rexp(20,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##993=> p =0.993

##medianG*n, n=40, Exp(1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-1)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rexp(40,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##992 => p =0.992

##medianG*n, n=20, Normal(2,9)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rnorm(20,2,3)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##973 => p =0.973

##medianG*n, n=40, Normal(2,9)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i]-2)
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rnorm(40,2,3)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##982 => p =0.982

##medianG*n, n=20, Normal(0,1)
n<-20
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rnorm(20,0,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=1000,boot.median.100(w))
count(boot_median_MC<=z)##980 => p =0.98

##medianG*n, n=40, Normal(0,1)
n<-40
vec.prob<-c(rep(1/n,n))
boot.compute<-function(X,w){for ( i in 1:n){
  a[i]<- abs(w[i]-1)*(X[i])
  b[i]<-(w[i]-1)^2};
  c(sum(a)/(sqrt(var(X))*sqrt(sum(b))))}
w<-list()
boot100<-c()
boot.median.100<-function(w){
  X<-rnorm(40,0,1)
  for (i in 1:100)
  {w[[i]]<-rmultinom(1,n,vec.prob);
  boot100<-c(boot100,boot.compute(X,w[[i]]))}
  return(median(boot100))}
boot_median_MC<-replicate(n=10000,boot.median.100(w))
count(boot_median_MC<=z) ##987 => p =0.987

