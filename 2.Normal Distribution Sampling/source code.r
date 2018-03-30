##Normal Distribution Sampling
#Parameter setting: 
set.seed(086)
N=1000
n=1000

#Sampling with rnorm() and 'lattice point discretization method':
norm.sample=rnorm(n)
xi=seq(-4, 4, length.out=N)
xi.density=dnorm(xi)/sum(dnorm(xi))
lattice.sample=sample(xi, replace=T, size=n, prob=xi.density)
opar<-par(mfrow=c(1,2))
hist(norm.sample, xlim=c(-4,4), ylim=c(0,0.5), breaks=32, probability=T)
lines(density(norm.sample), col="blue", lwd=2)
hist(lattice.sample, xlim=c(-4,4), ylim=c(0,0.5), breaks=32, probability=T)
lines(density(lattice.sample), col="blue", lwd=2)
par(opar)