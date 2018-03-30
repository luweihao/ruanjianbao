#Ex1: Screening method
mfscr.gamma=function(n, alpha){
    u1=runif(n)
    u2=runif(n)
    k=1/alpha+1/exp(1)
    y=ifelse(u2<=1/(alpha*k),
             (alpha*k*u2)^(1/alpha),
             -log(k*(1-u2)))
    return(y[(y<=1&u1<=exp(-y))|
             (y>1&u1<y^(alpha-1))])
}

alpha=0.6
x=mfscr.gamma(5000, alpha)
hist(x, probability = T,  breaks = 24, main="")
lines(density(x), col="blue", xlim=c(0,6))
sequ=seq(0,6,by=0.01)
lines(sequ, dgamma(sequ, alpha), col="red")
legend(6, 0.8, pch=c(17,8), col=c("blue","red"), cex=0.5,
       legend = c("Sample density function", "Gamma distribution"))
