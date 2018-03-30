#ex1

myexp=function(lambda, n){
  u=runif(n)
  y=-log(1-u)/lambda
  return(y)
}
ans1=myexp(1,1000)
hist(ans1, probability = TRUE)
lines(density(ans1), col="blue")


#ex2

mymin=function(n,k){
  # from a sample with size n
  # generate k numbers
  u=runif(k)
  y=1-u^(1/n)
  return(y)
}

ans2=mymin(1000,1000)
hist(ans2, probability = TRUE, ylim=c(0,800), breaks = 24)
lines(density(ans2), col="blue")


#ex3
gdata=function(m,cx,x)
  # m: sample size
  # cx:c_0,...,c_{n-1}
  # x: x_0,...,x_n
{
  result=rep(0,m)
  n=length(x)
  u=runif(m)
  x1=x[-1]
  x2=x[-n]
  p=cumsum(cx*(x1-x2))
  p_i=c(0,p[-(n-1)])
  for(j in 1:m){
    ii=max((1:(n-1))[p_i<=u[j]])  ## ii is
    result[j]=x[ii]+(u[j]-p_i[ii])/cx[ii]
  }
  hist(result, prob=T)
  lines(x2+mean(x1-x2)/2, cx, col="blue")
  return(result)
}

x=seq(-1,5)
cx=c(0.1,0.2,0.3,0.1,0.2,0.1)
ran=gdata(1000, cx, x)


#ex4 

mybou=function(n,p){
    u=runif(n)
    x=rep(1,n)
    x[u<1-p]=0
    return(x)
}


#ex extra: binomial distribution(imitate ex4)

nplace=function(u, fx){
  sum(u>fx)+1
}

mybinom=function(n, k, p){
  u=runif(k)
  x=1:n
  Fx=pbinom(x,n,p)
  ans=sapply(u, nplace, Fx)
  return(ans)
}

ans3=mybinom(100, 100, 1/2)
hist(ans3, probability = T, breaks = 24)
lines(density(ans3), col="blue")