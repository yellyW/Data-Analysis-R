### Exercise 1

# 1)
telephone=read.table("telephone.txt",header = TRUE)
t=median(telephone)
t
B=1000
tstar=numeric(B)
n=length(telephone)
for (i in 1:B) {
  xstar=rexp(n,1)
  tstar[i]=median(xstar)
}
hist(tstar,prob=T)
hist(tstar,prob=T,ylim=c(0,1),
      main="histogram of tstar & true density curve of T")
densmedianexp=function(x,n) n*exp(-x)*(1-exp(-x))^(n-1)
lines(rep(t,2),seq(0,2*densmedianexp(t,n),length=2),
         type="l", col="red", lwd=3)
axis(1,t,expression(paste("t") ) )
u=seq(0,median(tstar),length=200)
lines(u,densmedianexp(u,n),type="l",col="blue")



### Exercise 2
# 1)
light1=scan("light1879.txt")
light2=scan("light1882.txt")
hist(light1)
hist(light2)
boxplot(light1,light2)










