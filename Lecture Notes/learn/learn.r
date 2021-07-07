1+2
2*3
c(1,2,3)
1:5
4:1
rep(1,2)
rep(c(1,2,3), each=2)
m = rbind(c(1,2,3), c(2,3,4))
m
m[,1:2]
m[1:2]

par(mfrow=1:2)
x = seq(-3, 3, 0.1)
fx = dnorm(x, 0, 1)
plot(x, fx, type="l", main="standard normal dist")
plot(x, fx, type="b", col="6", main="standard normal dist")
dev.off()

par(mfrow=1:2)
px = pnorm(x, 0, 1)
plot(x, fx)
plot(x, px)
dev.off()

pnorm(1800, mean=2000, sd=200, lower.tail=F)
qnorm(0.8413447, mean=2000, sd=200, lower.tail=F)


n = 6
x = 0:n
par(mfrow=1:2)
fx = dbinom(x, n, 0.1)
px = pbinom(x, n, 0.1)
plot(x, fx, type="h")
plot(x, px, type="h")
dev.off()

p = 0.1
n = c(10, 30, 50)
par(mfrow=c(1,3))
for (i in 1:3) {
  x = seq(from=0, to=n[i], by=1)
  fx = dbinom(x, n[i], p)
  plot(x, fx, pch=16, xlim=c(-3,15), ylab="prob", xlab="", main=paste("dbinorm (p=", p, ", n=", n[i], ")"))
  lines(x, fx)
  
  y = seq(-5, 15, 0.1)
  mu = n[i]*p
  sd = sqrt(n[i]*p*(1-p))
  fy = dnorm(y, mu, sd)
  lines(y, fy, col="red")
}
dev.off()

pbinom(5, 20, prob=0.1)


n = c(10, 30, 50, 100)
par(mfrow=c(1,4))
for (i in 1:4) {
  x.mean = c()
  for (j in 1:1000) {
    x = runif(n[i], 0, 1)
    x.mean[j] = mean(x)
  }
  hist(x.mean, xlim=c(0,1), probability=T, main=paste("n=",n[i]))
  
  y = seq(0, 1, 0.01)
  mu = 0.5
  sigma = sqrt(1/12)
  fy = dnorm(y, mu, sigma/sqrt(n[i]))
  lines(y, fy)
}

dev.off()

# chi-square
n = 1
xsquare.sum = c()
for (i in 1:1000) {
  x = rnorm(n, mean=0, sd=1)
  xsquare.sum[i] = sum(x*x)
}
hist(xsquare.sum, probability=T, main=paste("n=", n))
y = seq(0, 15, 0.1)
fy = dchisq(y, n)
lines(y, fy)


n = c(10, 30, 50, 100)
par(mfrow=c(1,4))

for (i in 1:4) {
  xsquare.sum = c()
  for (j in 1:1000) {
    x = rnorm(n[i], mean=0, sd=1)
    xsquare.sum[j] = sum(x*x)
  }
  hist(xsquare.sum, probability=T, main=paste("n=", n[i]))
  y = seq(0, max(xsquare.sum), 0.1)
  fy = dchisq(y, n[i])
  lines(y, fy)
}
