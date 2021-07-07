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
