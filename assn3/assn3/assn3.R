
# ex1
ames = read.csv("ames.csv", header=T)
saleprice = ames$SalePrice
hist(saleprice, xlab="Sale Price ($)", main="Histogram of Sale Price")
summary(saleprice)

# ex2
saleprice.sample = sample(saleprice, 50, replace=F)
mean(saleprice); mean(saleprice.sample)

# ex3
sample_mean50 = c()
for (i in 1:5000) {
  saleprice.sample = sample(saleprice, 50, replace=F)
  sample_mean50[i] = mean(saleprice.sample)
}
hist(sample_mean50, xlim=c(140000, 230000),
     xlab="sample price ($)", main="Average sale price for sample size 50")

# ex4
c(mean(sample_mean50), var(sample_mean50))
c(mean(saleprice), var(saleprice)/50)

# ex5
sample_mean150 = c()
for (i in 1:5000) {
  saleprice.sample = sample(saleprice, 150, replace=F)
  sample_mean150[i] = mean(saleprice.sample)
}
hist(sample_mean150, xlim=c(140000, 230000),
     xlab="sample price ($)", main="Average sale price for sample size 150")

# ex6
n = c(10, 30, 50, 100)
par(mfrow=c(2,2))

for (i in 1:4) {
  xsquare.sum = c()
  for (j in 1:1000) {
    x = rnorm(n[i], mean=0, sd=1)
    xsquare.sum[j] = sum(x*x)
  }
  hist(xsquare.sum, probability=T, main=paste("chi-square ( n =", n[i], ")"))
  y = seq(0, max(xsquare.sum), 0.1)
  fy = dchisq(y, n[i])
  lines(y, fy)
}
dev.off()

# ex7
mean = 500
sd = 100
pnorm(800, mean, sd)
pnorm(300, mean, sd, lower.tail=F)

# ex8
par(mfrow=c(1,2))
data = rnorm(1000, mean=100, sd=36)
hist(data)
qqnorm(data)
qqline(data, col=2, lwd=3)
dev.off()
