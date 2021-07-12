
# ex1
ames = read.csv("ames.csv", header=T)
pop = ames$Gr.Liv.Area

pop.mean = mean(pop); pop.mean
pop.sigma = sd(pop); pop.sigma^2

# ex2
n = 60
pop.sample = sample(pop, n, replace=F)
mean(pop.sample)

# ex3
alpha = 0.05
hi = mean(pop.sample) + qnorm(1-alpha/2)*(pop.sigma/sqrt(n))
lo = mean(pop.sample) - qnorm(1-alpha/2)*(pop.sigma/sqrt(n))
(lo < pop.mean) && (pop.mean < hi)

# ex4
upper = lower = c()
for (i in 1:50) {
  pop.sample = sample(pop, n, replace=F)
  upper[i] = mean(pop.sample) + qnorm(1-alpha/2)*(pop.sigma/sqrt(n))
  lower[i] = mean(pop.sample) - qnorm(1-alpha/2)*(pop.sigma/sqrt(n))
  (lower[i] < pop.mean) && (pop.mean < upper[i])
}
### GIVEN ###
plot_ci <- function(lo, hi, m) {
  par(mar=c(2, 1, 1, 1), mgp=c(2.7, 0.7, 0))
  k <- length(lo)
  ci.max <- max(rowSums(matrix(c(-1 * lo, hi), ncol=2)))
  xR <- m + ci.max * c(-1, 1)
  yR <- c(0, 41 * k / 40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  abline(v=m, lty=2, col='#00000088')
  axis(1, at=m, paste("mu = ", round(m, 4)), cex.axis=1.15)
  for(i in 1:k) {
    x <- mean(c(hi[i], lo[i]))
    ci <- c(lo[i], hi[i])
    if (lo[i]>m | m>hi[i]) {
      col <- "#F05133"
      points(x, i, cex=1.4, col=col)
      lines(ci, rep(i, 2), col=col, lwd=5)
    }
    col <- 1
    points(x, i, pch=20, cex=1.2, col=col)
    lines(ci, rep(i, 2), col=col)
  }
}
plot_ci(lower, upper, pop.mean)

# ex5
set.seed(202011)
mu = 3; sigma = 9
x1 = rnorm(100, mu, sigma)
x2 = rnorm(1000, mu, sigma)
x3 = rnorm(10000, mu, sigma)
mean(x1); sd(x1)^2
mean(x2); sd(x2)^2
mean(x3); sd(x3)^2

# ex6
set.seed(202011)
mu = 3; sigma = 9; n = 10; alpha = 0.05
count = 0
for (i in 1:100) {
  sample = rnorm(n, mu, sigma)
  hi = mean(sample) + qnorm(1-alpha/2)*(sigma/sqrt(n))
  lo = mean(sample) - qnorm(1-alpha/2)*(sigma/sqrt(n))
  if ((lo < mu) && (mu < hi)) count = count + 1
}
count

# ex7
X.sample = c(78.70, 83.43, 76.19, 77.70, 78.54, 81.66, 78.42, 
             81.54, 79.37, 78.24, 76.28, 79.41, 79.95, 80.44, 
             77.94, 78.18, 78.54, 78.39, 79.27, 80.07, 81.07)
n = length(X.sample)
X.mu = 79; X.sigma = 2
z = (mean(X.sample) - X.mu) / (X.sigma/sqrt(n)); z
pvalue = pnorm(z, lower.tail=F); pvalue
