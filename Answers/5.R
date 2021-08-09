ames <- read.csv("ames.csv")

# 1
mean(ames$Gr.Liv.Area)
var(ames$Gr.Liv.Area)

# 2
sample60 <- sample(ames$Gr.Liv.Area, 60)
mean(sample60)

# 3
L <- mean(sample60) + qnorm(0.025)*sd(ames$Gr.Liv.Area)/sqrt(60)
U <- mean(sample60) - qnorm(0.025)*sd(ames$Gr.Liv.Area)/sqrt(60)
print(c(L, U))

# 4
lower <- c(); upper <- c()
pop.mean <- mean(ames$Gr.Liv.Area)
for(i in 1:50) {
  sample60 <- sample(ames$Gr.Liv.Area, 60)
  lower[i] <- mean(sample60) + qnorm(0.025)*sd(ames$Gr.Liv.Area)/sqrt(60)
  upper[i] <- mean(sample60) - qnorm(0.025)*sd(ames$Gr.Liv.Area)/sqrt(60)
}
plot_ci <- function(lo, hi, m) {
  par(mar=c(2,1,1,1), mgp=c(2.7, 0.7, 0))
  k <- length(lo)
  ci.max <- max(rowSums(matrix(c(-1*lo, hi), ncol=2)))
  xR <- m + ci.max * c(-1,1)
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  abline(v=m, lty=2, col='#00000088')
  axis(1, at=m, paste("mu = ", round(m, 4)), cex.axis=1.15)
  
  for(i in 1:k) {
    x <- mean(c(hi[i], lo[i]))
    ci <- c(lo[i], hi[i])
    if(lo[i] > m | m > hi[i]) {
      col <- '#F05133'
      points(x, i, cex=1.4, col=col)
      lines(ci, rep(i, 2), col=col, lwd=5)
    }
    col <- 1
    points(x, i, pch=20, cex=1.2, col=col)
    lines(ci, rep(i,2), col=col)
  }
}

plot_ci(lower, upper, pop.mean)

{
  set.seed(202011)
  
  # 5
  x1 <- rnorm(100, 3, 9)
  x2 <- rnorm(1000, 3, 9)
  x3 <- rnorm(10000, 3, 9)
  mean(x1); var(x1)
  mean(x2); var(x2)
  mean(x3); var(x3)
  
  # 6
  lower <- c(); upper <- c()
  count <- 0
  for(i in 1:100) {
    x4 <- rnorm(10, 3, 9)
    lower[i] <- mean(x4) + qnorm(0.025)*9/sqrt(10)
    upper[i] <- mean(x4) - qnorm(0.025)*9/sqrt(10)
    if(lower[i] < 3 & upper[i] > 3) {
      count <- count + 1
    }
  }
}

# 7
X <- c(78.70, 83.43, 76.19, 77.70, 78.54, 
       81.66, 78.42, 81.54, 79.37, 78.24,
       76.28, 79.41, 79.95, 80.44, 77.94,
       78.18, 78.54, 78.39, 79.27, 80.07, 81.07)
Z <- (mean(X) - 79) / (2 / sqrt(21))
pnorm(Z, lower.tail=F)
