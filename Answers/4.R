ames <- read.csv("ames.csv")
set.seed(1)

# 1
hist(ames$SalePrice)

# 2
mean(sample(ames$SalePrice, 50))

# 3 
sample_mean50 <- c()
for(i in 1:5000) {
  sample_mean50[i] <- mean(sample(ames$SalePrice, 50))
}
hist(sample_mean50)

# 4
mean(sample_mean50); mean(ames$SalePrice)
var(sample_mean50); var(ames$SalePrice)
var(sample_mean50)/var(ames$SalePrice)

# 5
sample_mean150 <- c()
for(i in 1:5000) {
  sample_mean150[i] <- mean(sample(ames$SalePrice, 150))
}
hist(sample_mean150)
mean(sample_mean150); mean(ames$SalePrice)
var(sample_mean150); var(ames$SalePrice)

# 6
# Density plot version
ns <- c(1,5,10,30)
y <- seq(0, 40, 0.1)
fy <- dchisq(y, 1)
plot(y, fy, type='l', ylim=c(0, 0.2))
for(i in 1:4) {
  fy <- dchisq(y, ns[i])
  lines(y, fy, col=i)
}
# Histogram version
ns <- c(1,5,10,30)
hist_list <- list()
for(j in 1:4) {
  n = ns[j]
  xsquare.sum = c()
  for(i in 1:1000) {
    x <- rnorm(n, 0, 1)
    xsquare.sum[i] <- sum(x*x)
  }
  hist_list[[j]] <- hist(xsquare.sum, probability=T)
}
range(c(hist_list[[1]]$breaks, hist_list[[2]]$breaks, 
        hist_list[[3]]$breaks, hist_list[[4]]$breaks)) # Get range for x-axis
max(c(hist_list[[1]]$count, hist_list[[2]]$count, 
      hist_list[[3]]$count, hist_list[[4]]$count)) # Get range for y-axis
plot(hist_list[[1]], xlim=c(0, 65), ylim=c(0, 700))
plot(hist_list[[2]], add=T, col=2)
plot(hist_list[[3]], add=T, col=3)
plot(hist_list[[4]], add=T, col=4)

# 7
pnorm(800, 500, 100)
pnorm(300, 500, 100, lower.tail=F)

# 8
data <- rnorm(1000, 100, sqrt(36))
qqnorm(data)
qqline(data)
