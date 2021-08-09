# 3.6 
# 1
data <- sample(1:100, 40)
mat1 <- matrix(data, nrow=5, ncol=8); mat1
mat2 <- mat1[c(2,3),]; mat2
mat3 <- mat1[,c(1,4,7,8)]; mat3
mean4 <- mean(mat1[,7]); var4 <- var(mat1[,7]); c(mean4, var4)

# 2
data <- sample(1:20, 1)
if(data >= 10) {
  print(c(data, "P"))
} else {
  print(c(data, "NP"))
}

# 3
data <- sample(1:20, 8)
for(i in data) {
  if(i >= 10) {
    print(c(i, "P"))
  } else {
    print(c(i, "NP"))
  }
}

# 4
n <- 1:100; sum(n)
three <- seq(3, 200, by=3); sum(three)
sd(three)

# 5
m <- matrix(c(7,6,9, 3,1,10), nrow=3);m
m2 <- m %% 4
m3 <- m + matrix(c(10,20,30,10,20,30), nrow=3)
