# 1
textbooks <- read.table("textbooks.txt")
t.test(textbooks$amazNew, textbooks$uclaNew, paired=T)

# 2
run10samp <- read.table("run10samp.txt")
var.test(run10samp$time ~ run10samp$gender)
t.test(run10samp$time ~ run10samp$gender, var.equal=T)

# 3
left <- c(1.8, 0.2, 1.8, 1.8, 0.9, 1.0, 0.4, 0.2, 0.5, 2.9)
right <- c(2.4, 0.5, 2.1, 2.2, 1.4, 1.5, 1.0, 0.5, 1.1, 3.2)
A <- t.test(left, right, paired=T); A
var.test(left, right)
B <- t.test(left, right, var.equal = T); B # Select

# 4
data('iris'); head(iris)
setosa <- iris$Sepal.Length[iris$Species=='setosa']
virginica <- iris$Sepal.Length[iris$Species=='virginica']
var.test(setosa, virginica)
t.test(setosa, virginica, alternative = "less")
