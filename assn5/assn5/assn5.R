# ex1
textbooks = read.table("textbooks.txt", header=T)
# 대응 비교
t.test(textbooks$amazNew, textbooks$uclaNew, paired=T)

# ex2
run10samp = read.table("run10samp.txt", header=T)
tapply(run10samp$time, run10samp$gender, mean)

# 등분산 여부 검정: 기각X (등분산)
var.test(time ~ gender, data=run10samp)
# 등분산 가정에서의 독립 이표본 평균 검정: 기각
t.test(time ~ gender, data=run10samp, var.equal=T, alternative="greater")

# ex3
left  = c(1.8, 0.2, 1.8, 1.8, 0.9, 1.0, 0.4, 0.2, 0.5, 2.9)
right = c(2.4, 0.5, 2.1, 2.2, 1.4, 1.5, 1.0, 0.5, 1.1, 3.2)

# 대응 비교: 기각
t.test(left, right, paired=T)
# 등분산 가정에서의 독립 이표본 평균 검정: 기각X
t.test(left, right, var.equal=T)

# ex4
data(iris)
iris = iris[(iris$Species=="setosa" | iris$Species=="virginica"),]
tapply(iris$Sepal.Length, iris$Species, mean)

# 등분산 여부 검정: 기각 (이분산)
var.test(Sepal.Length ~ Species, data=iris)
# 이분산 가정에서의 독립 이표본 평균 검정: 기각
t.test(Sepal.Length ~ Species, data=iris, var.equal=F, alternative="less")
