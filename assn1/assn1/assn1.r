# ex1
v = sample(1:100, 40, replace=F)
m = matrix(v, nrow=5, ncol=8)
m
m1 = m[2:3,]
m1
m2 = m[,c(1,4,7,8)]
m2
mean(m[,7]); var(m[,7])

# ex2
n = sample(1:20, 1, replace=F)
if (n >= 10) print("P") else print("NP")

# ex3
v = sample(1:20, 8, replace=F)
for (n in v) { if (n >= 10) print("P") else print("NP") }

# ex4
# ex4-1
n = 1:100
sum(n)
# ex4-2
three = seq(from=3, to=200, by=3)
sum(three)
# ex4-3
round(sd(three), 2)

# ex5
m = cbind(c(7, 6, 9), c(3, 1, 10))
# ex5-1
m %% 4
# ex5-2
rbind(m[1,]+10, m[2,]+20, m[3,]+30)
