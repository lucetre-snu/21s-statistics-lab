cdc <- read.table("cdc.txt", header=T)

# ex1
table(cdc$genhlth)

# ex2
summary(cdc$weight)

# ex3
plot(cdc$weight, cdc$wtdesire, 
     main="Weight & Desired weight (lb)")
cor(cdc$weight, cdc$wtdesire)

# ex4
wdiff <- cdc$wtdesire - cdc$weight
summary(wdiff)
plot(wdiff, main="Desired weight change (lb)")
# boxplot(wdiff, horizontal=T)

# ex5
hist(cdc$age, breaks=50,  main="Age Hist w. 50 breaks")
hist(cdc$age, breaks=100, main="Age Hist w. 100 breaks")

# ex6
plot(cdc$height, cdc$width, 
     main="correlation of height and width",
     xlab="height", ylab="weight")
cor(cdc$weight, cdc$wtdesire)

# ex7
bodydims <- read.csv("bodydims.csv", header=T)
table(bodydims$sex)

# ex8
summary(bodydims$elb.di)

# ex9
male   = bodydims[bodydims$sex == 1, ]
female = bodydims[bodydims$sex == 0, ]

ylim = c(0,100)
xlim = c(140,200)

hist(male$hgt, ylim=ylim, xlim=xlim,
     main="height of males")
hist(female$hgt, ylim=ylim, xlim=xlim, 
     main="height of females")
