cdc <- read.table("cdc.txt")
head(cdc)
bodydims <- read.csv("bodydims.csv")
head(bodydims)

# 1
table(cdc$genhlth)

# 2
summary(cdc$weight)

# 3
plot(cdc$weight, cdc$wtdesire)

# 4
cdc$wdiff <- cdc$wtdesire - cdc$weight
summary(cdc$wdiff); boxplot(cdc$wdiff)

# 5
hist(cdc$age, breaks=50)
hist(cdc$age, breaks=100)

# 6
plot(cdc$height, cdc$weight, 
     main="correlation of height and weight",
     xlab="height", ylab="weight")

# 7
table(bodydims$sex)

# 8
mean(bodydims$elb.di); sd(bodydims$elb.di); quantile(bodydims$elb.di)
sum(bodydims$elb.di); range(bodydims$elb.di)

# 9
hist(bodydims$hgt[bodydims$sex==0], breaks=20) # female
hist(bodydims$hgt[bodydims$sex==1], breaks=20) # male
