# ex1-1
handspan = read.table("handspan.txt", header=T)
plot(handspan$Height, handspan$HandSpan)
cor(handspan$Height, handspan$HandSpan)

# ex1-2
cor.test(handspan$Height, handspan$HandSpan)

# ex1-3
handspan.res <- lm(handspan$Height ~ handspan$HandSpan); handspan.res
summary(handspan.res)
anova(handspan.res)

# ex1-4
par(mfrow=c(1,2))
plot(handspan.res)
dev.off()


# ex2-1
carstopping = read.table("carstopping.txt", header=T)
plot(carstopping$Speed, carstopping$StopDist)
cor.test(carstopping$Speed, carstopping$StopDist)

# ex2-2
carstopping.res <- lm(carstopping$StopDist ~ carstopping$Speed); carstopping.res
summary(carstopping.res)
anova(carstopping.res)

# ex2-3
par(mfrow=c(1,2))
plot(carstopping.res)
dev.off()


# ex3-1
hospital = read.table("hospital.txt", header=T)

par(mfrow=c(1,3))
plot(InfctRsk ~ Stay + Age + Xray, data=hospital)
cor.test(hospital$InfctRsk, hospital$Stay)
cor.test(hospital$InfctRsk, hospital$Age)
cor.test(hospital$InfctRsk, hospital$Xray)
dev.off()

# ex3-2
hospital.res <- lm(InfctRsk ~ Stay + Age + Xray, data=hospital); hospital.res
summary(hospital.res)
anova(hospital.res)

# ex3-3
par(mfrow=c(1,2))
plot(hospital.res)
dev.off()


# ex4-a
hospital.res <- lm(InfctRsk ~ Stay + Xray, data=hospital); hospital.res
summary(hospital.res)
anova(hospital.res)

# ex4-b
par(mfrow=c(1,2))
plot(hospital.res)
dev.off()


# ex5-a
X = c(0, 1, 3, 3, 5, 7, 8, 8, 10, 11, 13)
Y = c(1, 3, 8, 11, 12, 18, 16, 20, 16, 24, 20) 
df <- data.frame(X, Y)
cor(df$Y, df$X)
plot(Y ~ X, data=df)

# ex5-b
cor.test(df$Y, df$X)
