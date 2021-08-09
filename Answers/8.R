# 1
handspan <- read.table("handspan.txt", header=T)
cor(handspan$Height, handspan$HandSpan) #1-1
plot(handspan$Height, handspan$HandSpan) #1-1
cor.test(handspan$Height, handspan$HandSpan) #1-2
lm(handspan$Height~handspan$HandSpan) #1-3
anova(lm(handspan$Height~handspan$HandSpan)) #1-3
plot(lm(handspan$Height~handspan$HandSpan)) #1-4
plot(residuals(lm(handspan$Height~handspan$HandSpan)))

# 2
carstopping <- read.table("carstopping.txt", header=T)
cor.test(carstopping$StopDist, carstopping$Speed) #2-1
fit <- lm(carstopping$StopDist ~ carstopping$Speed)
summary(fit) #2-2
plot(fit) #2-3
# plot(carstopping$Speed, carstopping$StopDist) #2-4
# plot(carstopping$Speed, sqrt(carstopping$StopDist)) #2-4
# fit2 <- lm(sqrt(StopDist) ~ Speed, data=carstopping)
# summary(fit2) # 2-5
# plot(fit2) # 2-6

# 3
hospital <- read.table("hospital.txt", header=T)
cor.test(hospital$InfctRsk, hospital$Stay)
cor.test(hospital$InfctRsk, hospital$Age)
cor.test(hospital$InfctRsk, hospital$Xray)
fit3 <- lm(InfctRsk ~ Stay + Age + Xray, data=hospital)
summary(fit3) #3-2
plot(fit3) #3-3

# 4
fit4 <- lm(InfctRsk ~ Stay + Xray, data=hospital)
summary(fit4) #3-a
plot(fit4) #3-b

#5
x <- c(0,1,3,3,5,7,8,8,10,11,13)
y <- c(1,3,8,11,12,18,16,20,16,24,20)
cor(x, y); plot(x,y) #5-a
cor.test(x,y) #5-b