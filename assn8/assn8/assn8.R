# ex1
y = c(16,13,18,11,19,14,18,15,25,15,15,11,22,16,14,17,21,15,14,17,15,13,10,13,16,19,18,14,22,16,15,16,21,20,15,13,18,14,11,11)
media = factor(c(rep(1:4,9),1,2,4,2))

df = data.frame(y, media)
fit = lm(y ~ media, data=df)
anova(fit)

# ex2
y = c(210,230,190,180,190,195,170,200,190,193,295,275,290,275,265)
A = rep(c("S1","S2","S3","S4","S5"), 3)
B = rep(c("B1","B2","B3"), rep(5,3))

df = data.frame(A, B, y)
fit = lm(y ~ A+B, data=df)
anova(fit)


# ex3
y = c(10,7,9,6,8,5,4,7,4,5,5,4,6,3,2,3,4,5,1,2)
A = rep(c("고","저","고","저"), rep(5,4))
B = rep(c("남","여"), rep(10,2))

df = data.frame(A, B, y)
fit = lm(y ~ A*B, data=df)
anova(fit)

# ex4
y = c(12.1,12.8,13.9,13.6,18.4,17.5,14.2,17.2,19.2,21.7,21.4,17.5)
A = rep(c("R1","R2","R3"), 4)
B = rep(c("F1","F2","F3","F4"), rep(3,4))

df = data.frame(A, B, y)
fit = lm(y ~ A+B, data=df)
anova(fit)


# ex5-1
mlb_players_18 = read.csv("mlb_players_18.csv", header=T)
boxplot(AVG ~ position, data=mlb_players_18, main="선수의 수비 위치에 따른 타율")

# ex5-2
par(mfrow=c(3,3))
for (pos in c("C","1B","2B","3B","LF","CF","RF","SS")) {
  data = mlb_players_18[mlb_players_18$position==pos,]
  qqnorm(data$AVG, main=paste("Q-Q plot for", pos))
  qqline(data$AVG, col=2, lwd=2)
}
dev.off()

# ex5-3
fit = lm(AVG ~ position, data=mlb_players_18)
anova(fit)


# ex6-1
Boston = MASS::Boston
Boston$age_group = ""
Boston[Boston$age<=50,]$age_group = "low"
Boston[Boston$age>50 & Boston$age<=90,]$age_group = "medium"
Boston[Boston$age>90,]$age_group = "high"
Boston$age_group

# ex6-2
interaction.plot(Boston$age_group, Boston$chas, Boston$medv,
                 main='Interaction Plot', xlab='age_group', ylab='mean of medv', trace.label='chas')

# ex6-3
fit = lm(medv ~ age_group*chas, data=Boston)
anova(fit)
