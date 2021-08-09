# 1
information <- c(16, 19, 25, 22, 21, 15, 16, 22, 21, 18,
                 13, 14, 15, 16, 15, 13, 19, 16, 20, 14, 11,
                 18, 18, 15, 14, 14, 10, 18, 15, 15,
                 11, 15, 11, 17, 17, 13, 14, 16, 13, 11)
media <- factor(c(rep("TV", 10), rep("newspaper", 11),
                  rep("radio", 9), rep("magazine", 10)))
fit <- lm(information ~ media)
anova(fit)

# 2
market <- factor(c(1:5, 1:5, 1:5))
box <- factor(rep(1:3, each=5))
sell <- c(210, 230, 190, 180, 190,
          195, 170, 200, 190, 193,
          295, 275, 290, 275, 265)
fit2 <- lm(sell ~ market + box)
anova(fit2)

# 3
A <- rep(c("male", "female"), each=10)
B <- rep(rep(c("high", "low"), each=5),2)
value <- c(10,7,9,6,8, 5,4,7,4,5, 5,4,6,3,2, 3,4,5,1,2)
fit3 <- lm(value~A*B)
anova(fit3); summary(fit3)
fit31 <- lm(value ~ A+B)
anova(fit31); summary(fit31)

# 4
region <- factor(rep(1:3, 4))
fertilizer <- factor(rep(1:4, each=3))
harvest <- c(12.1, 12.8, 13.9, 13.6, 18.4, 17.5,
             14.2, 17.2, 19.2, 21.7, 21.4, 17.5)
anova(lm(harvest~region+fertilizer))

# 5
mlb <- read.csv("mlb_players_18.csv")
boxplot(mlb$AVG ~ mlb$position) #5-1
par(mfrow=c(3,3))
positions <- factor(mlb$position)
for(i in 1:length(levels(positions))) {
  pos <- levels(positions)[i]
  which_pos <- which(mlb$position==pos)
  qqnorm(mlb$AVG[which_pos])
  qqline(mlb$AVG[which_pos])
} #5-2
layout(1)
fit5 <- lm(mlb$AVG ~ mlb$position)
anova(fit5) #5-3

# 6
library(MASS)
x <- Boston
x$age_group <- "low"
for(i in 1:nrow(x)) {
  if(x$age[i] > 90) {
    x$age_group[i] <- "high"
  } else if(x$age[i] > 50) {
    x$age_group[i] <- "medium"
  }
} #6-1
interaction.plot(x$chas, x$age_group, x$medv) #6-2
anova(lm(medv~chas * age_group, data=x))
