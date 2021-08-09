# 1
male <- c(288, 10, 61)
female <- c(378, 7, 62)
facebook <- data.frame(male, female)
chisq.test(facebook)

# 2
med_yes <- c(11, 39)
med_no <- c(14, 26)
med <- data.frame(med_yes, med_no)
chisq.test(med)

# 3
data(iris)
iris$size <- c()
for(i in 1:nrow(iris)) {
  if(iris$Sepal.Width[i] >= 3.3) {
    iris$size[i] <- 'L'
  } else if(iris$Sepal.Width[i] >= 2.8) {
    iris$size[i] <- 'M'
  } else {
    iris$size[i] <- 'S'
  }
}
setosa <- iris[which(iris$Species=="setosa"),]
versicolor <- iris[which(iris$Species=="versicolor"),]
setosa_size <- c(table(setosa$size))
versicolor_size <- c(table(versicolor$size))
sizes <- data.frame(setosa_size, versicolor_size)
chisq.test(sizes)

# 4
install.packages("vcd")
library("vcd")
data(Arthritis)
x <- Arthritis[which(Arthritis$Sex=="Male"),]
chisq.test(table(x$Improved, x$Treatment))
