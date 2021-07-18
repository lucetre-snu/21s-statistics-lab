# ex1
facebook <- rbind(c(288, 278), c(10, 7), c(61, 62))
chisq.test(facebook)

# ex2
blood.thinner <- rbind(c(11, 39), c(14, 26))
chisq.test(blood.thinner)

# ex3
data(iris)
setosa = iris[iris$Species == 'setosa',]
versicolor = iris[iris$Species == 'versicolor',]

setosa.L = setosa[setosa$Sepal.Width > 3.3,]
setosa.M = setosa[setosa$Sepal.Width >= 2.8 & setosa$Sepal.Width < 3.3,]
setosa.S = setosa[setosa$Sepal.Width < 2.8,]

versicolor.L = versicolor[versicolor$Sepal.Width > 3.3,]
versicolor.M = versicolor[versicolor$Sepal.Width >= 2.8 & versicolor$Sepal.Width < 3.3,]
versicolor.S = versicolor[versicolor$Sepal.Width < 2.8,]

iris.Sepal.Width = rbind(c(nrow(setosa.L), nrow(versicolor.L)),
                         c(nrow(setosa.M), nrow(versicolor.M)),
                         c(nrow(setosa.S), nrow(versicolor.S)))

chisq.test(iris.Sepal.Width)

# ex4
install.packages("vcd")
library("vcd")
data(Arthritis)
Arthritis.table = table(Arthritis$Treatment, Arthritis$Improved); Arthritis.table
chisq.test(Arthritis.table)
