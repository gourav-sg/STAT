#R CODE FOR CHAPTER 5 - FIRST TERM
#REGRESSION USING DUMMY VARIABLES

# use the following code to change to PDF after writing the code in PANDOC

rm(list=ls(all=TRUE))

setwd("~/Desktop/IPSUR-VIDEOS/Dr. Radice - First Series/5 Regression using dummy variables/")
getwd()
water <- read.csv("./../water.xls")
library("XLConnect")
water.xls <- file.path("./../water.xls")
water.1.xls <- readWorksheetFromFile(water.xls, sheet=1)
write.csv(water.1.xls,"./../water.csv")
water <- read.csv("./../water.csv")
View(water)


water[c(1:5),]
detach(water)
attach(water)
par(mfrow=c(1,1))
plot(calcium,mortality,type="n",main="Mortality vs Calcium")
points(calcium[north==0],mortality[north==0],pch=1)
points(calcium[north==1],mortality[north==1],pch=8, col="red")
legend(110,1900,c("north","south"),marks=c(8,1))

water.lmA <- lm(mortality ~ calcium, data = water)
summary(water.lmA)


# plot showing the fitted line and standardized residuals against fitted values  
par(mfrow = c(1, 2))
plot(calcium,mortality,type="n",main="Mortality vs Calcium: Model A")
points(calcium[north==0],mortality[north==0],pch=1)
points(calcium[north==1],mortality[north==1],pch=8,col="red")
legend(90,1900,c("north","south"),pch=c(8,1))

abline(water.lmA)

sresA<-ls.diag(water.lmA)$std.res
fitsA<-fitted(water.lmA)

plot(fitsA,sresA,type="n",main="Standardized Residuals vs Fitted Values")
points(fitsA[north==0],sresA[north==0],pch=1)
points(fitsA[north==1],sresA[north==1],pch=8,col="red")

abline(0,0)

#MODEL B:
north <- factor(north)
#note that with or without factor the values of summary are same.
water.lmB <- lm(mortality ~ calcium + north, data = water, qr = T)
summary(water.lmB)
par(mfrow = c(1, 2))
plot(calcium, mortality, type = "n", main = "Mortality vs Calcium: Model B")
points(calcium[north == 0], mortality[north == 0], pch = 1)
points(calcium[north == 1], mortality[north == 1], pch = 8, col="red")
legend(90, 1900, c("north", "south"), marks = c(8, 1))
summary(water$calcium)
ld <- seq(0, 145, 0.1)
#checking how the predict function works
predict(water.lmB, head(data.frame(calcium = ld, north = rep(0, length(ld)))))
#now we are drawing the lines
lines(ld, predict(water.lmB, data.frame(calcium = ld, north = rep(0, length(ld))),type = "response"))
lines(ld, predict(water.lmB, data.frame(calcium = ld, north = rep(1, length(ld))),type = "response"))

sresB <- ls.diag(water.lmB)$std.res
fitsB <- fitted(water.lmB)
plot(fitsB, sresB, type = "n", main = "Standardized Residuals vs Fitted Values")
points(fitsB[north == 0], sresB[north == 0], pch = 1)
points(fitsB[north == 1], sresB[north == 1], pch = 8, col="red")



#MODEL C:
water.lmC <- lm(mortality ~ calcium + north + north:calcium, data = water)
summary(water.lmC)

par(mfrow = c(1, 2))
plot(calcium, mortality, type = "n", main = "Mortality vs Calcium: Model B")
points(calcium[north == 0], mortality[north == 0], pch = 1)
points(calcium[north == 1], mortality[north == 1], pch = 8, col="red")
legend(90, 1900, c("north", "south"), marks = c(8, 1))

ld <- seq(0, 145, 0.1)
lines(ld, predict(water.lmC, data.frame(calcium = ld, north = rep(0, length(ld))),type = "response"))
lines(ld, predict(water.lmC, data.frame(calcium = ld, north = rep(1, length(ld))),type = "response"))

sresB <- ls.diag(water.lmC)$std.res
fitsB <- fitted(water.lmC)
plot(fitsB, sresB, type = "n", main = "Standardized Residuals vs Fitted Values")
points(fitsB[north == 0], sresB[north == 0], pch = 1)
points(fitsB[north == 1], sresB[north == 1], pch = 8, col="red")

#UNDERSTANDING COLLINEARITY:
#IT IS MENTIONED THAT IN CASE THERE IS COLLINEARITY THEN THE (X(inv)X)(transpose) DOES NOT EXISTS
#SINCE ITS VALUES BECOMES EQUAL TO ZERO.
#TRYING TO WORK THIS OUT BY CREATING A MATRIX WHOSE COLUMNS ARE IN TOTAL ALWAYS EQUAL TO 6
seq(1:6)
mat1 <- matrix(c(1,1,2,2,3,3,2,1,1,4,6,0,0),13,1)
mat1 <- cbind(mat1, c(2,3,2,1,1,2,3,4,1,1,0,6,0))
mat1 <- cbind(mat1, c(3,2,2,3,2,1,1,1,4,1,0,0,6))
mat1
solve(mat1)
t(mat1)
t(mat1) %*% mat1
#trying to work out what happens when the values in the matrix are not linearly dependant
mat1 <- matrix(c(1,1,2,2,3,3,2,1,1,4),10,1)
mat1 <- cbind(mat1, c(2,3,2,10,1,2,13,4,1,1))
mat1 <- cbind(mat1, c(3,2,28,3,2,17,1,1,4,16))
mat1
t(mat1)
t(mat1) %*% mat1

mat1 <- matrix(rep(6,9),9,1)
mat1 <- cbind(mat1, c(5,1,6,0,3,4,2,0,6))
mat1 <- cbind(mat1, c(1,5,0,6,3,2,4,6,0))
t(mat1) %*% mat1



set.seed(42)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- x1 + 2*x2 + rnorm(100)*0.0001    # so x3 approx a linear comb. of x1+x2
mm12 <- model.matrix(~ x1 + x2)        # normal model, two indep. regressors
mm123 <- model.matrix(~ x1 + x2 + x3)
kappa(mm12)  
kappa(mm123)  
mat12 <- matrix(x1)
x3 <- x1 + 2*x2
mat12 <- cbind(x1, x2, x3)
(t(mat12) %*% mat12)

x1 <- c(1,2,3,4,5,6)
x2 <- x1 * (1+2*x1)
mat1 <- matrix(x1,6,1)
mat1 <- cbind(mat1, x2)
mat1
t(mat1) %*% mat1
# COULD NOT UNDERSTAND COLLINEARITY AFTER REPEATED ATTEMPTS TO SEE HOW X(inv)X GIVES US 0 VALUE
# FOR MATRIX WHOSE VALUES ARE COLLINEAR. BUT THE CONCEPT WAS EXPLAINED WELL IN THE GRAPHS 
# AND SET THEORY IMPLEMENTATION IN https://files.nyu.edu/mrg217/public/multicollinearity_handouts.pdf

#SECTION 5.5
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
summary(iris)

#subset(iris,iris$Species =="setosa")
plot(iris$Sepal.Length, iris$Petal.Length, type = "n", main = "PetalL versus SepalL")
points(iris$Sepal.Length[iris$Species == "setosa"], iris$Petal.Length[iris$Species == "setosa"], pch = 1)
points(iris$Sepal.Length[iris$Species == "virginica"], iris$Petal.Length[iris$Species  == "virginica"], pch = 8, col="red")
points(iris$Sepal.Length[iris$Species == "versicolor"], iris$Petal.Length[iris$Species  == "versicolor"], pch = 3, col="blue")
legend(7, 2.5, c("setosa", "virginica", "versicolor"), marks = c("1", "3", "8"))


#PREPARING THE IRIS DATA SET
rm(list=ls(all=TRUE))
library("XLConnect")
iris.xls <- file.path("./iris.xls")
iris.1.xls <- readWorksheetFromFile(iris.xls, sheet=1)
write.csv(iris.1.xls,"./iris.csv")
iris<-read.csv("iris.csv")

attach(iris)

iris[c(1:5,51:55,101:105),]

#splom(~iris[,2:5])
#plot(sepalL,petalL,main="PetalL versus SepalL")

plot(sepalL,petalL,type="n",main="PetalL versus SepalL")
points(sepalL[species=="setosa"],petalL[species=="setosa"],pch=1)
points(sepalL[species=="virginica"],petalL[species=="virginica"],pch=3,col="red")
points(sepalL[species=="versicolor"],petalL[species=="versicolor"],pch=8,col="blue")
legend(7,2.5,c("setosa","virginica","versicolor"),pch=c(1,3,8))

#Model A
iris.lmA<-lm(petalL~sepalL,data=iris,qr=T)
summary(iris.lmA)

#par(mfrow=c(1,2))
par(mfrow=c(1,2))
plot(sepalL,petalL,type="n",main="PetalL versus SepalL: Model A")
points(sepalL[species=="setosa"],petalL[species=="setosa"],pch=1)
points(sepalL[species=="virginica"],petalL[species=="virginica"],pch=3,col="red")
points(sepalL[species=="versicolor"],petalL[species=="versicolor"],pch=8,col="blue")
legend(6.5,4,c("setosa","virginica","versicolor"),pch=c(1,3,8))
abline(iris.lmA)

sresA<-ls.diag(iris.lmA)$std.res
fitsA<-fitted(iris.lmA)

plot(fitsA,sresA,type="n",main="Standardized Residuals vs Fitted Values")
points(fitsA[species=="setosa"],sresA[species=="setosa"],pch=1)
points(fitsA[species=="virginica"],sresA[species=="virginica"],pch=3,col="red")
points(fitsA[species=="versicolor"],sresA[species=="versicolor"],pch=8,col="blue")
abline(0,0)
#plot(iris.lmA)


#Model B
iris.lmB<-lm(petalL~sepalL+species,data=iris,qr=T)
summary(iris.lmB)

#plot(sepalL,petalL,type="n",main="PetalL versus SepalL: Model B")
par(mfrow=c(1,1))
plot(seq(-2,8,0.1),seq(-2,8,0.1),type="n",main="PetalL versus SepalL: Model B")
abline(0,0)
abline(0,0,v=0)
points(sepalL[species=="setosa"],petalL[species=="setosa"],pch=1)
points(sepalL[species=="virginica"],petalL[species=="virginica"],pch=3,col="red")
points(sepalL[species=="versicolor"],petalL[species=="versicolor"],pch=8,col="blue")
#legend(6.5,2.5,c("setosa","virginica","versicolor"),marks=c("1","3","8"))
ld<-seq(0,8,0.1)
lines(ld,predict(iris.lmB,data.frame(sepalL=ld,species=factor(rep("setosa",length(ld))),type="response")))
lines(ld,predict(iris.lmB,data.frame(sepalL=ld,species=factor(rep("virginica",length(ld))),type="response")),col="red")
lines(ld,predict(iris.lmB,data.frame(sepalL=ld,species=(rep("versicolor",length(ld))),type="response")),col="blue")

sresB<-ls.diag(iris.lmB)$std.res
fitsB<-fitted(iris.lmB)

plot(fitsB,sresB,type="n",main="Standardized Residuals vs Fitted Values")
points(fitsB[species=="setosa"],sresB[species=="setosa"],pch=1)
points(fitsB[species=="virginica"],sresB[species=="virginica"],pch=3,col="red")
points(fitsB[species=="versicolor"],sresB[species=="versicolor"],pch=8,col="blue")
abline(0,0)
