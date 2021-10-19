#PCA:1.reduce the number of variables 2.remove any redundancy between variables
setwd()
mydata <- read.csv("C:/Users/asalb/OneDrive/Documents/test1/utilities.csv")
str(mydata)
head(mydata)
#convert values to z score, is neccessary step in pca aplication becase var use dif scales.
#we use %for mortality rate,use milion for pop//skip if all variable have same scale
mydata.st <- scale(mydata[ , -1]) #remove first fild company
head(mydata.st)
pca <- prcomp(mydata.st)
summary(pca)
#Importance of components:(most variance in data)pc1
#Second most variance pc2:
#rotation attribute of pc object: view the weight assign to each variable 
#weights tend to show the relative importance of each variable in creating pc
pca$rotation
#view principal component score: look x attribute of pca object
pca$x
#result shows final PC score
# now we have pc scores, we can choose to use the first few pc to represent our data set in subsequent analysis.
#run cluster analysis o datasetinstead of using all 8 original var, we can perform the class analysis are only first four pc
#because this allow retain 90 % info in original data.advantages:all pc uncorolated, it would remove any chance of multinarrative in analysis.

#prepare data for subsequent analysis:paste pc score to original data(contry names)
newdata <- data.frame(mydata, pca$x)
newdata <- newdata[ , -(2:8)]
head(newdata)
#pca help to reduce dimentionality of data by replacing large number of original variables with a few
#uncrollated pc that retain very large portion of info in data set

#***************************

library (ISLR)
labs <- NCI60$labs
d <- NCI60$data
dim(d)
#, cancer cell line, gene [1]   64 6830
table(labs)
pc <- prcomp(d , scale=TRUE)
Cols=function (vec ){cols=rainbow (length (unique (vec )))
return (cols[as.numeric (as.factor (vec))])}
plot(pc$x[,1:2], col = Cols(labs), pch =19, xlab ="Z1",ylab="Z2")

plot(pc$x[,c(1,3)], col = Cols(labs), pch =19, xlab ="Z1",ylab="Z3")
summary(pc)
plot(pc)
#**********************************
head(iris)
d <- iris[, 1:4]
pc <- princomp(d, cor= T, score=T) #corrolation= cor
summary(pc)
#Proportion of Variance: how much of your data explain with comp1
plot(pc)
plot(pc, type='l')
biplot(pc) #understand your data
dim(d)
attributes(pc) # we can get sd, loading, scale, scores,call
pc$loading
pc$scores
#collect data , standardization,find covariance matrix(find eignvector and eigenvalue), then reduce matrix.
head(iris)
str(iris)
summary(iris)
c <- prcomp(iris[, -5])
# PCA for specific attribute
#prcomp(-Sepal.Length + Petal.Width, data= iris)

#Demonestrate scale fun
plot(iris$Sepal.Length, iris$Sepal.Width)
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width))
c <- prcomp(iris[, -5], scale = TRUE)
c #eigenvector: rotation
summary(c)
#eigenvector use forcalculation of transformation
plot(c, type='l') #find the number of component that you have to choose!
biplot(c)
str(c)
#bind your data with original data(data+pc1+pc2)
iris2 <- cbind(iris, c$x[, 1:2])
head(iris2)
library(ggplot2)
ggplot(iris2, aes(PC1, PC2, col = Species, fill = Species))+
  stat_ellipse(geom = 'polygon',col = 'black', alpha = 0.5)+
  geom_point(shape = 21, col = 'black')
#we need to understand with related species column how PC1,2 given!col=specis

#corrolation between pc and attributes
cor(iris[,-5], iris2[,6:7])