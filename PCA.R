data.matrix <- matrix (nrow = 100, ncol = 10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))
rownames(data.matrix) <- paste("gene", 1:100, sep ="")
for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] <- c(wt.values, ko.values)
}
#samples=column, genes=row
head(data.matrix)
dim(data.matrix)

#the goal is to draw a graph shoes how samples are related or not related to eachother
#by defult procomp()expect to sample be row and genes to be column.since samples in our data 
#are column and genes are row.we have to transport the matrix by t()function.
#if we don't transport we get a graph to show how genes related to each other.
pca <- prcomp(t(data.matrix), scale=TRUE) 
#procomp()
#x: principal components(PCs) for drawing a graph.(10 sample here , 10 pc)
#sdev: standard diviation to calculate how much variation in the original data each Pc account for
#rotation
## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])
#use the first two column in x to draw a 2_D plot that uses the first two pc
#the first pc account for most variation in original data,the 2nd second most variation
#to plot 2_D pca graph we use first two pc.however sometimes we use pc2,pc3.
#pc1 on x axis, Pc2 on Y axis
# to get a sense of how meaningful this cluster, let see how much variation in the original data pc1.

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
#calculate percentage of variation pc because it is more interesting than actual value
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data
#which column contain x, Y coordinate and each column contain sample
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
#plot lable rather than dot or some other shape
  geom_text() +
#add x , y lables, here we use paste to combine per of variation with some text to look nice
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
#makes the graph's background white
  theme_bw() +
  ggtitle("My PCA Graph")

#lets look at loading scores to determined which which genes have the largest effect on where samples are ploted in PCA plot.
#the procomp function calls the loading score rotation.
loading_scores <- pca$rotation[,1]
#gene push samples in the left will have a large negative value and genes push sample in to the right will have large positive value.
# sort numbers based on magnitude rather than high to low.
gene_scores <- abs(loading_scores)
#sorting magnitudes of loading scores from high to low.
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
top_10_genes ## show the names of the top 10 genes
#which genes have positive loading score
pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)
##************************************
data("iris")
head(iris)
summary(iris)
myPr <- prcomp(iris[, -5], scale = TRUE)
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width))
myPr
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0)
str(myPr)
myPr$x
iris2 <- cbind(iris, myPr$x[,1:2])
head(iris2)
library(ggplot2)
ggplot(iris2, aes(PC1, PC2, col=Species, fill= Species))+
  stat_ellipse(geom = "polygon", col = "black", alpha= 0.5)+
  geom_point(shape = 21, col= "black")

cor(iris[, -5], iris2[,6:7])

#*******
library(stats)
data <- read.table('All_MedianNorm_TPMs.txt',sep='\t',header=T,check.names=F)

data_matrix <- as.matrix(data[1:dim(data)[1],2:dim(data)[2]])