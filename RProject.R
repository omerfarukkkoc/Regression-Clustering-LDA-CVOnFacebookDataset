#Ömer Faruk KOÇ
#09.01.2019

install.packages("animation")
install.packages("factoextra")
install.packages("caret")
install.packages("pvclust")
install.packages("klaR")
install.packages("e1071")
installed.packages("fpc")
library(e1071)
library(caret)


FacebookDataset <- read.csv("C:/Users/omerfarukkoc/Desktop/Facebook.csv", sep=";")

#Cleaning Missing Values
sum(is.na(FacebookDataset)); apply(FacebookDataset, 2, function(x)sum(is.na(x)))
LikeMean = round(mean(FacebookDataset[!is.na(FacebookDataset$like),]$like),0); LikeIndex <- which(is.na(FacebookDataset$like)); FacebookDataset$like[LikeIndex] = LikeMean
TotalInteractionsMean = round(mean(FacebookDataset[!is.na(FacebookDataset$Total.Interactions),]$Total.Interactions),0); TotalInteractionsIndex <- which(is.na(FacebookDataset$Total.Interactions)); FacebookDataset$Total.Interactions[TotalInteractionsIndex] = TotalInteractionsMean
sum(is.na(FacebookDataset)); apply(FacebookDataset, 2, function(x)sum(is.na(x)))



#Multiple Linear Regression
FacebookDataset[,"Degree.Of.Interaction"] =NULL
FacebookDataset[,"Positive.or.Negative.Interaction"] =NULL
sampleSize <- floor(0.70 * nrow(FacebookDataset));cat("Training Sample Size: ",sampleSize)
index = sample(nrow(FacebookDataset), size = sampleSize)
trainData <- FacebookDataset[index,]
testData <- FacebookDataset[-index,]
linearModel <- lm(trainData$Degree.Of.Interaction1 ~., trainData)
summary(linearModel) 
plot(linearModel)
confint(linearModel)


#Cross Validation K-NN Clustering
FacebookDataset[,"Degree.Of.Interaction"] =NULL
FacebookDataset[,"Positive.or.Negative.Interaction"] =NULL
FacebookDataset$Degree.Of.Interaction1 = factor(FacebookDataset$Degree.Of.Interaction1)
FacebookDataset[sapply(FacebookDataset, is.numeric)] <- lapply(FacebookDataset[sapply(FacebookDataset, is.numeric)], scale)
set.seed(123) 
testLength <- 1:30
trainSet <- FacebookDataset[-testLength,]; testSet <- FacebookDataset[testLength,]
trainSetClass <- FacebookDataset$Degree.Of.Interaction1[-testLength]; testSetClass <- FacebookDataset$Degree.Of.Interaction1[testLength]

knnModel <-  train(trainSet, trainSetClass,method="knn")
plot(knnModel); print(knnModel)

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
knnCVModel <- train(trainSet, trainSetClass, trControl = train.control, method="knn")
plot(knnCVModel); print(knnCVModel)



#Ward Hierarchical Clustering Method
library(pvclust); FacebookDataset[,"Degree.Of.Interaction"] =NULL; FacebookDataset[,"Degree.Of.Interaction1"] =NULL; FacebookDataset[,"Positive.or.Negative.Interaction"] =NULL
wardModel <- pvclust(t(FacebookDataset), method.hclust="ward.D2", method.dist="euclidean")
plot(wardModel)
pvrect(wardModel, alpha=.5)



#NonHierarchicalClustering(k-means)
FacebookDatasetKlon = FacebookDataset; FacebookDataset[,"Degree.Of.Interaction"] =NULL; FacebookDataset[,"Degree.Of.Interaction1"] =NULL; FacebookDataset[,"Positive.or.Negative.Interaction"] =NULL
df <- scale(FacebookDataset); head(df, n = 3)
cluster1 <- kmeans(FacebookDataset,3)
head(cbind(FacebookDataset, cluster = cluster1$cluster))
table(cluster1$cluster, FacebookDatasetKlon$Degree.Of.Interaction)
library(cluster)
clusplot(scale(FacebookDataset[-1]), cluster1$cluster, main='Representation of the Cluster', color=TRUE, shade=TRUE, labels=2, lines=0)
library(fpc)
plotcluster(FacebookDataset, cluster1$cluster)
library(factoextra)
fviz_cluster(cluster1, data = df)

#iterationalK-means
library(animation)
cluster2 <- kmeans.ani(FacebookDataset,3)
head(cbind(FacebookDataset, cluster = cluster2$cluster))
table(cluster2$cluster, FacebookDatasetKlon$Degree.Of.Interaction1)




#Manova
manovaa <- manova(cbind(FacebookDataset$like, FacebookDataset$share) ~ FacebookDataset$Degree.Of.Interaction, data = FacebookDataset)
summary.aov(manovaa)



#Discriminant Analysis
library(MASS)
FacebookDataset[,"Degree.Of.Interaction1"] =NULL
FacebookDataset[,"Degree.Of.Interaction"] =NULL
FacebookDataset[,"Positive.or.Negative.Interaction"] =NULL
ldaModel <- lda(FacebookDataset$Positive.or.Negative.Interaction~. , FacebookDataset, na.action="na.omit", CV=TRUE)
ldaModel
head(ldaModel$posterior, 2)
plot(ldaModel)
print(ldaModel)

library(caret)
set.seed(123)
training.samples <- createDataPartition(FacebookDataset$Positive.or.Negative.Interaction, p = 0.8, list = FALSE)
train.data <- FacebookDataset[training.samples, ]
test.data <- FacebookDataset[-training.samples, ]
train.data$Positive.or.Negative.Interaction = as.factor(train.data$Positive.or.Negative.Interaction)
ldaModell <- lda(train.data$Positive.or.Negative.Interaction~., train.data)
predictions <- predict(ldaModell, test.data); predictions
acccuracy = mean(predictions$class==test.data$Positive.or.Negative.Interaction); acccuracy
plot(ldaModell)

lda.data <- cbind(train.data, predict(ldaModell)$x)
ggplot(lda.data, aes(LD1)) +
  geom_point(aes(color = Positive.or.Negative.Interaction))
plot(ldaModel$class, col = as.integer(train.data$Positive.or.Negative.Interaction))
library(klaR)
partimat(FacebookDataset$Positive.or.Negative.Interaction ~ FacebookDataset$Total.Reach + FacebookDataset$Total.Impressions + FacebookDataset$Engaged.Users + FacebookDataset$Before.Post, FacebookDataset, method="lda")


