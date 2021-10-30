# iris data
iris=read.csv("D:/Sujata/iris flower prediction/Iris (1).csv")
View(iris)

#install the packages
library.packages("e1071")
library.packages("caTools")
library.packages("caret")

library(e1071)
library(caTools)
library(caret)

library(ggplot2)

ggplot(data=iris,aes(x=sepal_length ,y=sepal_width))+
  geom_point(aes(color=species,shape=species),size=3)+
  ggtitle("Sepal Length vs Width")

# Let's create  training set
split=sample.split(iris,SplitRatio = 0.7)
train_cl=subset(iris,split=="TRUE")
test_cl=subset(iris,split=="FALSE")
split
train
test_cl

# to classify the differnt class of iris create a naive bayes classifier
nbclassifier=naiveBayes(species ~.,data = train_cl)

nbpredict=predict(nbclassifier,test_cl)
nbpredict

cm=table(test_cl$species,nbpredict)
cm
confusionMatrix(cm)

# use knn clasifier
# install the packages
iris.df=read.csv("D:/Sujata/Iris (1).csv")
View(iris)

install.packages("class")
library(caTools)
library(caret)
library(class)
#creating training set and test set
split=sample.split(iris,SplitRatio = 0.7)
train_cl=subset(iris,split=="TRUE")
test_cl=subset(iris,split=="FALSE")
split
train_cl
test_cl
#Feature scaling
train_scale=scale(train_cl[,1:4])
test_scale=scale(test_cl[,1:4])

#create KNN model
knn_1=knn(train_scale,test=test_scale,cl=train_cl$species,k = 1)
knn_3=knn(train_scale,test=test_scale,cl=train_cl$species,k = 3)
knn_7=knn(train_scale,test=test_scale,cl=train_cl$species,k = 7)
knn_1
knn_3
knn_7
#calculate accuracy for the k value
Error_1=mean(knn_1 !=test_cl$species)
print(paste('Accuracy =',1-Error_1))
Error_3=mean(knn_1 !=test_cl$species)
print(paste('Accuracy =',1-Error_3))
Error_7=mean(knn_1 !=test_cl$species)
print(paste('Accuracy =',1-Error_1))

#confusion matrix for all k values
cm=table(test_cl$Species,knn_1)
cm
confusionMatrix(cm)


cm=table(test_cl$Species,knn_3)
cm
confusionMatrix(cm)

cm=table(test_cl$Species,knn_7)
cm
confusionMatrix(cm)


