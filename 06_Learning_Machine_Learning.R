#Machine Learning in R from the following tutorial: 
#https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.1qOXxOY
#Using caret - does KNN method (k-nearest neighbors) but we can change easily to "RF"
library(ggvis)
library(class)
library(gmodels)
library(caret)
library(e1071)
library(randomForest)
#1. Get your data----------------
#Built_in_IRIS_dataset
iris
#From UC-Irvine Machine learning repository
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header=FALSE) 
#Print first lines
head(iris)
#add column names
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris

#2. Know your data----------------------
#Iris scatter plots using ggvis
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill=~Species) %>% layer_points()
#Test overall correlation between 'Petal.Length' and 'Petal.Width'
cor(iris$Petal.Length, iris$Petal.Width)
#return values of 'iris' levels
x=levels(iris$Species)
#Print Setosa, correlation matrix
print(x[1])
cor(iris[iris$Species==x[1],1:4])

#Print Versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2],1:4])

#Print Virginica correlation matrix
print(x[3])
cor(iris[iris$Species==x[3],1:4])

str(iris)

#Division of 'Species'
table(iris$Species)

#Percentual division of 'Species'
round(prop.table(table(iris$Species))* 100, digits=1)

summary(iris)

#3. Where to go now?---------------
#This example: Species is the target variable
#We should also try one of the numerical classess as a target variable


#4. Prepare your workspace--------------
#Get package "class"

#5. Prepare your data----------------------
#Normalization makes data easier for the KNN algorithm to learn
#Two types of normalization: 
#example normalization (normalize each case individually)
#feature normalization (adjust each feature in the same way across all cases)
#Normalization a good idea when one attribute has a wide range of values compared to others
#User-defined normalization function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#Normalize data 
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

#Divide into training and test sets
#Most common splitting choice: 2/3 original set as training set
#1/3 as test set
#Need to make sure instances of all 3 species equal in the training set

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

#Define & inspect training set
iris.training <- iris_norm[ind==1, 1:4]
head(iris.training)
#Define & inspect test set
iris.test <- iris_norm[ind==2, 1:4]
head(iris.test)
#Store class labels as factor vectors and divide over training and test sets
iris.trainLabels <- iris[ind==1,5]
print(iris.trainLabels)
iris.testLabels <- iris[ind==2,5]
print(iris.testLabels)

#6. KNN Model

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

#7. Evaluate model

#Merge iris.TestLables and iris_pred
iris.testLabels <- data.frame(iris.testLabels)
merge <- data.frame(iris_pred, iris.testLabels)
names(merge) <- c("Predicted Species", "Observed Species")
merge
#Row 29 is wrong

#Make contingency table using gmodels
str(iris.testLabels)
str(iris_pred)
#Not working - 39 observations in one and 40 in the other
CrossTable(x=iris.testLabels, y=iris_pred, prop.chisq=FALSE)

#8. Machine Learning with "caret"
#Split data into training and test set ( alittle different) - ratio 75/25
#Create index to split based on labels
index <- createDataPartition(iris$Species, p=0.75, list=FALSE)
index
#Subset training set
iris.training <- iris[index,]
iris.test <- iris[-index,]
#Overview of algorithms supported by caret function
names(getModelInfo())
#Train a model (trying both KNN and random forest)
model_knn <- train(iris.training[,1:4], iris.training[,5], method='knn')
model_rf <- train(iris.training[,1:4], iris.training[,5], method='rf')

#Predict based on model
predictions <- predict(object=model_rf, iris.test[,1:4])
table(predictions)
confusionMatrix(predictions, iris.test[,5])
str(predictions)
str(iris.test[,5])
#Try out preprocessing methods
model_knn <- train(iris.training[,1:4], iris.training[,5], method='knn',
preProcess=c("center", "scale"))
predictions <- predict(object=model_knn, iris.test[,1:4], type="raw")
confusionMatrix(predictions, iris.test[,5])
