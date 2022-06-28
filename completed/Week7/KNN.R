names(surgery_df)

# https://www.datacamp.com/community/tutorials/machine-learning-in-r

## Step One: Get Your Data

iris

# load 'ggvis'
# install.packages("ggvis")
# install.packages("htmltools") # if out of date
install.packages("gmodels")
# library(ggvis)
library(ggplot2)
library(class)
library(gmodels)


## Step Two: Know Your Data

# iris scatter plot
# iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
ggplot(data = iris, mapping = aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = iris, mapping = aes(Petal.Length, Petal.Width, color = Species)) + geom_point() + geom_smooth(method = "lm")

# Overall correlation `Petal.Length` and `Petal.Width`
cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)
# Return values of `iris` levels 
x=levels(iris$Species)
# Print Setosa correlation matrix
print(x[1])
cor(iris[iris$Species==x[1],1:4])
# Print Versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2],1:4])
# Print Virginica correlation matrix
print(x[3])
cor(iris[iris$Species==x[3],1:4])

# Return first 5 lines of `iris`
head(iris)
# Return structure of `iris`
str(iris)
str(surgery_df)

# Division of `Species`
table(iris$Species) 
# Percentual division of `Species`
round(prop.table(table(iris$Species)) * 100, digits = 1)
# Summary overview of `iris`
summary(iris) 
# Refined summary overview
summary(iris[c("Petal.Width", "Sepal.Width")])


## Step Three: Where To Go Now?

## Step Four: Prepare Your Workspace

## Step Five: Prepare Your Data

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
# Compose training set
iris.training <- iris[ind==1, 1:4]
# Inspect training set
head(iris.training)
# Compose test set
iris.test <- iris[ind==2, 1:4]
# Inspect test set
head(iris.test)

# Compose `iris` training labels
iris.trainLabels <- iris[ind==1,5]
# Inspect result
print(iris.trainLabels)
# Compose `iris` test labels
iris.testLabels <- iris[ind==2, 5]
# Inspect result
print(iris.testLabels)


## Step Six: The Actual KNN Model

# Build the model
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
# Inspect `iris_pred`
iris_pred


## Step Seven: Evaluation of Your Model

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)




### ===================================================


ggplot(data =  bi_class_df, mapping = aes(x, y, color = label)) + geom_point() + geom_smooth(method = "lm")
summary(bi_class_df)
str(bi_class_df)

bi_class_df.2 <- bi_class_df
bi_class_df.2$label <- factor(bi_class_df$label)
ggplot(data = bi_class_df.2, mapping = aes(x, y, color = label)) + geom_point() + geom_smooth(method = "lm")
summary(bi_class_df.2)
str(bi_class_df.2)
head(bi_class_df.2)

set.seed(4242)
ind.2 <- sample(2, nrow(bi_class_df.2), replace=TRUE, prob=c(5/7, 2/7))
bi_class.2.train <- bi_class_df.2[ind.2==1, 2:3]
bi_class.2.test <- bi_class_df.2[ind.2==2, 2:3]
bi_class.2.train.labels <- bi_class_df.2[ind.2==1,1]
bi_class.2.test.labels <- bi_class_df.2[ind.2==2,1]

bi_class_pred.2 <- knn(train = bi_class.2.train, test = bi_class.2.test, cl = bi_class.2.train.labels, k=3)
bi_class_pred.2

CrossTable(x = bi_class.2.test.labels, y = bi_class_pred.2, prop.chisq=FALSE)


bi_class_df.3 <- bi_class_df.2
bi_class.3.train <- bi_class_df.3[2:3]
bi_class.3.test <- bi_class_df.3[2:3]
# bi_class.3.labels <- bi_class_df.3[1]
bi_class.3.labels <- bi_class_df.3$label

bi_class_pred.3 <- knn(train = bi_class.3.train, test = bi_class.3.test, cl = bi_class.3.labels, k = 3)

CrossTable(x = bi_class.3.labels, y = bi_class_pred.3, prop.chisq = FALSE)

bi_class_pred.3b <- knn(train = bi_class_df.3[2:3], test = bi_class_df.3[2:3], cl = bi_class_df.3$label, k = 3)
CrossTable(x = bi_class_df.3$label, y = bi_class_pred.3b, k = 3)

summary(classifier_glm)

bc2.testlabels <- bc2.testlabels$label

CrossTable(x = bc2.testlabels, y = bc_pred, k = 3)
head(bc2.testlabels)
head(bc_pred)
bc2.testlabels
bc_pred
# percent accuracy
sum(bc2.testlabels==bc_pred)/length(bc_pred)