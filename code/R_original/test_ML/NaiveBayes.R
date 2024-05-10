# Installing Packages
# install.packages("e1071")
# install.packages("caTools")
# install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Species ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Species, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)































# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- read.csv("./code/R/test_ML/binary.csv", header = T)
str(data)
xtabs(~admit+rank, data = data)
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

# Visualization
pairs.panels(data[-1])
data %>%
         ggplot(aes(x=admit, y=gpa, fill = admit)) +
         geom_boxplot() +
         ggtitle("Box Plot")

data %>% ggplot(aes(x=gpa, fill = admit)) +
         geom_density(alpha=0.8, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(admit ~ ., data = train, usekernel = T)
model

train %>%
         filter(admit == "1") %>%
         summarise(mean(gre), sd(gre))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)
