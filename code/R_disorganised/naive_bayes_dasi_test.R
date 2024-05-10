source("./code/R/libs.R")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(naivebayes)


# Create test set on the classified papers --------------------------------

dasi <- fread("./output/round2_3_coded_rescored.csv")
full_data <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")
setcolorder(dasi, c("file_in", "title", "journal"))

dasi$stat_bool <- factor(dasi$stat_bool, levels = c(0,1), labels = c("False", "True"))
indxTrain <- createDataPartition(y = dasi$stat_bool,p = 0.75,list = FALSE)
training <- dasi[indxTrain,]
testing <- dasi[-indxTrain,]

prop.table(table(dasi$stat_bool)) * 100
prop.table(table(training$stat_bool)) * 100
prop.table(table(testing$stat_bool)) * 100

x = training[,6:as.integer(ncol(dasi)-1)]
y = training$stat_bool
# model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model <- naive_bayes(x, y, prior = NULL, laplace = 0)
Predict <- predict(model,newdata = testing) 

testing$predicted <- Predict
testing %>% View()

confusionMatrix(Predict, testing$stat_bool)


# Now run on full data ----------------------------------------------------

dasi <- fread("./output/round2_3_coded_rescored.csv")
full_data <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")
setcolorder(dasi, c("file_in", "title", "journal"))

dasi$stat_bool <- factor(dasi$stat_bool, levels = c(0,1), labels = c("False", "True"))
x = dasi[,6:as.integer(ncol(dasi)-1)]
y = dasi$stat_bool

# try naive_bayes implementation
model <- naive_bayes(x, y, prior = NULL, laplace = 0)
predict <- predict(model,newdata = full_data[,c(-1:-3)])

full_data$predicted <- predict
full_data %>% count(predicted)

# try e1071 implementation
model2 <- e1071::naiveBayes(x, y, prior = NULL, laplace = 1)
predict2 <- predict(model2, newdata = full_data[,c(-1:-3)]) 

full_data$predicted2 <- predict2
full_data %>% count(predicted2)

# testing %>% View()

# confusionMatrix(predict, testing$stat_bool)



# This was a fully working vignette. Happy with how it turned out. 
data <- iris
data$Species <- factor(data$Species, levels = c(0,1), labels = c("False", "True"))

indxTrain <- createDataPartition(y = data$Species,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

prop.table(table(data$Species)) * 100

prop.table(table(training$Species)) * 100
prop.table(table(testing$Species)) * 100

x = training[,1:4]
y = training$Species
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
Predict <- predict(model,newdata = testing ) 
confusionMatrix(Predict, testing$Species)













# Splitting data into train and test data
dasi <- fread("./output/round2_3_coded_rescored.csv")

iris
str(dasi)

split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")



labels <- dasi[,1]


split <- sample.split(labels, SplitRatio = 0.7)
train_cl <- subset(dasi, split == "TRUE")
test_cl <- subset(dasi, split == "FALSE")

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











