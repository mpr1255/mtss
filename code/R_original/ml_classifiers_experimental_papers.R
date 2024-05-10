source("./code/R/libs.R")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(naivebayes)


# TRAINING: Linear SVM on Experimental data -------------------------------------------------------

dat1 <- fread("./output/experiment_handcodes_round2.csv")
dat2 <- fread("./output/experimental_scores_round2.csv")

dat <- dat1[dat2, on = "file_in"][,-c("title", "journal", "file_in")]

dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 20)

intrain <- createDataPartition(y = dat$exp_bool, p=0.75, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training); 
dim(testing);

anyNA(dat)

training[["exp_bool"]] = factor(training[["exp_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(exp_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(table(test_pred, testing$exp_bool))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(exp_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$exp_bool))

confusionMatrix(table(test_pred_grid, testing$exp_bool)) %>% write_lines("./output/ml_svm_exp_test.txt")

# PRODUCTION: Linear SVM with real data ------------------------------------------------------
dat1 <- fread("./output/experiment_handcodes_round2.csv")
dat3 <- fread("./output/experimental_count_full_20210907.csv")
dat3_for_scoring <- dat3[,-c("file_in", "title", "journal")]

experimental_production_data <- fread("./output/experimental_count_full_20210907.csv")
experimental_production_data[,uniqueN(file_in)]

dat <- dat3[dat1, on = "file_in"][,-c("title", "journal", "file_in")]

dat[is.na(dat)] <- 0
dat3_for_scoring[is.na(dat3_for_scoring)] <- 0

dat <- subset(dat, select=colSums(dat) > 0)
dat3_for_scoring <- subset(dat3_for_scoring, select=colSums(dat3_for_scoring) > 5)

intrain <- createDataPartition(y = dat$exp_bool, p=0.8, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training); 
dim(testing);

anyNA(dat)

training[["exp_bool"]] = factor(training[["exp_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# svm_Linear <- train(exp_bool ~., data = training, method = "svmLinear",
#                     trControl=trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 10)
# 
# svm_Linear
# 
# test_pred <- predict(svm_Linear, newdata = dat3_for_scoring)
# 
# dat3_w_exp_scores <- cbind(dat3, test_pred)
# 
# dat3_w_exp_scores %>% count(test_pred)
# 
# confusionMatrix(table(test_pred, testing$exp_bool))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(exp_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

# svm_Linear_Grid %>% write_lines("./output/exp_bool_predictions_SVM_linear_20210911.csv")

exp_bool <- predict(svm_Linear_Grid, newdata = dat3_for_scoring)

exp_scores_production <- cbind(dat3, exp_bool)

exp_scores_production %>% count(exp_bool)

exp_scores_production[,.(file_in, exp_bool)] %>% fwrite("./output/exp_bool_predictions_20210911_1558.csv")

# confusionMatrix(table(test_pred_grid, testing$exp_bool))



# Naive bayes on experimental data ----------------------------------------

dat1 <- fread("./output/experiment_handcodes_round2.csv")
dat2 <- fread("./output/experimental_scores_round2.csv")

dat <- dat1[dat2, on = "file_in"][,-c("title", "journal", "file_in")]

dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 2)

dat[["exp_bool"]] = factor(dat[["exp_bool"]])

indxTrain <- createDataPartition(y = dat$exp_bool,p = 0.75,list = FALSE)
training <- dat[indxTrain,]
testing <- dat[-indxTrain,]

prop.table(table(dat$exp_bool)) * 100
prop.table(table(training$exp_bool)) * 100
prop.table(table(testing$exp_bool)) * 100

x = training[,-"exp_bool"]
y = training$exp_bool

# model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model <- naive_bayes(x, y, prior = NULL, laplace = 0)
Predict <- predict(model,newdata = testing) 

testing$predicted <- Predict


confusionMatrix(Predict, testing$exp_bool)


