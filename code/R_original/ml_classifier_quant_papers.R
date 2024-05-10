source("./code/R/libs.R")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(naivebayes)


# Run on test for stat_bool using linear svm without the dataverse bool (dv_doi) --------------------------------
train_test_data <- fread("./output/round2_3_coded_rescored.csv")
dat <- train_test_data[,-c("file_in", "title", "journal", "data_bool")]
dat[is.na(dat)] <- 0
dat[["stat_bool"]] = factor(dat[["stat_bool"]])
dat <- subset(dat, select=colSums(dat) > 0)

intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training);
dim(testing);

anyNA(dat)

# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

predicted <- predict(svm_Linear, newdata = testing)

confusionMatrix(table(predicted, testing$stat_bool))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(stat_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$stat_bool)) %>% write_lines("./output/ml_svm_stat_test.txt")

# Run on production for stat_bool using linear svm without dv_doi--------------------------------
production_data1 <- fread("./output/data_stat_scores_all_papers_20210907.csv")
production_data2 <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")

nrow(production_data1)
nrow(production_data2)

production_data <- rbind(production_data1, production_data2)

# dat1 <- fread("./output/round2_3_coded_rescored.csv")

training_data <- fread("./output/round2_3_coded_rescored.csv")

dat <- training_data[,-c("file_in", "title", "journal", "data_bool")]

dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 10)

# intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
# training <- dat[intrain,]
# testing <- dat[-intrain,]

# dim(training); 
# dim(testing);

anyNA(dat)

dat[["stat_bool"]] = factor(dat[["stat_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~., data = dat, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

production_for_predict <- production_data[,-c("file_in", "title", "journal")]
# production_for_predict %>% View
production_for_predict[is.na(production_for_predict)] <- 0

pred_production <- predict(svm_Linear, newdata = na.omit(production_for_predict))

production_data4 <- cbind(production_data, pred_production)
production_data4 %>% count(pred_production)

production_data4 %>% fwrite("./output/production_stat_bool_prediction1_20210909.csv")


# Run on test for stat_bool using linear svm WITH the dataverse bool (dv_doi) --------------------------------
train_test_data <- fread("./output/round2_3_coded_rescored.csv")
dv_doi_papers <- fread("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")
handcoded_papers <- fread("./output/round2_3_handcoded_clean.csv")

paper_dois_w_dv <- dv_doi_papers[str_replace_all(paper_doi, "/", "_") %in% handcoded_papers$file_in][,.(paper_doi = str_replace_all(paper_doi, "/", "_"))]

handcoded_papers[file_in %in% paper_dois_w_dv$paper_doi] %>% count(data_bool)
handcoded_papers[file_in %in% paper_dois_w_dv$paper_doi] %>% count(stat_bool)
handcoded_papers[file_in %in% paper_dois_w_dv$paper_doi] %>% View


train_test_data[,dv_doi := ifelse(file_in %in% paper_dois_w_dv$paper_doi, 1, 0)]

dat <- train_test_data[,-c("file_in", "title", "journal", "data_bool")]
dat[is.na(dat)] <- 0
dat <- subset(dat, select=colSums(dat) > 5)
dat[["stat_bool"]] = factor(dat[["stat_bool"]])

intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training);
dim(testing);

anyNA(dat)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

svm_Linear <- train(stat_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

predicted <- predict(svm_Linear, newdata = testing)

confusionMatrix(table(predicted, testing$stat_bool))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(stat_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$stat_bool))



# Run on production for stat_bool using linear svm WITH dv_doi--------------------------------
production_data1 <- fread("./output/data_stat_scores_all_papers_20210907.csv")
production_data2 <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")

training_data <- fread("./output/round2_3_coded_rescored.csv")
dv_doi_papers <- fread("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")
handcoded_papers <- fread("./output/round2_3_handcoded_clean.csv")

production_data <- rbind(production_data1, production_data2)

production_papers_w_dv_dois <- dv_doi_papers[str_replace_all(paper_doi, "/", "_") %in% production_data$file_in][,.(paper_doi = str_replace_all(paper_doi, "/", "_"))]


production_data[,dv_doi := ifelse(file_in %in% production_papers_w_dv_dois$paper_doi, 1, 0)]

# dat1 <- fread("./output/round2_3_coded_rescored.csv")

paper_dois_w_dv_ <- dv_doi_papers[str_replace_all(paper_doi, "/", "_") %in% handcoded_papers$file_in][,.(paper_doi = str_replace_all(paper_doi, "/", "_"))]

training_data[,dv_doi := ifelse(file_in %in% paper_dois_w_dv_$paper_doi, 1, 0)]

dat <- training_data[,-c("file_in", "title", "journal", "data_bool")]

dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 0)

# intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
# training <- dat[intrain,]
# testing <- dat[-intrain,]

# dim(training); 
# dim(testing);

anyNA(dat)

dat[["stat_bool"]] = factor(dat[["stat_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~., data = dat, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

production_for_predict <- production_data[,-c("file_in", "title", "journal")]
# production_for_predict %>% View
production_for_predict[is.na(production_for_predict)] <- 0
production_for_predict <- subset(production_for_predict, select=colSums(production_for_predict) > 0)

ncol(dat)
ncol(production_for_predict)

stat_bool <- predict(svm_Linear, newdata = na.omit(production_for_predict))

production_data4 <- cbind(production_data, stat_bool)
production_data4 %>% count(stat_bool)

# production_data4 %>% fwrite("./output/production_stat_bool_prediction2_w_dv_doi_20210910.csv")
# production_data4 <- fread("./output/production_stat_bool_prediction2_w_dv_doi_20210910.csv")


production_data4[dv_doi == 1 & stat_bool == 0] %>%  fwrite("./output/anomalous predictions bermond check it out_20210910_2252.csv")



# Run training-test for data_bool -----------------------------------------

train_test_data <- fread("./output/round2_3_coded_rescored.csv")
dat <- train_test_data[,-c("file_in", "title", "journal", "stat_bool")]
dat[["data_bool"]] = factor(dat[["data_bool"]])
dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 0)

intrain <- createDataPartition(y = dat$data_bool, p= 0.7, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training);
dim(testing);

anyNA(dat)

# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(data_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

predicted <- predict(svm_Linear, newdata = testing)

dim(testing)
dim(training)

confusionMatrix(table(predicted, testing$data_bool))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(data_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$data_bool)) %>% write_lines("./output/ml_svm_data_test.txt")

# list(confusionMatrix(table(test_pred_grid, testing$data_bool))) %>% write_lines("./output/data_bool_predictions_20210911.txt")


# Run production for data_bool --------------------------------------------

production_data1 <- fread("./output/data_stat_scores_all_papers_20210907.csv")
production_data2 <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")

production_data <- rbind(production_data1, production_data2)

# dat1 <- fread("./output/round2_3_coded_rescored.csv")

training_data <- fread("./output/round2_3_coded_rescored.csv")

dat <- training_data[,-c("file_in", "title", "journal", "stat_bool")]

dat[is.na(dat)] <- 0

dat <- subset(dat, select=colSums(dat) > 0)

# intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
# training <- dat[intrain,]
# testing <- dat[-intrain,]

# dim(training); 
# dim(testing);

anyNA(dat)

dat[["data_bool"]] = factor(dat[["data_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(data_bool ~., data = dat, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear



production_for_predict <- production_data[,-c("file_in", "title", "journal")]

production_for_predict[is.na(production_for_predict)] <- 0

pred_production <- predict(svm_Linear, newdata = production_for_predict)

production_data5_data_bool <- cbind(production_data, pred_production)
production_data5_data_bool %>% count(pred_production)

# production_data5_data_bool %>% fwrite("./output/production_data_bool_prediction1_20210909.csv")

# Had to check some manually just to be sure the ML model actually worked. 
# production_data5_data_bool[pred_production == 1,.(file_in, title, journal)] %>% sample_n(100)