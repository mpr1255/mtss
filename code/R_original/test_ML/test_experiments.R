library(e1071)
library(caTools)
library(caret)
library(ConfusionTableR)
library(dplyr)
library(gridGraphics)

# TRAINING: Linear SVM on experimental data -------------------------------------------------------

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

predictions <- cbind(data.frame(train_preds=test_pred_grid, 
                                testing$exp_bool)) %>%
  mutate(testing.exp_bool = as.factor(testing.exp_bool))

binary_visualiseR(train_labels = predictions$train_preds,
                  truth_labels= predictions$testing.exp_bool,
                  class_label1 = "Non-experiment", 
                  class_label2 = "Experiment",
                  quadrant_col1 = "#28ACB4", 
                  quadrant_col2 = "#4397D2", 
                  custom_title = "Experiment Papers \nConfusion Matrix", 
                  text_col= "black")

grid.echo()

bc_plot <- grid.grab()
grid.draw(bc_plot)

# ggsave("graphs/experiment_svm_plot.png",
#        plot = bc_plot,
#        width = 7,
#        height = 7,
#        scale = 1.25,
#        limitsize = FALSE,
#        dpi=700)
