# Create test set on the classified papers ---

library(e1071)
library(caTools)
library(caret)
library(ConfusionTableR)
library(dplyr)
library(gridGraphics)

dat <- data.table::fread("./output/round2_3_coded_rescored.csv")

dat <- dat[,-c("file_in", "title", "journal", "data_bool")]

dat[is.na(dat)] <- 0

intrain <- createDataPartition(y = dat$stat_bool, p= 0.7, list = FALSE)
training <- dat[intrain,]
testing <- dat[-intrain,]

dim(training); 
dim(testing);

anyNA(dat)

training[["stat_bool"]] = factor(training[["stat_bool"]])
# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing, type = "raw")

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(stat_bool ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

predictions <- cbind(data.frame(train_preds=test_pred, 
                                testing$stat_bool)) %>%
  mutate(testing.stat_bool = as.factor(testing.stat_bool))

cm <- confusionMatrix(predictions$train_preds, predictions$testing.stat_bool)

bc_df <- binary_class_cm(predictions$train_preds, predictions$testing.stat_bool
)

binary_visualiseR(train_labels = predictions$train_preds,
                                   truth_labels= predictions$testing.stat_bool,
                                   class_label1 = "Non-statistical", 
                                   class_label2 = "Statistical",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "Statistical Inference Papers \nConfusion Matrix", 
                                   text_col= "black")

grid.echo()

bc_plot <- grid.grab()
grid.draw(bc_plot)

# ggsave("graphs/stat_inf_svm_plot.png",
#        plot = bc_plot,
#        width = 7,
#        height = 7,
#        scale = 1.25,
#        limitsize = FALSE,
#        dpi=700)

# full_data <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")
