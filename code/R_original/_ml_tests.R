dat <- read.csv("https://raw.githubusercontent.com/rashida048/Datasets/master/dat.csv", sep = ',', header = T)

dat1 <- fread("./output/round2_3_coded_rescored.csv")

dat <- dat1[,-c("file_in", "title", "journal", "data_bool")]

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

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(table(test_pred, testing$stat_bool))

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





























################# This fully works. 
heart <- read.csv("https://raw.githubusercontent.com/rashida048/Datasets/master/Heart.csv", sep = ',', header = T)
heart[is.na(heart)] <- 0

intrain <- createDataPartition(y = heart$AHD, p= 0.7, list = FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]

dim(training); 
dim(testing);

anyNA(heart)

training[["AHD"]] = factor(training[["AHD"]])
training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(AHD ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(table(test_pred, testing$AHD))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(AHD ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$AHD))






set.seed (1)
x <- matrix (rnorm (200*2) , ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <-  x[101:150,] - 2
y=c(rep(1,150), rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma =1, cost =1)

plot(svmfit, dat[train,])
summary(svmfit)















############################### didn't work$$$$$$$$$$$$$$$$$$$$$$$$$$$$
############################### 
############################### 
############################### 

dasi <- fread("./output/round2_3_coded_rescored.csv")
full_data <- fread("./output/data_stat_scores_for_all_papers_20210901_2122.csv")
setcolorder(dasi, c("file_in", "title", "journal"))

# dasi$stat_bool <- factor(dasi$stat_bool, levels = c(0,1), labels = c("False", "True"))
dasi$stat_bool <- as.factor(dasi$stat_bool)
indxTrain <- createDataPartition(y = dasi$stat_bool,p = 0.75,list = FALSE)
training <- dasi[indxTrain,]
testing <- dasi[-indxTrain,]

prop.table(table(dasi$stat_bool)) * 100
prop.table(table(training$stat_bool)) * 100
prop.table(table(testing$stat_bool)) * 100

x = training[,6:as.integer(ncol(dasi)-1)]
y = training$stat_bool
dat <- training[,-c("file_in", "title", "journal")]

# for (j in names(dat)){
#   set(dat, which(is.na(dat[[j]])),j,0)
# }
# 
# dat <- as.data.table(map_df(dat, ~as.numeric(.x)))
# 
# dat[,nearZeroVar(dat)]
# 
# dat <- subset(dat, select=colSums(dat) > 0)
# subset(dat, select=colSums(dat) > 0) %>% ncol
# dat %>% ncol

# x <- x %>% clean_names


# svmfit <- svm(dat$stat_bool ~ ., data = dat, kernel = "radial", gamma =1, cost =1)


training[["stat_bool"]] = factor(training[["stat_bool"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10, 
)

























svmfit = svm(y ~ ., data = x, kernel = "linear", cost = 1, scale = FALSE)

test_pred <- predict(svmfit, newdata = testing)

testres <- testing[,6:as.integer(ncol(dasi)-1)]
confusionMatrix(table(test_pred, testres))

testing$stat_bool %>% length
test_pred %>% length



x %>% nrow
y %>% length



dat$svm <- svmfit$fitted

dat %>% select(data_count, stat_count, stat_count_minus_ast, game_formal, stat_game_ratio, stat_bool, fitted) %>% View()





svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



plot(svmfit, dat)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(dat$stat_bool ~ ., data = dat,
                    method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    na.action=na.exclude
)



svm_Linear <- train(training$stat_bool ~ ., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    na.action=na.exclude
)

svm_Linear <- train(y ~ ., data = x, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(table(test_pred, testing$stat_bool))


grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing$V14))
