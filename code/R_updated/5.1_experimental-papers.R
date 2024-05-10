library(here)
library(glue)
here <- here()
source(glue("{here}/code/libs.R"))
library(furrr)

print("getting file list")
txt_files <- data.table(list.files(glue("{here}/data/txt/"), full.names = TRUE))
txt_files[,basename := basename(V1)]

pubs <- data.table(read_fst(glue("{here}/out/pubs.fst"))) %>% distinct(doi, .keep_all = T)
# quant_dict <- as.data.table(tibble(term = str_trim(read_lines(glue("{here}/output/quant_dict.txt")))))
experimental_dict <- fread(glue("{here}/out/experimental_dict.txt"), sep = NULL, header = FALSE)

# Make a function that counts up all the terms and assembles them into a row along with the paper
getExperimentalCount <- function(f_doi){
  # file_in <- "10.1007_s11109-015-9319-3"
  # file_in <- "10.1007_s11109-018-9511-3"
  # file_in <- "10.1093_isq_sqy018.txt"
  # file_in <- test_paper
  # tic()
  # f_doi <- "10.1093/isq/sqy018"
  txtfile <- str_replace_all(f_doi, "\\/", "_")
  file_w_path <- glue('{here}/data/txt/{txtfile}.txt')
  print(basename(file_w_path))
  
  
  if(length(file_w_path) == 0){
    print(paste0(file_w_path, " variable not filled"))
    return()
  }
  if(!file.exists(file_w_path)){
    print(paste0(file_w_path, " doesn't exist"))
    return()
  }
  if(file.size(file_w_path) == 0){
    print(paste0(file_w_path, " empty"))
    return()
  }
  
  fulltext <- fread(file_w_path, header = FALSE, sep = NULL)
  
  if(!fulltext %like% "experiment") return(NA)
  
  fulltext <- str_remove_all(fulltext, "�")
  fulltext <- str_to_lower(paste0(fulltext))
  
  title <- pubs[doi == f_doi]$title
  journal <- pubs[doi == f_doi]$container_title
  nchar <- nchar(fulltext)
  
  count_experiment <- stri_count_regex(fulltext, "experiment")
  
  if(count_experiment < 2) return(NA)
  
  # experimental_term_count <- experimental_dict %>% mutate(count = future_map_int(V1, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  experimental_term_count <- experimental_dict %>% mutate(count = map_int(V1, ~stri_count_regex(fulltext, .x)))
  t_experimental_term_count <-  transpose(experimental_term_count, make.names = TRUE)
  experiment_in_header <- ifelse(str_to_lower(title) %like% "experiment", 1, 0)
  exp_sum <- sum(experimental_term_count$count)
  exp_avg <- sum(experimental_term_count$count)/nrow(experimental_term_count)
  res <- data.table(f_doi, txtfile, title, journal, t_experimental_term_count, count_experiment, experiment_in_header, exp_sum, exp_avg, nchar)
  
  # fwrite(res, "./out/experiment_counts.csv", append = T)
  
  return(res)
  
}

possibly_getExperimentalCount <- possibly(getExperimentalCount, otherwise = NA)

tictoc::tic()
experimental_count <- map(pubs$doi, ~possibly_getExperimentalCount(.x))
tictoc::toc()


# microbenchmark::microbenchmark(
#   experimental_count <- map(pubs$doi, ~possibly_getExperimentalCount(.x)),
#   experimental_count <- future_map(pubs$doi, ~possibly_getExperimentalCount(.x)),
#   times = 1)

experimental_count[!is.na(experimental_count)] %>% rbindlist() %>% write_fst(., glue("{here}/out/experiment_counts.fst"))

# one <- data.table(read_fst("./out/experiment_counts.fst"))
# two <- fread("./out/experiment_counts.csv")
# dim(two)
# one[f_doi %chin% two$f_doi] %>% nrow

# Now use ML to classify them ---------------------------------------------
# Loading package
library(e1071)
library(caTools)
library(caret)

# TRAINING: Linear SVM on Experimental data -------------------------------------------------------

dat <- fread("./out/experiment_handcodes_round2.csv")
experiment_counts <- data.table(read_fst(glue("{here}/out/experiment_counts.fst")))
experiment_counts[,file_in := txtfile]
j1 <- experiment_counts[file_in %chin% dat$file_in]
dat <- dat[j1, on = "file_in"] 
options(datatable.prettyprint.char=40L)
dat <- dat[file_in != ""]
dat[is.na(exp_bool)] <- 0
dat <- dat[,-c("file_in", "f_doi", "txtfile", "title", "journal")]

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
confusionMatrix(table(test_pred_grid, testing$exp_bool)) %>% write_lines("./out/svm_results_experimental_papers_classifier.txt")

# PRODUCTION: SVM Linear grid with real data ------------------------------------------------------
prod_dat <- data.table(read_fst("./out/experiment_counts.fst"))
prod_dat <- prod_dat[,-c("f_doi","txtfile", "title", "journal")]
prod_dat <- prod_dat[,-1]

names(prod_dat) %chin% names(dat)
names(dat)[names(dat) %notchin% names(prod_dat)]
prod_dat$exp_bool <- 0
prod_dat$`average marginal component effect` <- 0
prod_dat$`choice experi.1` <- 0

prod_dat[is.na(prod_dat)] <- 0
# prod_dat <- subset(prod_dat, select=colSums(prod_dat) > 0)
# intrain <- createDataPartition(y = dat$exp_bool, p=0.8, list = FALSE)
# training <- dat[intrain,]
# testing <- dat[-intrain,]
# dim(training); 
# dim(testing);
# anyNA(dat)
# training[["exp_bool"]] = factor(training[["exp_bool"]])
# # training <- training[,-1]
# 
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# 
# grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
# svm_Linear_Grid <- train(exp_bool ~., data = training, method = "svmLinear",
#                          trControl=trctrl,
#                          preProcess = c("center", "scale"),
#                          tuneGrid = grid,
#                          tuneLength = 10)
# svm_Linear_Grid
# plot(svm_Linear_Grid)

# svm_Linear_Grid %>% write_lines("./output/exp_bool_predictions_SVM_linear_20210911.csv")

experimental_papers_predicted <- predict(svm_Linear_Grid, newdata = prod_dat)
experimental_counts <- data.table(read_fst("./out/experiment_counts.fst"))
experimental_counts$exp_bool <- experimental_papers_predicted
experimental_counts[,.(doi = f_doi, exp_bool)] %>% write_fst("./out/experimental_papers_classified.fst")