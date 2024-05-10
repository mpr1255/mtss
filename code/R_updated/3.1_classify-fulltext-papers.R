# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    3.1_classify-fulltext-papers.R                     :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 16:35:21 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/29 19:10:01 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# There are a few steps in the classification of fulltext papers into those that use
# statistics and those that do not.
# The first step is simply counting those features in each fulltext paper.
# The second is running a model on manual classifications of the papers based on
# those features to assess its accuracy. Finally, the model is applied to the full dataset.
# **************************************************************************** #

library(here)
library(glue)
here <- here()
source(glue("{here}/code/libs.R"))
library(furrr)
plan(multisession, workers = 8)

# Step one: count statistical features ------------------------------------------
fulltext_papers <- list.files(glue("{here}/data/txt", full.names = T))
quant_data_dict <- fread(glue("{here}/out/quant_dict_w_cats.txt"), sep = "\t")
stat_terms <- tibble(term = quant_data_dict[type == "stat"]$term)
quant_asterisk_dict <- as.data.table(tibble(term = c("*", "**", "***")))
quant_p_value_dict <- c("p < 0.001", "p < 0.01", "p < 0.05", "p value", "p-value", "p<0.001", "p<0.01", "p<0.05", "p<", "p <", "p  <", "p>", "p >", "p  >", "p¼", "p ¼", "p  ¼")
game_theory <- read_lines(glue("{here}/out/game_theory_dict.txt"))

# Make a function that counts up all the terms and assembles them into a row along with the paper
pubs <- as.data.table(read_fst(glue("{here}/out/pubs.fst")))
pubs <- distinct(pubs, doi, .keep_all = T)
pubs[, got_fulltext := file.exists(paste0(glue("{here}/data/txt/"), txt_file))]

stat_counts <- as.data.table(read_fst(glue("{here}/out/stat_counts.fst")))
stat_counts[file %notlike% "txt", file := paste0(file, ".txt")]
stat_counts[, .(file)]

pubs <- pubs[txt_file %notchin% stat_counts$file]

pubs[, txt_file_size := file.size(paste0(glue("{here}/data/txt/"), txt_file))]

# Filter out all the papers already classified ----------------------------
# The newly classified ones will simply be row-bound to the older ones.
# This structure allows for sequential runs and no duplication of work.

countStatFeatures <- function(file) {
  # file_in <- "10.1086_684998"
  # file_in <- "10.1093/afraf/adab005"
  # file_in <- test_paper
  # tic()
  # file <- fulltext_papers[1]
  file <- paste0(here, "/data/txt/", file)

  print(file)

  if (file.size(file) < 10) {
    return()
  }

  title <- pubs[txt_file == basename(file)]$title
  journal <- pubs[txt_file == basename(file)]$container_title
  fulltext <- str_to_lower(fread(file, header = FALSE, sep = NULL))
  nchar <- nchar(fulltext)
  stat_term_count <- stat_terms %>% mutate(count = map_int(term, ~ stri_count_regex(fulltext, paste0("\\b", .x, "[\\b\\W+]"))))
  # We're aware of how this inflates the count of asterisks and it is a feature not a bug.
  asterisk_count <- quant_asterisk_dict %>% mutate(count = map(term, ~ stri_count_coll(fulltext, .x)))

  figure_count <- as.data.table(tibble(figures = stri_count_regex(fulltext, "figure.[0-9]|figures.[0-9]|fig..[0-9]|fig.[0-9]")))
  p_value_count <- as.data.table(tibble(p_values = stri_count_regex(fulltext, paste(quant_p_value_dict, collapse = "|"))))
  game_formal <- as.data.table(tibble(game_formal = stri_count_regex(fulltext, paste(game_theory, collapse = "|"))))

  setDT(stat_term_count)

  asterisk_count[1]$term <- "ast_1"
  asterisk_count[2]$term <- "ast_2"
  asterisk_count[3]$term <- "ast_3"
  asterisk_count$count <- as.numeric(paste0(asterisk_count$count))
  setDT(asterisk_count)

  t_stat_term_count <- transpose(stat_term_count, make.names = TRUE)
  t_asterisk_count <- transpose(asterisk_count, make.names = TRUE)
  t_asterisk_count$ast_sum <- sum(t_asterisk_count)

  stat_count <- sum(stat_term_count$count) + sum(asterisk_count$count) + sum(p_value_count$p_values)
  stat_count_minus_ast <- stat_count - t_asterisk_count$ast_sum
  stat_game_ratio <- stat_count / (stat_count + game_formal$game_formal)

  file <- basename(file)
  res <- cbind(file, title, journal, t_stat_term_count, t_asterisk_count, figure_count, p_value_count, stat_count = stat_count, stat_count_minus_ast, game_formal, stat_game_ratio, nchar)
  setDT(res)
  return(res)
}
Possibly_countStatFeatures <- possibly(countStatFeatures, otherwise = NA)

tictoc::tic()
new_stat_counts <- map(pubs$txt_file, ~ Possibly_countStatFeatures(.x))
tictoc::toc()
setDT(new_stat_counts)
new_stat_counts <- new_stat_counts[!is.na(new_stat_counts)] %>% rbindlist()

# Make sure there was no overlap
stopifnot(sum(new_stat_counts$file %chin% stat_counts$file) == 0)

# Rbind those with the classified papers and write them out, replacing stat_counts.fst
updated_stat_counts <- rbind(new_stat_counts, scored_papers, fill = T)

# Replace NA with 0
for (j in seq_len(ncol(updated_stat_counts))) {
  set(updated_stat_counts, which(is.na(updated_stat_counts[[j]])), j, 0)
}
updated_stat_counts <- unique(updated_stat_counts, by = "file")
updated_stat_counts %>% write_fst(glue("{here}/out/stat_counts.fst"))

# Step two: classify papers using an ML model --------------------------------------
# Note that you need to have previously trained the model by hand-coding the files.
# Load ML packages
library(e1071)
library(caTools)
library(caret)

train_test_data <- fread("./out/handcoded_stat_papers.csv") # Hand-coded files here for training & test data.
dat <- train_test_data[, -c("file_in", "title", "journal", "data_bool")]
dat[is.na(dat)] <- 0
dat[["stat_bool"]] <- factor(dat[["stat_bool"]])
# dat[, names(dat) := lapply(.SD, as.numeric)]
# dat <- subset(dat, select=colSums(dat) > 5)

intrain <- createDataPartition(y = dat$stat_bool, p = 0.7, list = FALSE)
training <- dat[intrain, ]
testing <- dat[-intrain, ]
dim(training)
dim(testing)
anyNA(dat)

# training <- training[,-1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(stat_bool ~ .,
  data = training, method = "svmLinear",
  trControl = trctrl,
  preProcess = c("center", "scale"),
  tuneLength = 10
)

svm_Linear
predicted <- predict(svm_Linear, newdata = testing)
confusionMatrix(table(predicted, testing$stat_bool))

grid <- expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

if (!file.exists("./out/svm_Linear_Grid.Rds")) {
  svm_Linear_Grid <- train(stat_bool ~ .,
    data = training, method = "svmLinear",
    trControl = trctrl,
    preProcess = c("center", "scale"),
    tuneGrid = grid,
    tuneLength = 10
  )

  svm_Linear_Grid %>% write_rds("./out/svm_Linear_Grid.Rds")
} else {
  svm_Linear_Grid <- read_rds("./out/svm_Linear_Grid.Rds")
}

plot(svm_Linear_Grid)
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$stat_bool)) %>% write_lines("./out/svm_model_results.txt")

# Step three: apply this model to the full dataset ------------------------
# Be very careful to ensure that the columns in the prediction dataset are IDENTICAL to the columns in the training dataset.

new_stat_counts <- as.data.table(read_fst("./out/stat_counts.fst"))
dim(new_stat_counts)
production_prediction <- new_stat_counts[, -c("file", "title", "journal")]
production_prediction[is.na(production_prediction)] <- 0
production_prediction[, names(production_prediction) := lapply(.SD, as.numeric)]
production_prediction$stat_bool <- 0
predictions <- predict(svm_Linear_Grid, newdata = production_prediction)
new_stat_counts$stat_bool <- predictions

# Do some manual validation. Validated 10 of each and accuracy was 100%.
new_stat_counts[, .N, stat_bool]
new_stat_counts[stat_bool == 0, .(file, title, journal)][sample(.N, 10)]
new_stat_counts[stat_bool == 1, .(file, title, journal)][sample(.N, 10)]
# Export ------------------------------------------------------------------
# Result of the above classifications will be a df to save out as an fst file.
# That file will then be used to query the dataverse and other sources.
new_stat_counts %>% write_fst("./out/classified_papers.fst")

### Testing
# new_stat_counts <- data.table(read_fst("./out/classified_papers.fst"))
# new_stat_counts[, .(file, title, data_count, stat_bool)][,.N, by = "stat_bool"]
# new_stat_counts[, .(file, title, data_count, stat_bool)][1:20] %>% View()