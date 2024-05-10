library(glue)
library(here)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/R/libs.R"))

print("getting file list")
filename_w_path <- as.data.table(tibble(file_w_path = list.files(glue("{here}/data/txt/"), full.names = TRUE)))

print("getting file list again")
filename_no_path <- as.data.table(tibble(file = list.files(glue("{here}/data/txt/")) %>% str_remove("\\.txt")))

files <- cbind(filename_w_path, filename_no_path)

# files  <-  head(files, 1000)

refs2 <- fread(glue("{here}/data/zotero2/my_library_20210907.csv")) %>% clean_names()

# quant_dict <- as.data.table(tibble(term = str_trim(read_lines(glue("{here}/output/quant_dict.txt")))))
quant_data_dict <- fread(glue("{here}/output/quant_dict_w_cats.txt"), sep = "\t")

# quant_dict %>% print(100)
quant_asterisk_dict <- as.data.table(tibble(term = c("*", "**", "***")))
quant_p_value_dict <- c("p < 0.001", "p < 0.01", "p < 0.05", "p value", "p-value", "p<0.001", "p<0.01", "p<0.05", "p<", "p <", "p  <", "p>", "p >", "p  >", "p¼", "p ¼", "p  ¼")

game_theory <- read_lines(glue("{here}/output/game_theory_dict.txt"))

scored_papers <- fread(glue("{here}/output/data_stat_scores_for_all_papers_20210901_2122.csv"))

files1 <- files[file %notin% scored_papers$file_in]


# Make a function that counts up all the terms and assembles them into a row along with the paper
get_count <- function(file_in){
  # file_in <- "10.1086_684998"
  # file_in <- "10.1093/afraf/adab005"
  # file_in <- test_paper
  # tic()
  
  if(file_in %like% "/"){
    file_in <- str_replace_all(file_in, "/", "_")
  }
  
  print(file_in)
  file_w_path <- files[file == file_in]$file_w_path
  
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
  
  this_doi <- str_replace_all(file_in, "_", "\\/")
  title <- refs2[doi == this_doi]$title
  journal <- refs2[doi == this_doi]$journal
  
  fulltext <- str_to_lower(fread(file_w_path, header = FALSE, sep = NULL))
  nchar <- nchar(fulltext)
  
  stat_terms <- tibble(term = quant_data_dict[type == "stat"]$term)
  data_terms <- tibble(term = quant_data_dict[type == "data"]$term)
  
  stat_term_count <- stat_terms %>% mutate(count = map_int(term, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  data_term_count <- data_terms %>% mutate(count = map_int(term, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  
  # We're aware of how this inflates the count of asterisks and it is a feature not a bug.
  asterisk_count <- quant_asterisk_dict %>% mutate(count = map(term, ~stri_count_coll(fulltext, .x)))
  
  figure_count <- as.data.table(tibble(figures = stri_count_regex(fulltext, "figure.[0-9]|figures.[0-9]|fig..[0-9]|fig.[0-9]")))
  p_value_count <- as.data.table(tibble(p_values = stri_count_regex(fulltext, paste(quant_p_value_dict, collapse = "|"))))
  
  game_formal <- as.data.table(tibble(game_formal = stri_count_regex(fulltext, paste(game_theory, collapse = "|"))))
  
  
  setDT(stat_term_count)
  setDT(data_term_count)
  
  asterisk_count[1]$term <- "ast_1"
  asterisk_count[2]$term <- "ast_2"
  asterisk_count[3]$term <- "ast_3"
  asterisk_count$count <- as.numeric(paste0(asterisk_count$count))
  setDT(asterisk_count)
  
  # stat_term_count %>% arrange(count)
  # data_term_count %>% arrange(count)
  
  t_stat_term_count <-  transpose(stat_term_count, make.names = TRUE)
  t_data_term_count <-  transpose(data_term_count, make.names = TRUE)
  t_asterisk_count <-  transpose(asterisk_count, make.names = TRUE)
  t_asterisk_count$ast_sum <- sum(t_asterisk_count)
  
  data_count <- sum(data_term_count$count) + sum(figure_count$figures)
  stat_count <- sum(stat_term_count$count) + sum(asterisk_count$count) + sum(p_value_count$p_values)
  stat_count_minus_ast <- stat_count-t_asterisk_count$ast_sum
  stat_game_ratio <- stat_count/(stat_count+game_formal$game_formal)
  
  
  res <- cbind(file_in, title, journal, t_stat_term_count, t_data_term_count, t_asterisk_count, figure_count, p_value_count, data_count = data_count, stat_count = stat_count, stat_count_minus_ast, game_formal, stat_game_ratio, nchar)
  
  return(res)

}

# safely_get_count <- safely(.f = get_count)

# get_count("10.1017_s0007123417000096")

figure_out_400 <- map_df(dv_1_but_stat_data_0$paper_doi, ~get_count(.x))

figure_out_400 %>% arrange(desc(stat_count_minus_ast)) %>% print(30)

# figure_out_400[file_in == "10.1111_j.1540-5907.2011.00544.x"] %>% transpose(., make.names = T) %>% arrange(desc(.)) %>% print(100)

res4_full_count_again <- map_df(files1$file, ~get_count(.x))

res4_full_count_again %>% fwrite(glue("{here}/output/data_stat_scores_all_papers_20210907.csv"))

# Disregard ---------------------------------------------------------------

# round2_3 <- fread("{here}/output/round2_3_codes_clean.csv", sep = "\t")
# res4_full_count_again[is.na(data_bool)]$data_bool <- 0
# round2_3[is.na(stat_bool)]$stat_bool <- 0
# 
# res <- future_map_dfr(round2_3$file_in, ~try(get_count(.x)))
# 
# round3_full_count_again <- round2_3[res4_full_count_again, on = "file_in"]
# 
# round2_3_scored %>% fwrite("{here}/output/round2_3_coded_rescored.csv")
# 
# inspect1 <- round2_3_scored[,.(file_in, stat_bool, stat_count_minus_ast, data_count, game_formal, stat_game_ratio)] %>% 
#   arrange(stat_bool, desc(stat_count_minus_ast)) %>% 
#   slice(1:40)
# 
# 
# round2_3_scored[file_in %in% inspect1$file_in] %>% View()
# 
# 
# get_count("10.1016_j.ejpoleco.2014.10.010") %>% transpose(keep.names = "file_in") %>% arrange(desc(V1)) %>% print(20)
# 
# round3_review <- fread("{here}/output/round3_codes_full.csv")
# round3_review %>% select(file_in, stat_count, stat_count_minus_ast, data_count) %>% arrange(stat_count) %>% print(100)
# 
# res <- future_map_dfr(files$file, ~try(get_count(.x)))
# 
# res4_full_count_again %>% fwrite("{here}/output/data_stat_scores_for_all_papers_20210901_2122.csv")
# # res_test <- res4_full_count_again
# res[stat_count_minus_ast > 40]
# 
# res_test <- fread("{here}/output/data_stat_scores_for_all_papers_20210901_1048.csv")
#   
# res_test %>% count(stat_count > 46) 
# 
# res_test %>% count(stat_count_minus_ast > 26)
# 
# res_test %>% count(data_count > 30)
# 
# 18/70
# .25*109
# 27.25*.80
# 
# 
# 
# res %>% 
#   ggplot(aes(stat_count)) + 
#   geom_histogram(binwidth=10) +
#   scale_x_continuous(breaks = seq(0, 200, 10), limits = c(0, 200)) +
#   scale_y_continuous(breaks = seq(0, 10000, 500), limits = c(0, 10000)) +
#   theme_classic()
# 
# 
# res %>% 
#   ggplot(aes(data_count)) + 
#   geom_histogram(binwidth=10) +
#   scale_x_continuous(breaks = seq(0, 200, 10), limits = c(0, 200)) +
#   theme_classic()
# 
# 
# res %>% 
#   group_by(file_in) %>% 
#   ggplot(aes())
# 
# # use this cutoffs or around them to output the terms. 
# # 
# # data_count cutoff 60 (88%)
# # stat_count cutoff 60 (88%)
# # stat_count_minus_ast cutoff 30 (86%)
# # for 40 the PP is 97%
# # round3_coding <- 
# res1 <- res[data_count > 5 & data_count < 80]
# res2 <- res[stat_count > 5 & stat_count < 80]
# res4 <- sample_n(rbind(res1, res2), 600)
# res4 %>% fwrite("{here}/output/round3_codes_full.csv")
# 
# res4 <- res
# 
# setnames(res4, "file_in", "doi")
# res4[,doi := str_replace_all(doi, "/", "_")]
# 
# 
# round3_coding_clean <- refs2[res4, on = "doi"][,.(doi, str_remove_all(title, "\\{|\\}"), data_count, stat_count, stat_count_minus_ast, game_formal, stat_game_ratio)]
# 
# round3_coding_clean[, data_bool := fifelse(data_count > 60, 1, 0)]
# round3_coding_clean[, stat_bool := fifelse(stat_count > 60, 1, 0)]
# round3_coding_clean[, stat_bool_minus_ast := fifelse(stat_count_minus_ast > 30, 1, 0)]  
# round3_coding_clean %>% fwrite("{here}/output/round3_codes_redacted.csv", sep = "\t")
# 
# 
# # files_to_recode <- fread("{here}/output/files_to_code_20210824.csv")
# 
# test %>% fwrite("{here}/output/recoded_2000_files_from_round2.csv")
# 
# recoded <- fread("{here}/output/recoded_2000_files_from_round2.csv")
# old_scores <- fread("{here}/output/scores_from_coding_round2.csv")
# old_scores[is.na(data_bool)]$data_bool <- 0
# old_scores[is.na(stat_bool)]$stat_bool <- 0
# old_scores <- old_scores[,-c("title", "journal", "data_count", "stat_count")]
# 
# old_scored_out <- old_scores[recoded, on = "file_in"]
# old_scored_out %>% fwrite("{here}/output/round2_new_scores_and_coded.csv")
# 
# test <- map_dfr(sample(files$file, 10), ~get_count(.x))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # pca_res <- princomp(test)    
# # df$index <- PCA_results$scores[1]
# 
# test %>% fwrite("{here}/output/files_to_code_20210824.csv", sep = "\t")
# 
# # test %>% fwrite("{here}/output/stat_term_count_output_500.csv")
# # test <- fread("{here}/output/stat_term_count_output_500.csv")
# 
# test[is.na(test)] <- 0
# test <- test[, lapply(.SD, as.numeric), by=file_in]
# test$sum <- rowSums(test[,-c("file_in")])
# refs2[,match_doi := str_replace(doi, "/", "_")]
# 
# test_for_match <- test[,.(file_in, p_values, sum, ast1, ast2, ast3)][sum > 20 & sum < 200]
# test_for_match[refs2, on = c("file_in" = "match_doi" )][!is.na(sum)][,.(file_in, title, p_values, ast1, ast2, ast3, sum)] %>% fwrite("{here}/output/quant_dict_validation_2_w_p_values_and_asterisks.csv", sep = "\t")
# 
# test %>% 
#   filter(sum < 100) %>%
#   mutate(logsum = log(sum)) %>%
#   ggplot() +
#   geom_point(aes(file_in, sum)) +
#   theme_classic()
# 
# test %>% mutate(logsum = log(sum)) %>% select(file_in, logsum, sum) %>%  sample_n(50) 
# 
# check_paper <- function(doi){
#   test <- get_count(doi)
#   test[is.na(test)] <- 0
#   test <- test[, lapply(.SD, as.numeric), by=file_in]
#   test$sum <- rowSums(test[,-c("file_in")])
#   test <- transpose(test[file_in %like% doi], keep.names = "file_in") %>% arrange(desc(V1))
#   test <- test[file_in != "file_in"]
#   return(head(test, 20))
# }
# check_paper("10.1017_s0007123416000387")
# 
# refs2[doi %like% gsub("_","/",test_paper)]


# tic()
# fulltext <- read_lines(file_w_path)
# toc()


# corpus %>% write_rds("{here}/output/corpus.rds")
# stat_dict <- read_lines("{here}/output/quant_dict.txt")
# dict <- dictionary(list(quant = stat_dict))
# 
# dict %>% paste0()
# 
# res3 <- tokens(corpus) %>%
#   tokens_lookup(dictionary = dict) %>%
#   dfm()
# 
# res4 <- as.data.table(res3)
# 
# res4 %>%
#   # mutate(quant = log(quant)) %>%
#   ggplot(aes(doc_id, quant)) +
#   geom_point() 
# 
# 
#  
# 
# 
# 
# as.dictionary(as.data.table(stat_dict))
# 
# dat_dict <- corpus %>%
#   tokens() %>%
#   tokens_lookup(dictionary = stat_dict,
#                 nested_scope = "dictionary") %>%
#   dfm() %>%
#   convert(to = "data.frame")
# dat_dict
# 
# 
# refs_got$title
# 
# 
# readtext(files[1])
# 
# options(datatable.prettyprint.char=10L)
# 
# str(data_dictionary_LSD2015)

# 
# 
# counts$term <- as.character(counts$term)
# counts$count <- as.character(counts$count)
# counts_t <- transpose(counts, make.names = "term")
# 
# regex_counts$term <- as.character(regex_counts$term)
# regex_counts$count <- as.character(regex_counts$count)
# regex_counts_t <- transpose(regex_counts, make.names = "term")
# names(regex_counts_t) <- c("ast1", "ast2", "ast3")