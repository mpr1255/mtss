here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/R/libs.R"))

# library(quanteda)
# library(writexl)

print("getting file list")
filename_w_path <- as.data.table(tibble(file_w_path = list.files(glue("{here}/data/txt/"), full.names = TRUE)))

print("getting file list again")
filename_no_path <- as.data.table(tibble(file = list.files(glue("{here}/data/txt/")) %>% str_remove("\\.txt")))

files <- cbind(filename_w_path, filename_no_path)

# files  <-  head(files, 1000)

refs2 <- fread(glue("{here}/data/zotero2/my_library_20210907.csv")) %>% clean_names()

# quant_dict <- as.data.table(tibble(term = str_trim(read_lines(glue("{here}/output/quant_dict.txt")))))
experimental_dict <- fread(glue("{here}/output/experimental_dict.txt"), sep = NULL, header = FALSE)
experimental_dict <- as.data.table(map_df(experimental_dict, ~str_to_lower(.x)))


# Make a function that counts up all the terms and assembles them into a row along with the paper
get_count <- function(file_in){
  # file_in <- "10.1007_s11109-015-9319-3"
  # file_in <- "10.1007_s11109-018-9511-3"
  file_in <- "10.1093_isq_sqy018"
  # file_in <- test_paper
  # tic()
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
  
  fulltext <- fread(file_w_path, header = FALSE, sep = NULL)
  
  
  if(!fulltext %like% "experiment"){
    return()
  }
  
  fulltext <- str_remove_all(fulltext, "�")
  fulltext <- str_to_lower(paste0(fulltext))
  
  this_doi <- str_replace_all(file_in, "_", "\\/")
  title <- refs2[doi == this_doi]$title
  journal <- refs2[doi == this_doi]$journal
  
  
  nchar <- nchar(fulltext)
  
  count_experiment <- stri_count_regex(fulltext, "experiment")
  
  if(count_experiment < 2){
    return()
  }
  
    # experimental_term_count <- experimental_dict %>% mutate(count = future_map_int(V1, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  experimental_term_count <- experimental_dict %>% mutate(count = map_int(V1, ~stri_count_regex(fulltext, .x)))
  
  t_experimental_term_count <-  transpose(experimental_term_count, make.names = TRUE)
  
  experiment_in_header <- ifelse(str_to_lower(title) %like% "experiment", 1, 0)
  
  exp_sum <- sum(experimental_term_count$count)
    
  exp_avg <- sum(experimental_term_count$count)/nrow(experimental_term_count)
  
  res <- cbind(file_in, title, journal, t_experimental_term_count, count_experiment = count_experiment, exp_header = experiment_in_header, exp_sum, exp_avg, nchar)
  
  fwrite(res, glue("{here}/output/experiment_counts_20210910.csv"), append = TRUE)
  
  return(res)
  
}


# exp2 <- fread("./output/experimental_count_20210902.csv")

experimental_count_full <- map_df(files$file, ~get_count(.x))

experimental_count_full %>% fwrite(glue("{here}/output/experimental_count_full_20210907.csv"))

exp_count_edit <- fread("./output/experiment_prereg_context3.csv")

exp_count_edit[prereg_term_context %like% "osf.io|egap.org|aspredicted.org", uniqueN(file_in)]

exp_count_edit[prereg_term_context %like% "osf|egap|aspredicted", uniqueN(file_in)]

exp_count_edit[prereg_term_context %like% "osf|egap|aspredicted"] %>% fwrite("./output/experiment_prereg_context4.csv")


exp_count_edit[, uniqueN(file_in)]





















# Disregard below ---------------------------------------------------------------
# 
# 
# experimental_count_round2 %>% fwrite("./output/experimental_scores_round2.csv")
# 
# experimental_count %>% fwrite("./output/experimental_count_20210902.csv")
# 
# experimental_count %>% sample_n(600)  %>% fwrite("./output/experiment_sample_round1.csv", sep = "\t")
# 
# experimental_count %>% writexl::write_xlsx("./output/experiment_count.xlsx")
# experimental_count <- readxl::read_excel("./output/experiment_count.xlsx")
# setDT(experimental_count)
# 
# # experimental_count %>% fwrite("./output/experimental_count.csv")
# 
# # experimental_count1 <- map(experimental_count[,-c("file_in", "title", "journal")], ~as.numeric(.x))
# 
# 
# experimental_count[, term_count := rowSums(.SD), by=file_in, .SDcols = -c("title", "journal", "nchar")][]
# 
# corpus(experimental_count[1:4], docid_field = "file_in")
# 
# corp <- corpus(experimental_count[1:4], 
#                      docvars = data.frame(party = names(experimental_count)))
# 
# experimental_count %>% arrange(term_count) %>% writexl::write_xlsx("./output/experiment_count1.xlsx")
# 
#
# Disregard above ---------------------------------------------------------------

















# experimental_count_full <- fread("./output/experimental_count_full_20210907.csv")
# prereg_count <- map_df(experimental_count_full$file_in, ~try(get_prereg(.x)))
# 
# terms <- c("aspredicted", "osf", "egap", "e-gap")
# terms_ast <- paste0("***", terms, "***")
# for(i in seq_along(terms)){
#   prereg_count[,context := str_replace_all(context, terms[i], terms_ast[i])]
# }
# test1 <- prereg_count[context %like% paste(terms, collapse = "|"), relevant := TRUE]



# Load coded prereg -------------------------------------------------------

# prereg_codes <- fread("./output/prereg_count2_links.csv")
# prereg_codes[relevant == TRUE] %>% count(validate)
# 
# 
# test_for_terms <- map_df()
# 
# 
# fwrite(test1, "./output/prereg_count2.csv")
# 

# Couldn't figure out how to purrr it. 
# prereg_count[,context:= map2_dfr(terms, paste0("***", terms,"***"), ~str_replace_all(prereg_count$context, .x, .y))]
# map2(terms, paste0("***", terms,"***"), ~stri_replace_all_coll(prereg_count$context[1:2], .x, .y))
# map(prereg_count$context[1:20], ~stri_replace_all_coll(.x, terms, paste0("***", terms,"***")))


# prereg_count[context %like% "e-gap", .N]
# 
# files_w_egap <- read_lines("./data/misc/other_experimental_papers.txt")
# files_w_egap <- as.data.table(files_w_egap)
# 
# prereg_count[file_in %in% files_w_egap]
# 
# files_w_egap[files_w_egap %notin% experimental_count_full$file_in]
# 
# prereg_count[file_in %notin% ]
# 
# 
# 
# 
# prereg_count[title == "NULL"]$title <- "none"
# prereg_count[journal == "NULL"]$journal <- "none"
# 
# write_xlsx(prereg_count, "./output/prereg_context.xlsx")
# 
# setDT(prereg_count)
# 
# prereg_count[is.na(prereg_count)]
# 
# prereg_count %>% distinct(file_in)
# 
# prereg_count[context %like% "egap", .N]
# prereg_count[context %like% "osf", .N]
# prereg_count[context %like% "aspredicted", .N]
# 
# is.null()
# 
# View(prereg_count)
# 
# fwrite(prereg_count, "./output/prereg_count.csv")
# 
# prereg_count
# 
# 
# prereg_count[context %like% "http"]
# 
# prereg_count %>% View



