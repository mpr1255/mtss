here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/R/libs.R"))

# library(quanteda)
# library(writexl)

# Identify papers with pre-registration -----------------------------------

prereg_terms <- c("preanalysis", "pre-analysis", "Open Science Foundation", "Evidence in Governance and Politics", "egap", "e-gap", "osf", "aspredicted")

prereg_terms_tighter <- c("egap.org", "osf.io", "aspredicted.org")


cbind_fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

get_prereg <- function(file_in){
  # file_in <- "10.1007_s11109-018-9491-3"
  # file_in <- test_paper
  # tic()
  
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
  
  if(!fulltext %like% "prereg|pre-reg"){
    return()
  }
  
  print(file_in)
  
  fulltext <- str_remove_all(fulltext, "�")
  fulltext <- str_to_lower(paste0(fulltext))
  
  this_doi <- str_replace_all(file_in, "_", "\\/")
  title <- refs2[doi == this_doi]$title
  journal <- refs2[doi == this_doi]$journal
  
  nchar <- nchar(fulltext)
  
  
  count_prereg <- stri_count_regex(fulltext, "prereg|pre-reg")
  
  locations <- as.data.table(stri_locate_all_regex(fulltext, "prereg|pre-reg")[[1]])
  if(is.na(locations$start[1])){
    return()
  }
  
  prereg_context <- map2(locations$start, locations$end, ~substr(fulltext, as.integer(.x)-60, as.integer(.y)+400))
  
  prereg_term_locations  <- as.data.table(stri_locate_all_regex(fulltext, paste0(prereg_terms, collapse = "|"))[[1]])
  if(is.na(locations$start[1])){
    return()
  }
  
  prereg_term_context <- map2(prereg_term_locations$start, prereg_term_locations$end, ~substr(fulltext, as.integer(.x)-100, as.integer(.y)+400))
  
  prereg_website_locations  <- as.data.table(stri_locate_all_regex(fulltext, paste0(prereg_terms_tighter, collapse = "|"))[[1]])
  if(is.na(locations$start[1])){
    return()
  }
  
  prereg_website_context <- map2(prereg_website_locations$start, prereg_website_locations$end, ~substr(fulltext, as.integer(.x)-50, as.integer(.y)+200))
  
  res <- as.data.table(cbind_fill(file_in, title, journal, count_prereg, prereg_context, prereg_term_context, prereg_website_context))
  
  names(res) <- c("file_in", "title", "journal", "count_prereg", "prereg_context", "prereg_term_context", "prereg_website_context")
  
  return(res)
  
}

potential_prereg_papers <- map_df(unique(all_potential_exp_papers2[file_in %notin% prereg_coded$file_in]$file_in), ~get_prereg(.x))

potential_prereg_papers %>% fwrite("./output/potential_prereg_papers_final_round.csv")


setDT(potential_prereg_papers)
potential_prereg_papers[,uniqueN(file_in)]

potential_prereg_papers %>% distinct(file_in)


length(unique(all_potential_exp_papers2$file_in))

all_potential_exp_papers <- fread(glue("{here}/output/experimental_count_full_20210907.csv"))

all_potential_exp_papers2 <- fread(glue("{here}/output/experiment_prereg_counts_context_20210910.csv"))

all_potential_exp_papers2 %>% sort(count_experiment)

all_potential_exp_papers2[count_experiment == 2]


# Re-export experimental papers that Bermond has not already examined --------

potential_exp_papers <- fread("./output/experiment_prereg_counts_context_20210910.csv")
marked_ones <- fread("./output/prereg_count2_links.csv")
marked_ones[,uniqueN(file_in)]
bermond_coded <- marked_ones[relevant %like% TRUE & validate == "Yes",unique(file_in)]
papers_to_recode <- potential_exp_papers[file_in %notin% bermond_coded, unique(file_in)] 

###################

res <- map_df(papers_to_recode, ~get_prereg(.x))

res %>% fwrite("./output/experiment_prereg_context3.csv")



# Examine Bermond handcoded files and re-integrate 

prereg_coded1 <- readxl::read_excel("./output/prereg_count2_links_edited.xlsx")
prereg_coded2 <- readxl::read_excel("./output/experiment_prereg_contextl4_links_edited.xlsx")

prereg_coded1 <- prereg_coded1 %>% select(file_in, validate, validate_link) 

prereg_coded2 <- prereg_coded2 %>% select(file_in, validate, validate_link)

prereg_coded <- as.data.table(rbind(prereg_coded1, prereg_coded2))

prereg_coded <- prereg_coded[!is.na(validate)] %>% distinct(file_in, .keep_all = T)



# 


potential_exp_papers[,uniqueN(file_in)]







