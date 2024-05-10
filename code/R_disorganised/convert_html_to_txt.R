library(boilerpipeR)
library(future)
library(purrr)
library(data.table)
library(glue)
library(here)
library(htm2txt)
library(tictoc)
library(tidyverse)

here <- here()

all_htmls_path <- list.files(glue("{here}/data/journal_policies/"), recursive = TRUE, pattern = "html", full.names = TRUE)
all_htmls_names <- list.files(glue("{here}/data/journal_policies/"), recursive = TRUE, pattern = "html", full.names = FALSE)
# all_htmls1 <- all_htmls[1]

# all_htmls_names2 <- str_extract(all_htmls_names[1], "^.*\\/")
# all_htmls_names2 <- str_extract(all_htmls_names[1], "^.*\\/")

save_text <- function(html_w_path, html_name){
  folder <- str_extract(html_name, "^.*\\/")
  html_name <- str_remove(html_name, "^.*\\/")
  
  final_file <- glue("{here}/data/journal_policies_text/{folder}/{html_name}.txt")
  
  if(file.exists(final_file)){
    return("file exists!")
  }
  
  text <- try(gettxt(html_w_path, encoding = "UTF-8"))
  
  if(!dir.exists(glue("{here}/data/journal_policies_text/{folder}"))){
    dir.create(glue("{here}/data/journal_policies_text/{folder}"))
  }
  
  try(write_lines(text, final_file))
  invisible()
}

tic()
map2(all_htmls_path, all_htmls_names, ~save_text(.x, .y))
toc()

# gettxt(all_htmls1, encoding = "UTF-8") %>%  write_lines(., glue("{here}/data/journal_policies_text/test1.txt"))
# toc()

# tic()
# gettxt(all_htmls1, encoding = "UTF-8") %>% as.list() %>% fwrite(., glue("{here}/data/journal_policies_text/test_dt.txt"), showProgress = TRUE, sep = "\n")
# toc()
