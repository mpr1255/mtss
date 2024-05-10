library(here)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/R/libs.R"))

rds_files <- list.files(glue("{here}/data/crossref_dumps/"))
target_journals <- fread(glue("{here}/output/jcr_combined_list.csv"))
crossref_list <- fread(glue("{here}/output/crossref_journal_list.csv"))


setnames(target_journals, "issn", "pissn")
setnames(target_journals, "e_issn", "eissn")
target_journals[,pissn := str_remove(pissn, "-")]
target_journals[,eissn := str_remove(eissn, "-")]

target_journal_crossref_matches <- crossref_list[target_journals, on = c("pissn", "eissn")]

convert_to_csv <- function(rds){
  file_out <- paste0("./data/crossref_dumps_csv/", rds, ".csv")
  
  if(file.exists(file_out)){
    print(paste0(file_out, "exists already"))
    return()
  }
  file_w_path <- paste0(here,"/data/crossref_dumps/", rds, ".Rds")
  temp_read <- read_rds(file_w_path) %>% clean_names
  temp_read <- temp_read %>% select(container_title, title, created, published_print, doi, issued, prefix, publisher)
  # temp_read <- temp_read %>% select(container_title, created, published_print, published_online, doi, issued, prefix, publisher)
  
  temp_read %>% fwrite(file_out)
  if(file.exists(file_out)){
    print(paste0(file_out, " now exists!"))
  }
  invisible()
}

map(target_journal_crossref_matches$journal_name, ~try(convert_to_csv(.x)))