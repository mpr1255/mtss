library(tidyverse)
library(lubridate, warn.conflicts = FALSE)

source("./code/R/libs.R")
here <- rprojroot::find_rstudio_root_file()

# setwd("/Volumes/PortableSSD/trust_us/output")
data <- fread("./output/109k_papers_all_coded_for_pub1.csv")

all_dart_journals <- c("American Journal Of Political Science", "American Political Science Review", "British Journal Of Political Science", "Comparative Political Studies", "Conflict Management And Peace Science", "European Journal Of Political Research", "European Union Politics", "International Interactions", "Journal Of Conflict Resolution", "Journal Of European Public Policy", "Journal Of Peace Research", "Party Politics", "Political Analysis", "Political Behavior", "Political Science Research And Methods", "The Journal Of Politics")

dart_low_journals <- c("Conflict Management And Peace Science", "Comparative Political Studies", "European Journal Of Political Research", "European Union Politics", "Journal Of European Public Policy", "Party Politics")

dart_low_reg_pubs <- data[journal_name %like% paste0(dart_low_journals, collapse = "|")]
pubs_to_inspect <- dart_low_reg_pubs[order(-published_print_ym_date_format), head(.SD, 50), by = journal_name][,.(doi, journal_name, published_print, title, dart_year, published_print_ym_date_format)]

dart_html_dir <- glue("{here}/data/dart_verification/html/")

MonolithDownloadDOI <- function(f_doi){
  # f_doi <- "10.1111/1475-6765.12502"
  # f_doi <- "10.1177/1465116519870870"
  # 
  # f_doi <- pubs_to_inspect$doi[1]

  journal_name <- str_to_lower(str_replace_all(pubs_to_inspect[doi == f_doi]$journal_name, "\\s+", "_"))
  out_file <- paste0(dart_html_dir, journal_name, "--", str_replace_all(f_doi, "/", "_"), ".html")
  
  if(file.exists(out_file)){
    print(paste(basename(out_file),"exists!"))
    return()
  }
  
  print(f_doi)
  print(journal_name)
  
  doi_url <- paste0("https://www.doi.org/", f_doi)
  res <- curl_fetch_memory(doi_url)
  final_url <- res$url
  print(final_url)
  
  doi_and_final <- as.data.table(cbind(doi_url, final_url))
  fwrite(doi_and_final, "./data/dart_verification/doi_urls_and_final_urls.csv", append = T, quote = T)
  syscommand <- glue("monolith '{final_url}' -ekIavn -u 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0' -o {out_file}")
  print(syscommand)
  system(syscommand)
  
  if(file.exists(out_file)){
    print(paste("downloaded", basename(out_file)))  
  }
  
}

SafelyMonolithDownloadDOI <- safely(MonolithDownloadDOI)
map(pubs_to_inspect$doi[54], ~SafelyMonolithDownloadDOI(.x))


