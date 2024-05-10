source("./code/R/libs.R")





files_in_doi <- fread("./output/zotero_lib_20210823_1717.csv")
files_to_code <- fread("./output/experiment_sample_round1.csv")
# files_to_code <- fread("./data/files_to_code_20210824.csv")
files_to_code[,file_in := str_replace_all(file_in, "_", "\\/")]

files_in_doi <- files_in_doi[DOI %in% files_to_code$file_in]

files_in_doi[FILE %like% "storage", file_location := str_extract(FILE, "zotero2.*?\\.pdf")]
files_in_doi[FILE %like% "pdfs", file_location := str_extract(FILE, "pdfs.*?\\.pdf")]
files_in_doi <- files_in_doi[, file_location := str_replace_all(file_location, "\\\\\\\\", "/")]


match_and_move_pdf <- function(doi_in){
  # doi_in <- files_in_doi$DOI[1]
  in_file <- paste0(here, "/data/", files_in_doi[DOI == doi_in]$file_location)
  out_file <- paste0(here, "/data/code_experiments_round1/", str_remove_all(files_in_doi[DOI == doi_in]$TITLE, "\\{|\\}|�|"), ".pdf")
  
  
  if(file.exists(out_file)){
    print(paste0(out_file, " exists"))
    return()
  }else{
    # print(glue("touching {out_name}"))
    file.copy(in_file, out_file)
    print(paste0(in_file, " copied to ", out_file))
  }
  
  
  return()
  
  
}

map(files_in_doi$DOI, ~match_and_move_pdf(.x))