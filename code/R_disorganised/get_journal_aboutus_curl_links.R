require(searchConsoleR)
require(httr)
require(tidyverse)
require(data.table)
require(glue)
require(here)
require(janitor)
require(curl)

here <- here()


output2 <-  readxl::read_excel(glue("{here}/data/journal_list_about_us_results.xlsx"))


get_html <- function(url, keyword, serp, folder, title){
  
  folder <- str_trim(folder)
  
  if(!dir.exists(glue("{here}/data/journal_policies/{folder}"))){
    dir.create(glue("{here}/data/journal_policies/{folder}"))
    print(glue("creating {folder} folder"))
  }
  # url <- "http://www.stanford.edu/~bgirod/pdfs/LiangVirginIsllands2002.pdf"
  filetypes <- c("\\.pdf", "\\.doc", "\\.docx", "\\.xls", "\\.xlsx", "\\.ppt", "\\.html$", "\\.htm$")
  
  # title <- "C:\\DOC\\Macro with diversity, Inflation Modelling and Beliefs\\Basic"
  title <- str_replace_all(title, "\\W", " ")
  title <- str_replace_all(title, "\\s+", " ")
  
  
  
  ext_bool <- !is.na(str_match(url, filetypes))
  ext_position <- which(ext_bool, arr.ind = FALSE, useNames = TRUE)
  ext <- str_extract(filetypes[ext_position], "\\w+")
  if(identical(ext, character(0))){
    ext <- "html"
  }
  
  timenow <- strftime(Sys.time(), format="%Y%m%d--%H-%M-%S")
  
  file_name_dir <- glue("{here}/data/journal_policies/{folder}/{keyword}--{serp}--{title}.{ext}")
  
  if(!file.exists(file_name_dir)){
    try(curl_download(url, file_name_dir, mode = "wb"))
    print(glue("wrote {file_name_dir} {timenow}"))
  }else{
    print(glue("{file_name_dir} exists! {timenow}"))
  }
  
  
}

# output1 <- output1[1:5]

# output2 <- output2[1]

# got_files <- file.list(glue("{here}/data/journal_policies/*"), )

pmap(list(output2$link, output2$keyword, output2$serp, output2$journal, output2$title), ~get_html(..1, ..2, ..3, ..4, ..5))

