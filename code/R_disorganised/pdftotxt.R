# library(tidyverse)
library(data.table)
library(bib2df)
library(readtext)
library(quanteda)
library(stringdist)
library(stringi)
library(stringr)
library(bib2df)
library(janitor)
library(glue)
library(purrr)

here <- rprojroot::find_rstudio_root_file()

`%notin%` <- Negate(`%in%`)

# print("getting file list")
# filename_w_path <- list.files(glue("{here}/data/pdfs/", full.names = TRUE))

# print("getting file list again")
# filename_no_path <- as.data.table(list.files(glue("{here}/data/pdfs/")) %>% str_remove("\\.pdf"))

# names(filename_no_path) <- "title"

# print("combining them")
# files <- cbind(filename_no_path, filename_w_path)

# refs <- as.data.table(read_rds("./data/all_pubs_post_2010.Rds"))

# refs2 <- clean_names(as.data.table(bib2df(glue("{here}/data/all_dois_out.bib"))))
# fwrite(refs2, "./data/all_dois_out.csv")


# print("reading in the list of references.")
# refs <- fread(glue("{here}/data/all_dois_out.csv"))

# filter out ones we already have
# print("filtering out ones already got")

print("getting list of text files we have")
txt_files <- as.data.table(list.files(glue("{here}/data/txt/")) %>% str_remove("\\.txt"))

names(txt_files) <- "doi"

# files_in_doi1 <- bib2df(glue("{here}/data/zotero2/my_library_20210907.bib"))
# files_in_doi <- as.data.table(files_in_doi1)
# files_in_doi %>% fwrite("./data/zotero2/my_library_20210906.csv")

print("reading in list of references")
files_in_doi <- fread(glue("{here}/data/zotero2/my_library_20210906.csv"))
files_in_doi[FILE %like% "storage", file_location := str_extract(FILE, "zotero2.*?\\.pdf")]
files_in_doi[FILE %like% "pdfs", file_location := str_extract(FILE, "pdfs.*?\\.pdf")]
files_in_doi <- files_in_doi[, file_location := str_replace_all(file_location, "\\\\\\\\", "/")]
files_in_doi[,doi_to_match := str_replace_all(DOI, "\\/", "_")]

print("filtering out files already converted")
files_in_doi <- files_in_doi[doi_to_match %notin% txt_files$doi]


# filename <- "Male pupils taught by female homeroom teachers show a higher preference for"

match_and_move_text <- function(doi){
  # doi <- "10.1017/s0003055418000187"
  # doi <-  "10.1016/s0265-9646(11)00003-8"
  file <- files_in_doi[DOI == doi]
  
  
  file_name_w_path <- paste0(here,"/data/", file$file_location)
  doi_out <- str_replace_all(doi, "\\/", "_")
  out_name <- paste0(doi_out, ".txt")
  
  if(!file.exists(file_name_w_path)){
    return()
  }
  
  out_path <- glue("{here}/data/txt/{out_name}")
  
  if(file.exists(out_path)){
    print(paste0(out_name, " exists"))
    return()
  }
  else{
    print(glue("touching {out_name}"))
    file.create(out_path)
  }
  
  system(glue('pdftotext -raw -nopgbrk "{file_name_w_path}" - | tr "\n" " " > {here}/data/txt/{out_name}'))
  
  print(glue("outputted {out_name}"))

  
  if(file.size(out_path) == 0){
    print(paste0(out_path, " is 0, deleting"))
    cat(glue("{file_name_w_path},{out_name}"), file = glue("{here}/output/pdf_to_txt_log.txt"), sep = "\n", append = TRUE)
    unlink(out_path)
  }
  
  return()  
}

files_to_copy <- files_in_doi[!is.na(file_location)]$DOI
map(files_to_copy, ~match_and_move_text(.x))



# future_map(files$title, ~match_and_move_text(.x))


# 
# 
# 
# match_and_move_text(test2)
# 
# 
# 
# refs$title[1]
# 
# refs$title[1:10]
# 
# refs[title %like% test_title]
# refs[doi %like% "10.1017"]
# 
# refs[title %like% ""]
# 
# 
# refs2[amatch(test2, refs2$title, method = "jw", maxDist = 0.30)]
# 
# 
# refs2[title %like% "Maritime labour"]
# 
# target <- "Maritime labour and subaltern geographies of internationalism: black internationalist seafarers' organising in the interwar period"
# 
# stringdist(test2, target, method = 'jw')
# 
# refs2 %>% filter(title %like% "Measuring fishery") %>% select(title)
# refs2 %>% filter(title %like% test2) %>% select(title)
# 
# 
# 
# # stringdist::amatch(test_title, refs$title)
# 
# refs_got <- refs[title %in% files_no_name$title]
# refs_got[,file_path := paste0("./data/pdfs/", title, ".pdf")]
# 
# 
# 
# corpus <- corpus(readtext(refs_got$file_path)$text)
# 
# corpus %>% write_rds("./data/corpus.rds")
# stat_dict <- read_lines("./data/quant_dict.txt")
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