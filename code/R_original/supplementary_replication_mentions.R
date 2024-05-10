library(tictoc)
library(furrr)
plan(multisession, workers = 8)

source("./code/R/libs.R")

here <- rprojroot::find_rstudio_root_file()

print("getting file list")
filename_w_path <- as.data.table(tibble(file_w_path = list.files(glue("{here}/data/txt/"), full.names = TRUE)))

print("getting file list again")
filename_no_path <- map(filename_w_path, ~str_remove(.x, glue("{here}/data/txt//")))
filename_no_path <- map_chr(filename_no_path$file_w_path, ~str_remove(.x, glue("\\.txt")))
files <- cbind(filename_w_path, filename_no_path)

# files  <-  head(files, 1000)

refs2 <- fread(glue("{here}/output/all_dois_out.csv"))

other_terms <- c("See Annex", "Data presented in this article", "Underlying data for this paper", "supporting")

related_words <- c("data", "file", "code", "material", "information", "do-files", "folder", "author", "result", "archive", "script")


cbind_fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}


# find_offending_character <- function(x, maxStringLength=256){  
#   print(x)
#   for (c in 1:maxStringLength){
#     offendingChar <- substr(x,c,c)
#     #print(offendingChar) #uncomment if you want the indiv characters printed
#     #the next character is the offending multibyte Character
#   }    
# }
# string_vector <- fulltext
# lapply(string_vector, find_offending_character)


# main function
get_string_match1 <- function(file_in){
  # file_in <- "10.1016_j.marpol.2016.10.008"
  file_w_path <- files[file == file_in]$file_w_path
  print(file_in)
  
  if(!file.exists(file_w_path)){
    return()
  }
  
  fulltext <- fread(file_w_path, header = FALSE, sep = NULL, encoding = "UTF-8")
  fulltext <- str_remove_all(fulltext, "�")
  fulltext <- str_to_lower(paste0(fulltext))
  
  
  if(length(fulltext) == 0){
    return()
  }
  
  # Find inside files for closest match to each of the replication strings
  # %like% from data.table was the fastest implementation
  if(!fulltext %like% "replicat|supplement|supporting"){
    return()
  }
  
  locations <- as.data.table(stri_locate_all_regex(fulltext, "replicat|supplement|supporting")[[1]])
  if(is.na(locations$start[1])){
    return()
  }
  
  context <- map2(locations$start, locations$end, ~substr(fulltext, as.integer(.x)-50, as.integer(.y)+200))
  
  context_bool <- map(context, ~stri_detect_regex(.x, paste0(related_words, collapse = "|")))
  if(!any(unlist(context_bool))){
    return()
  }
  context <- context[unlist(context_bool)]
  
  replication <- context[context %like% "replicat"]
  supplementary <- context[context %like% "supplement|supporting"]
  
  terms_to_match <- c("data", "replication", "materials", "online")
  
  rep_sup_bound <- as.data.table(cbind.fill(replication, supplementary))
  names(rep_sup_bound) <- c("replication", "supplementary")

  title <- refs2[doi == str_replace_all(file_in, "_", "/")]$title
  
  res <- as.data.table(cbind(file_in, title, rep_sup_bound))
  
  return(res) 
}


# test <- sample_n(files, 1000)$file

tic()
res <- future_map_dfr(files$file, ~get_string_match(.x))
toc()
# res %>% fwrite("./output/supplementary_and_replication_mentions.csv")


# Try another string matching algorithm -----------------------------------

get_string_match2 <- function(file_in){
  # file_in <- files$filename_no_path[1]
  file_w_path <- files[filename_no_path == file_in]$file_w_path
  print(file_in)
  
  if(!file.exists(file_w_path)){
    return()
  }
  
  fulltext <- fread(file_w_path, header = FALSE, sep = NULL, encoding = "UTF-8")
  fulltext <- str_remove_all(fulltext, "�")
  fulltext <- str_to_lower(paste0(fulltext))
  
  
  if(length(fulltext) == 0){
    return()
  }
  
  # Find inside files for closest match to each of the replication strings
  # %like% from data.table was the fastest implementation
  if(!fulltext %like% "replicat|supplement|supporting|underlying"){
    return()
  }
  
  locations <- as.data.table(stri_locate_all_regex(fulltext, "(replication|supplementary|supplemental|supporting|underlying)(?:\\W+\\w+){0,6}?\\W+(data|materials|material|files)(?:\\W+\\w+){0,10}?\\W+(http|https|www)")[[1]])
  if(is.na(locations$start[1])){
    return()
  }
  
  context <- map2(locations$start, locations$end, ~substr(fulltext, as.integer(.x)-50, as.integer(.y)+200))
  
  # context_bool <- map(context, ~stri_detect_regex(.x, paste0(related_words, collapse = "|")))
  # if(!any(unlist(context_bool))){
  #   return()
  # }
  # context <- context[unlist(context_bool)]
  # 
  # replication <- context[context %like% "replicat"]
  # supplementary <- context[context %like% "supplement|supporting"]
  # 
  # rep_sup_bound <- as.data.table(cbind.fill(replication, supplementary))
  # names(rep_sup_bound) <- c("replication", "supplementary")
  
  title <- refs2[doi == str_replace_all(file_in, "_", "/")]$title
  
  res <- as.data.table(cbind(file_in, title, context))
  
  return(res) 
}

tic()
res <- future_map_dfr(files$filename_no_path, ~get_string_match2(.x))

replicat_underly <- res[context %notlike% "dataverse|10\\.7910" & context %like% "replicat|underly"]
replicat_underly[,doi := str_replace_all(file_in, "_", "/")]

jpr_data <- fread("./output/jpr_replication_data_matches.csv")

replicat_underly <- replicat_underly[doi %notin% jpr_data$doi]
replicat_underly$file_in <- paste0(replicat_underly$file_in)
replicat_underly$title <- paste0(replicat_underly$title)
replicat_underly$context <- paste0(replicat_underly$context)
replicat_underly %>% fwrite("./output/papers_w_precarious_data.csv")

supplementary <- res[context %notlike% "dataverse|10\\.7910" & context %like% "suppleme|support"]
supplementary$file_in <- paste0(supplementary$file_in)
supplementary$title <- paste0(supplementary$title)
supplementary$context <- paste0(supplementary$context)
supplementary %>% fwrite("./output/papers_w_supplementary_supporting_data.csv")





toc()

str(res4_deduped)

# Pull out website references ---------------------------------------------
rep_mentions <- fread("./output/supplementary_and_replication_mentions.csv")

rm <- rep_mentions[replication %like% "http|www" & replication %notlike% "10.7910" & replication %notlike% "dataverse"]
refs2[,file_in := str_replace_all(doi, "/", "_")]
rm <- refs2[rm, on = "file_in"]
rm[journal %like% "Peace Research"] %>% distinct(file_in)



rep_mentions[replication %like% "http"][replication %notlike% "10.7910" & replication %notlike% "dataverse"] %>% writexl::write_xlsx("./output/replication_with_non_dataverse_links.xlsx")

rep_mentions[replication %like% "http|www" & replication %notlike% "10.7910" & replication %notlike% "dataverse" & replication %like% "author" & replication %like% "website"] %>% writexl::write_xlsx("./output/replication_links_author_website.xlsx")

rep_mentions[replication %like% "http|www" & replication %notlike% "10.7910" & replication %notlike% "dataverse" & replication %like% "author" & replication %like% "website|site|page|personal"] %>% writexl::write_xlsx("./output/replication_links_author_website1.xlsx")


rep_mentions[replication %like% "http|www" & replication %notlike% "10.7910" & replication %notlike% "dataverse"] %>% writexl::write_xlsx("./output/replication_links_author_website2.xlsx")


#replication %like% "author" & replication %like% "website|site|page|personal"]

rep_mentions[replication %like% "http" & replication %notlike% "10.7910" & replication %notlike% "dataverse" & replication %like% "author" & replication %like% "website"] %>% writexl::write_xlsx("./output/replication_links_author_website.xlsx")

res[!is.na(replication)][replication %like% "http" & replication %notlike% "10.7910" & replication %notlike% "dataverse"][,.(file_in, replication)] %>% print(30)

res[!is.na(replication)][replication %like% "author" & replication %like% "website"]

res[!is.na(supplementary)][supplementary %like% "http"] %>% nrow()
uniqueN(res$replication)

toc()


View(res)

length(unique(res$file_in))

map_df(res, ~tibble(.x)) %>% View()
toc()

res







# Sys.time()
# toc()
# Sys.time()
# tic()
# op <- options(digits.secs = 6)
# Sys.time()






# Scrapped 
#   Sys.time()
#   replication_location <- as.data.table(tibble(replication_location = paste0(map(res, ~pluck(.x, "location")))))
#   replication_distance <- as.data.table(tibble(replication_distance = paste0(map(res, ~pluck(.x, "distance")))))
#   replication_match <- as.data.table(tibble(replication_match = paste0(map(res, ~pluck(.x, "match")))))
#   
#   dt1 <- cbind(replication_dict, replication_match, replication_distance, replication_location)
#   
#   dt1 <- dt1[replication_distance < 0.03]
#   
#   # Find the surrounding text
#   replication_context <- map(dt1$replication_location, ~substr(fulltext, as.integer(.x)-70, as.integer(.x)+70))
#   replication_context <- as.data.table(tibble(replication_context = paste0(replication_context)))
#   
#   dt1 <- cbind(dt1, replication_context)
#   
#   # Now do the same for supplementary data
#   tic("supplementary dict")
#   res2 <- map(supplementary_dict, ~afind(fulltext, .x, window = nchar(.x), method="jw"))
#   toc()
#   
#   supplementary_location <- as.data.table(tibble(supplementary_location = paste0(map(res2, ~pluck(.x, "location")))))
#   supplementary_distance <- as.data.table(tibble(supplementary_distance = paste0(map(res2, ~pluck(.x, "distance")))))
#   supplementary_match <- as.data.table(tibble(supplementary_match = paste0(map(res2, ~pluck(.x, "match")))))
#   
#   dt2 <- cbind(supplementary_dict, supplementary_match, supplementary_distance, supplementary_location)
#   
#   dt2 <- dt2[supplementary_distance < 0.03]
#   
#   # find surrounding text
#   supplementary_context <- map(dt2$supplementary_location, ~substr(fulltext, as.integer(.x)-70, as.integer(.x)+70))
#   supplementary_context <- as.data.table(tibble(supplementary_context = paste0(supplementary_context)))
#   
#   dt2 <- cbind(dt2, supplementary_context)
# 
#   dt <- cbind(dt1, dt2)
# 
#   return(dt)
# }