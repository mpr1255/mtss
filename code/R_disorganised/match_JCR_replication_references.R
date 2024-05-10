# ##############################
# JCR (Journal of Conflict Resolution) presents something more of a problem than other publications
# Most of them are on the Dataverse. JPR (Journal of Peace Research) had a different system which required careful reconciliation, 
# and it turns out that JCR is even more difficult.
# ##############################
 
source("./code/R/libs.R")

here <- rprojroot::find_rstudio_root_file()


dl_dir <- glue(("{here}/data/jcr_replication"))

all_files <- fread("./output/all_109k_pubs_cleanish.csv")[container_title %like% "Conflict Resolution"]
jcr_dois <- map_chr(all_files[,.(doi)]$doi, ~paste0("https://journals.sagepub.com/doi/suppl/", .x))
jcr_dois <- as.data.table(cbind(jcr_dois, all_files[,.(doi)]$doi))
setnames(jcr_dois, "V2", "doi")

get_url <- function(url){
  # url <- jcr_dois[1]
  print(url) 
  doi <- jcr_dois[jcr_dois == url]$doi
  file <- paste0(str_replace_all(doi,"/","_"), ".html")
  file_w_dir <- paste0(dl_dir, "/", file)
  
  if(file.exists(file_w_dir)){
    return()
  }
  command <- paste0("curl ", url," --cookie-jar -c -L -o ",file_w_dir)
  system(command)
  cat(command)
  
  print(file_w_dir)
  
  return()
  
}

map(jcr_dois$jcr_dois, ~get_url(.x))

system(glue('rg "suppl_file" {dl_dir} > {here}/output/jcr_replication_files.txt'))

jcr_rep <- fread(glue("{here}/output/jcr_replication_files.txt"), sep = ":", header = F)
jcr_rep <- jcr_rep[V2 %notlike% "pdf"][] # negative look behind
jcr_rep[,doi := str_extract(V1, "10.*?(?=\\.html)")][]
jcr_rep[,od_doi := paste0("https://journals.sagepub.com", str_extract(V2, "\\/doi\\/suppl\\/.*?\\.[a-z|7z]{2,3}"))][]
jcr_rep$V1 <- NULL
jcr_rep$V2 <- NULL
jcr_rep <- jcr_rep %>% distinct(doi, .keep_all = T)

jcr_rep %>% fwrite("./output/jcr_replication_data_matches.csv")


