source("./code/R/libs.R")
here <- rprojroot::find_rstudio_root_file()


jpr_rep_files <- fread(glue("{here}/data/jpr_replication_papers.csv"), sep = "|")
jpr_rep_files[,title1 := str_extract(author_title, "(?<=[\\:|\\?]).*")][] # negative look ahead lookahead!!
jpr_rep_files[,match_title := str_extract(title1, "[^\\:|\\?]*")][] # negative look ahead lookahead!!
jpr_rep_files[is.na(match_title), match_title := author_title][] # negative look ahead lookahead!!
jpr_rep_files[author_title %like% "Basque"]

all_papers <- fread("./output/109k_papers_all_coded_for_pub1.csv")
all_jpr_papers <- all_papers[journal_name %like% "Peace Research"]
all_jpr_papers[title %like% "[\\:\\?]",match_title := str_extract(title, "[^\\:|\\?]*")][]
all_jpr_papers[is.na(match_title),match_title := title][]
options(datatable.prettyprint.char=70L)

fuzzy_match_jpr_titles <- function(search_title){
  url <- jpr_rep_files[match_title == search_title]$url
  
  search_title <- str_trim(search_title)
  print(search_title)
  
  title_match <- all_jpr_papers[amatch(str_to_lower(search_title), str_to_lower(all_jpr_papers$match_title), method = "jw", maxDist = 0.24)]$match_title
  
  doi <- all_jpr_papers[match_title == title_match]$doi
  diff <- stringdist(search_title, title_match, method = "jw")
  orig_title <- all_jpr_papers[match_title == title_match]$title
  
  
  
  res <- as.data.table(cbind(orig_title, search_title, title_match, doi, diff, url))
  
  return(res)
}

matches <- map_df(jpr_rep_files$match_title, ~fuzzy_match_jpr_titles(.x))
matches[!is.na(title_match)]
matches[is.na(title_match)]
setorder(matches, -diff)[]
good_matches <- matches[!is.na(doi) & diff < .24]
good_matches %>% fwrite("./output/jpr_replication_data_matches.csv")


# Ignore the below. Exploratory code. -------------------------------------
# all_papers[title %like% "diversity of repression"]
# 
# jpr_rep_files[author_title %like% "Frozen conflicts in world politics"]
# all_jpr_papers[title %like% "Frozen"]
# all_jpr_papers[title %like% "Frozen"]
# good_matches <- matches[diff < 0.22]
# options(datatable.prettyprint.char=30L)
# options(datatable.print.topn = 50L)
# 
# jpr_rep_files[str_trim(jpr_rep_files$title) %notin% good_matches$search_title]
# 
# all_papers[title %like% "Conflict resolution processes" & journal_name %like% "Peace"]$title
# 
# matches[!is.na(doi)]
# matches[!is.na(diff)]
# 
# matches[diff > 0.1, .(diff, search_title, title_match)]
# 
# matches[diff < .29 & diff > 0.22, .(diff, search_title, title_match)] %>% arrange(desc(diff)) 
# 
# options(datatable.prettyprint.char=50L)
# 
# unique(matches, on = "search_title")
# 
# matches[search_title %like% "Does electoral violence affect vote choice"]
# 
# all_papers[title %like% "Does electoral violence affect vote choice"]
# 
# all_jpr_papers[stat_bool == 1]
# 
# matches %>% distinct(search_title)