library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(foreign)

source("./code/R/libs.R")

# setwd("/Volumes/PortableSSD/trust_us/output")
data1 <- fread("./output/109k_papers_all_coded_for_pub.csv")
data <- data1


data1[journal_name %like% "Political Science Quarterly"] %>% distinct(dv_doi)
data1[journal_name %like% "Government and Opposition"] %>% distinct(dv_doi)

data1[dv_doi == "10.7910/DVN/92UDQH"]
data1[dv_doi == "10.7910/DVN/PBB57U"]

# Check date situation
data[published_print %like% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"]

# Fix dates
data[published_print %like% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",published_print_ym := stri_sub(published_print, 1, 7)]
data[published_print %like% "^[0-9]{4}-[0-9]{2}$",published_print_ym := paste0(published_print)]
data[published_print %like% "^[0-9]{4}$",published_print_ym := paste0(published_print, "-01")]

data[,published_print_ym_date_format := parse_date_time(published_print_ym, "ym")]

data[,published_print_year := stri_sub(published_print_ym, 1, 4)]
data[,published_print_month := stri_sub(published_print_ym, 6, 7)]

# Fix issn and some repeated journal names
# data %>% fwrite("./output/109k_papers_all_coded_for_pub1.csv")

# double-check. 
# data[published_print1 %notlike% "^[0-9]{4}$"]
# data[published_print1 %notlike% "^[0-9]{4}-[0-9]{2}$"]
# data[published_print1 %notlike% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"]
# data[published_print %like% "^[0-9]{4}-[0-9]{2}$", published_print1 := parse_date_time(published_print, "ym")]
# data[published_online %like% "^[0-9]{4}-[0-9]{2}$", published_online1 := parse_date_time(published_online, "ym")]
# data[published_print %notlike% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" & published_online %notlike% "^[0-9]{4}-[0-9]{2}$"]
data[published_online == ""]
data[published_print != ""]


data[published_online %like% "^[0-9]{4}-[0-9]{2}$"]
data[published_online %like% "^[0-9]{4}$"]

############################

data[journal_name %like% "The British Journal of Politics & International Relations", uniqueN(url)]
data[journal_name %like% "British Journal of Politics & International Relations", uniqueN(url)]

full_papers_crossref <- fread("./data/all_pubs2.csv")
full_papers_crossref <- full_papers_crossref %>% clean_names

full_papers_crossref[container_title %like% "British Journal of Politics"]


full_papers_crossref[full_papers_crossref[,stri_length(container_title) > 13]]


