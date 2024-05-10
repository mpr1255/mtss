################################################################################################################
#
# This file is simply for ensuring that our use of the Crossref API made sense, and didn't leave out 
# some large number of papers.
# 
# It's also to confirm that our list of Dataverse DOIs matched with those in the Grossman and Pedazhur paper.
# 
# In both cases below the data largely matches -- though we have more papers than Key, for some reason.
#
################################################################################################################

# Grossman and Pedahzur 2020 ---------------------------------------------------

source("./code/R/libs.R")
dt <- fread("./data/misc/Grossman_and_Pedahzur_2020_Dataset.csv")

options(datatable.prettyprint.char=100L)

dt <- clean_names(dt)

dt[,dv_doi := str_extract(link_to_replication_materials_page, "10\\.7910.*$")]

known_dv_doi_matches <- fread("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")

dt[dv_doi %in% known_dv_doi_matches$dv_doi]

dt[,uniqueN(dv_doi)]

proof_of_correspondence <- known_dv_doi_matches[dt, on = "dv_doi"][,.(dv_doi, str_extract(link_to_replication_materials_page, "10\\.7910.*$"))] %>% drop_na

# Key 2016 ---------------------------------------------------------------------

# dt2 <- read_delim("./data/misc/Key_2016_replication_PS.tab", delim = "\t")
# dt2 %>% fwrite("./data/misc/Key_2016_replication_PS.csv")
dt2 <- fread("./data/misc/Key_2016_replication_PS.csv")
dt2[,.N, c("jname", "year")]

all_refs <- fread("./output/all_papers_clean.csv")
all_refs[,published_year_month  := substr(published_print, 1, 7)]
all_refs[,published_year  := substr(published_year_month, 1, 4)]

all_refs[journal_name %like% "American Political Science Review|American Journal of Political Science|British Journal of Political Science" & published_year %like% "2013|2014", .N, c("published_year", "journal_name")] 