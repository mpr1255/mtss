source("./code/R/libs.R")
library(abbrevr)
here <- rprojroot::find_rstudio_root_file()

# all_post_2010 <- read_rds("./data/all_pubs_post_2010.Rds")

# crossref_list <- read_rds("./data/all_pubs.Rds")
# setDT(crossref_list)
# crossref_list <- crossref_list %>% clean_names()

# crossref_list1 <- crossref_list[,.(container_title, created, deposited, published_online, published_print, doi, indexed, issn, publisher, title)]

# crossref_list1 %>% fwrite("./data/all_pubs.csv")

target_journals <- fread(glue("{here}/output/jcr_combined_list.csv"))
crossref_list <- fread(glue("{here}/data/all_pubs.csv"))

setnames(target_journals, "issn", "pissn")
setnames(target_journals, "e_issn", "eissn")
target_journals[,pissn := str_remove(pissn, "-")]
target_journals[,eissn := str_remove(eissn, "-")]


crossref_list <- crossref_list %>% separate(issn, c("pissn", "eissn"), sep = ",", remove = F)
crossref_list[,pissn := str_remove_all(pissn, "-")]
crossref_list[,eissn := str_remove_all(eissn, "-")]

# crossref_list[container_title %like% "British Journal of Politics"] %>% sample_n(100)

# Remove repeat appearances of journal names
crossref_list %>% distinct(container_title) %>% nrow
crossref_list %>% distinct(pissn, .keep_all = T)
crossref_list[stri_length(eissn) != 8]

# Fix up British Journal of Politcs and International Relations
crossref_list[container_title %like% "British Journal of"] %>% count(container_title)
crossref_list[container_title %like% "British Journal of Politics & International Relations"]$container_title <- "The British Journal of Politics and International Relations"
crossref_list[container_title %like% "The British Journal of Politics & International Relations"]$container_title <- "The British Journal of Politics and International Relations"

crossref_list_target_journals <- crossref_list[eissn %in% target_journals$eissn | pissn %in% target_journals$pissn | eissn %in% target_journals$pissn | pissn %in% target_journals$eissn]

crossref_list_target_journals[published_online > 2010] %>% distinct(container_title)

crossref_list_target_journals[published_online > 2010, uniqueN(container_title)]
crossref_list_target_journals[published_print > 2010, uniqueN(container_title)]

cr_print2010 <- crossref_list_target_journals[published_print > 2010][order(-published_print)]

# Add journal jcr_category
ir <- fread("./output/JCR_International_Relations.csv", header = T)
ps <- fread("./output/JCR_Political_Science.csv") 
psir <- rbind(ir, ps, fill=T) %>% clean_names
setnames(psir, "issn", "pissn")
setnames(psir, "e_issn", "eissn")
psir[,pissn := str_remove(pissn, "-")]
psir[,eissn := str_remove(eissn, "-")]
psir[, ir_bool := ifelse(category %like% "INTERNATIONAL RELATIONS", 1, 0)]
psir[, ps_bool := ifelse(category %like% "POLITICAL SCIENCE", 1, 0)]
psir <- psir[,.(pissn, eissn, ir_bool, ps_bool)]
ir_bool <- psir[ir_bool == 1, -c("ps_bool")]
ps_bool <- psir[ps_bool == 1, -c("ir_bool")]

ps_bool <- c(ps_bool$pissn, ps_bool$eissn)
ps_bool <- ps_bool[ps_bool != "N/A"]

ir_bool <- c(ir_bool$pissn, ir_bool$eissn)
ir_bool <- ir_bool[ir_bool != "N/A"]

cr_print2010[eissn %in% ir_bool | pissn %in% ir_bool, ir_bool := 1]
cr_print2010[eissn %in% ps_bool | pissn %in% ps_bool, ps_bool := 1]
cr_print2010[is.na(ir_bool), ir_bool := 0]
cr_print2010[is.na(ps_bool), ps_bool := 0]


# Add abbreviation --------------------------------------------------------
abbrev_titles <- cr_print2010 %>% 
  distinct(container_title) %>% 
  mutate(container_title = str_to_title(container_title)) %>% 
  mutate(abbrev_name = map_chr(container_title, ~AbbrevTitle(.x)))

abbrev_titles[abbrev_name %like% "NANA", abbrev_name := str_replace_all(abbrev_name, "NANA", "Pol.")]

cr_print2010[, container_title := str_to_title(container_title)]

cr_print2010 <- abbrev_titles[cr_print2010, on = "container_title", allow.cartesian=TRUE]

# cr_print2010 %>% write_rds("./output/all_109k_included_papers_cleanish.Rds")
cr_print2010 %>% fwrite("./output/all_109k_pubs_cleanish.csv")

##################### Ignore the following. These are our actual working files and this was just some stuff I had to figure out.
##############################
# cr_online2010 <- crossref_list_target_journals[published_online > 2010]
# 
# cr_online2010[doi %notin% cr_print2010$doi] %>% fwrite("./output/missing_papers_")
# 
# cr_online2010[doi %notin% cr_print2010$doi, uniqueN(doi), by = "container_title"] %>% print(160)
# 
# cr_online2010[container_title == "British Journal of Political Science"][order(published_print),.(published_online, published_print)]
# cr_print2010[container_title == "American Political Science Review"]
# 
# options(datatable.prettyprint.char=50L)
# 
# target_journals_crossref_matches[,uniqueN(container_title)]
# 
# setDT(crossref_list)
# setDT(all_post_2010)
# all_post_2010[published_print > "2010-01",.(container_title, created, deposited, published_online, published_print, published_print_year, doi, indexed, issn, publisher, title)] 
# 
# all_post_2010[title %like% "Congressional Representation"]
# 
# all_csv_files <- list.files("./data/crossref_dumps_csv/", full.names = T)
# 
# all_csv <- map_df(all_csv_files, ~fread(.x, colClasses = 'character'))
# all_csv %>% fwrite("./output/all_papers_from_most_journals_without_regard_to_year.csv")
# 
# all_csv[container_title %like% "Legislative Studies"][title %like% "Congressional Representation"]

