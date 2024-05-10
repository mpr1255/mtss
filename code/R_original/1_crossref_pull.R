library(httr)
library(glue) 
library(curl)
library(rcrossref)
library(tidyverse)
library(data.table)
library(lubridate)
library(here)
library(janitor)
library(furrr)
library(future)

# Note! If you're going to hit the API you'll need to add crossref_email="your email" to file.edit("~/.Renviron")

here <- here()

polisci_journals <- fread(glue("{here}/data/JCR_Political_Science.csv"))
ir_journals <- fread(glue("{here}/data/JCR_International_Relations.csv"),header = TRUE)

journals <- rbind(polisci_journals, ir_journals, fill = TRUE) %>% 
  clean_names() %>% 
  mutate(journal_name = str_to_title(journal_name))

# journals <- journals[1:20]

res <- list()

for(i in seq_len(nrow(journals))){
  
  outname <- glue("{here}/data/crossref_dumps/{journals$journal_name[i]}.Rds")
  
  if(file.exists(outname)){
    print(paste0(outname," exists"))
    next
  }else{
  
    file.create(outname)
    
    cur_journal <- journals$journal_name[i]
    
    print(glue("trying {cur_journal}...."))
    try(res[i] <- tibble(V1 = cr_journals(journals$issn[i], cursor = "*", cursor_max = 1000000, works = TRUE, .progress = TRUE)))
    
    try(res[[i]]$data %>% write_rds(outname))
    
    timenow <- strftime(Sys.time(), format="%Y%m%d%H%M")
    
    try(res %>% write_rds(glue("{here}/data/crossref_dumps/res_{timenow}.Rds")))
 
    print(paste0(cur_journal, " out"))
  }
}

## Count the number of publications in there

journals_got_path <- list.files(glue("{here}/data/crossref_dumps"), full.names = TRUE)
journals_got_names <- list.files(glue("{here}/data/crossref_dumps"))
journals_got <- as.data.table(cbind(journals_got_path, journals_got_names))

journals_got <- journals_got %>%
  mutate(size = map_chr(journals_got_path, ~file.size(.x)))  %>%
  mutate(size = as.integer(size)) %>%
  mutate(journals_got_names = str_remove(journals_got_names, "\\.Rds")) %>%
  rename(journal_name = journals_got_names) 
  
journals_got <- journals_got %>% filter(size > 0)


all_pubs <- future_map_dfr(journals_got$journals_got_path, ~read_rds(.x))

setDT(all_pubs)

all_pubs <- clean_names(all_pubs)

all_pubs[,published_print_year := str_extract(published_print, "\\d{4}")]

all_pubs_post_2010 <- all_pubs[published_print_year >= 2010]

# all_pubs_post_2010 %>% write_rds(glue("{here}/data/all_pubs_post_2010.Rds"))
# all_pubs %>% write_rds(glue("{here}/data/all_pubs.Rds"))

all_pubs_post_2010 <- read_rds(glue("{here}/data/all_pubs_post_2010.Rds"))

all_pubs <- read_rds(glue("{here}/data/all_pubs.Rds"))
all_pubs <- clean_names(all_pubs)

target_journals <- fread("./data/jcr_combined_list.csv")

all_pubs_post_2010[container_title %in% target_journals$journal_name]

setDT(all_pubs)

all_pubs[,published_print_year := str_extract(published.print, "\\d{4}")]

all_pubs_post_2010[container_title %like% "American Political Science"]

all_pubs_post_2010[container_title %like% "American Political Science"]

all_pubs %>% filter(container.title %like% "American Political Science")

all_pubs_post_2010[container_title %like% "Political Analysis"][published_print > 2020]

all_pubs_post_2010[doi %like% "10.1177/00220027211021620"]

all_pubs_post_2010[container_title %like% "Conflict Resolution"] %>% arrange(desc(published_print))

all_pubs_post_2010[container_title %like% "Cold War"] %>% arrange(desc(published_print))

all_pubs_post_2010[doi %like% "00220027211027291"]


############ trying cleanup here ######## NOT RUN 

test <- crminer::crm_links("10.1007/s12286-019-00429-1")

left_join(missing_journals, journals, by = "journal_name")

#cross ref data
# Journal Of Comparative Politics    0 1338-1385 1338-1385
# clarivate data
# Comparative Politics 00104159
# Join them to crossref

all_crossref_journals <- fread(glue("{here}/data/crossref_journal_list.csv"))
all_crossref_journals <- all_crossref_journals %>% rename("journal_name" = JournalTitle) %>%
  mutate(journal_name = str_to_title(journal_name))

all_crossref_journals[journal_name %like% "Comparative Politics"][,.(journal_name, pissn)]

all_crossref_journals$pissn

all_crossref_journals[journal_name %like% "American Political"]

journals[issn == "0003-0554"]

journals <- rbind(polisci_journals, ir_journals, fill = TRUE)

journals[issn == "0137-1592"]
