library(glue) 
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(bib2df)

`%notin%` <- Negate(`%in%`)
`%notlike%` <- Negate(`%like%`)

# Note! If running this outside the Dell Rstudio Windows 10, add your email to the environment file -- like "crossref_email=email@email.com" after pressing file.edit("~/.Renviron")

here <- here()

# Cleaning here. Once off so commented out so as not to overwrite the file.
# jcr_combined_list <- fread(glue("{here}/data/jcr_combined_list.csv")) %>% 
#   clean_names() %>% 
#   mutate(journal_name = str_to_title(journal_name))
# 
# jcr_combined_list %>% fwrite(glue("{here}/data/jcr_combined_list.csv"))

combined_journals <- fread(glue("{here}/data/jcr_combined_list.csv"))

combined_journals[issn == "N/A", issn := e_issn]

# combined_journals[,.(unique(issn))]

allpubs <- read_rds(glue("{here}/data/all_pubs_post_2010.Rds"))

allpubs[,first_issn := substr(issn, 1, 9)][,second_issn := substr(issn, 11, 20)]
          
dtout <- allpubs[first_issn %in% combined_journals$issn | second_issn %in% combined_journals$issn | first_issn %in% combined_journals$e_issn | second_issn %in% combined_journals$e_issn][,.(doi, journal = container_title, title, url)]

dtout[,CATEGORY := "ARTICLE"][,BIBTEXKEY := glue("out{rleidv(dtout)}")]

dtout$title <- dtout[,.(title = str_remove_all(title, "\\n"))][,.(title = str_replace_all(title, "\\s+", " "))][,.(title = str_to_sentence(title))][,.(title = str_replace_all(title, '“', '"'))][,.(title = str_replace_all(title, '”', '"'))][,.(title = str_replace_all(title, '‘', "'"))][,.(title = str_replace_all(title, '’', "'"))]

df2bib(dtout, glue("{here}/data/all_dois_out.bib"))