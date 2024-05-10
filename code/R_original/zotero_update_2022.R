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

print("getting list of text files we have")
got_dois <- as.data.table(list.files(glue("{here}/data/txt/")) %>% str_remove("\\.txt"))

all_files <- fread("./output/all_109k_pubs_cleanish.csv")

all_files[order(published_online)]