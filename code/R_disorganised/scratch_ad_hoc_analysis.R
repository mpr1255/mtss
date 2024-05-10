library(tidyverse)
library(data.table)
library(readtext)
library(quanteda)
library(stringdist)
library(bib2df)
library(janitor)
library(glue)
library(tictoc)
library(stringi)
library(furrr)
library(future)

here <- rprojroot::find_rstudio_root_file()


files <- fread("./data/files_to_code_20210824.csv")



test <- read_rds("./data/crossref_dumps/American Political Science Review.Rds")
setDT(test)

test[,.(sample(title))]

test$abstract



