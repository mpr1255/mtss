# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    4.2_pull-dataverse-links-from-papers.R             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 17:46:51 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/31 15:19:47 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# **************************************************************************** #
# EXPLANATION
# This goes through all of the classified papers in the corpus and pulls out the dois
# to Harvard Dataverse items.
# **************************************************************************** #

devtools::load_all("/Volumes/t7/projects/transparentr")
library(transparentr)
source("./code/libs.R")
library(rlang)
library(furrr)
plan(multisession, workers = 8)

pubs <- as.data.table(read_fst("./out/pubs.fst"))
pubs <- unique(pubs, by = "doi")
classified_papers <- as.data.table(read_fst("./out/classified_papers.fst"))
classified_papers[file %notlike% ".txt", file := paste0(file, ".txt")]
stat_papers <- classified_papers[stat_bool == 1, .(file)]
target_pubs <- pubs[stat_papers, on = c("txt_file" = "file")]
target_pubs <- unique(target_pubs, by = "doi")
target_pubs[, got_fulltext := ifelse(file.exists(paste0("./data/txt/", txt_file)), T, F)]
stopifnot(all(target_pubs[, .N, got_fulltext])) # Make sure got all those text files..

# Pull dataverse links from papers ------------------------------------
# First filter out the ones already done. May need to touch this file first if it doesn't exist.
dataverse_dois_from_papers <- as.data.table(read_fst("./out/dataverse_dois_from_papers.fst"))
target_pubs <- target_pubs[doi %notchin% dataverse_dois_from_papers$doi]
# So if target_puts is empty here, you're done with this file. There are no more dois to pull from target papers.

pullDataverseLink <- function(file, doi) {
  if (is.na(file)) {
    return(invisible(NULL))
  }

  file <- paste0(here, "/data/txt/", file)
  if (!file.exists(file)) {
    return(invisible(NULL))
  }
  if (!file.size(file) > 10) {
    return(invisible(NULL))
  }

  fulltext <- fread(file, header = FALSE, sep = NULL)$V1
  if (identical(fulltext, character(0))) fulltext <- read_lines(file)
  dataverse_doi <- str_extract_all(fulltext, "10\\.7910.{0,5}DVN.{0,3}/.*?(?=\\.|\\s+|\\))")
  if (identical(dataverse_doi[[1]], character(0))) {
    return(data.table(file, doi, dataverse_doi = NA))
  }
  dataverse_doi <- unique(dataverse_doi[[1]])
  if (any(lapply(dataverse_doi, nchar)[[1]] > 20)) {
    dataverse_doi <- str_extract_all(dataverse_doi, "10\\.7910.{0,5}DVN.{0,3}/.*?(?=\\.|[A-Za-z])")
  }

  dataverse_doi <- map_chr(dataverse_doi, ~ stri_replace_all_charclass(.x, "\\p{WHITE_SPACE}", ""))
  dataverse_doi <- unique(dataverse_doi[[1]])
  dataverse_doi <- paste(dataverse_doi, collapse = ";")

  return(data.table(file, doi, dataverse_doi))
}
target_pubs_to_check <- map2_dfr(target_pubs$txt_file, target_pubs$doi, ~ pullDataverseLink(.x, .y))
target_pubs_to_check <- rbind(res, dataverse_dois_from_papers)
# target_pubs_to_check %>% write_fst("./out/dataverse_dois_from_papers.fst")

# Need to check all this out...............
# TODO: Figure out what is going on here with this Check dataverse thing.
CheckDataverseLinksMatchTitles <- function(f_doi, dataverse_doi) {
  # f_doi <- "10.1057/s41269-020-00192-2"
  # dataverse_doi <- "doi:10.7910/DVN/ZIDRMH"
  # f_doi <- res$doi[3]
  # dataverse_doi <- res$dataverse_doi[3]
  print(glue("checking {f_doi} | dataverse {dataverse_doi} "))
  if (is.na(dataverse_doi)) {
    return(invisible(NULL))
  }
  dataverse_doi <- "10.7910/DVN/ZIDRM​H"
  dataverse_doi <- str_remove_all(dataverse_doi, '\\"|\\s+')
  title <- paste0("Replication Data for: ", pubs[doi == f_doi]$title)
  print(glue("checking {title} | doi {f_doi} | dataverse {dataverse_doi} "))
  url <- paste0('https://dataverse.harvard.edu/api/search?q="', dataverse_doi, '"&type=dataset')
  url <- str_replace_all(url, '\\"', "%22")

  res <- system(glue("curl https://dataverse.harvard.edu/api/search?q=%22{dataverse_doi}%22&type=dataset"), intern = T)
  res2 <- jsonlite::fromJSON(res)
  dataverse::get_dataset(dataverse_doi)
  dataverse::dataset_metadata(dataverse_doi)
  dataverse_doi
  # res <- try(curl::curl_fetch_memory(url))
  # res2 <- jsonlite::fromJSON(readr::read_lines(res$content))
  if (length(res2$data$items) == 0) {
    return(invisible(NULL))
  }
  simple_res <- subset(res2$data$items, select = c(
    name, type,
    url, global_id, description, published_at, publisher,
    citationHtml, identifier_of_dataverse, name_of_dataverse,
    citation, createdAt
  ))

  closest_match <- simple_res[stringdist::amatch(title, simple_res$name), ]
  match_dist <- stringdist::stringdist(title, closest_match$name,
    method = "jw"
  )
  if (match_dist > .28) {
    return(data.table(f_doi, dataverse_doi, closest_match, match_dist))
  }
  return(invisible(NULL))
}

bad_matches <- map2(target_pubs_to_check$doi[1:10], target_pubs_to_check$dataverse_doi[1:10], ~ CheckDataverseLinksMatchTitles(.x, .y))

options(datatable.prettyprint.char = 60L)
res <- map2_df(dataverse_matches2$txt_file[1:100], dataverse_matches2$doi[1:100], ~ PullDataverseLink(.x, .y))
