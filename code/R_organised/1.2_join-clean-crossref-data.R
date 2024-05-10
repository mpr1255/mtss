# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    1.2_join-clean-crossref-data.R                     :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 16:42:40 by matthew p r       #+#    #+#              #
#    Updated: 2022/09/09 17:16:33 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# This function will turn all the rds files into fst files,
# with author, URL, and paper metadata in separate files.
# The key issue is that each fst file needs to be created from a different crossref pull.
# So we need to parse out which crossref session the Rds files came from.
# This is parsed out from the file name.
# **************************************************************************** #

source("./code/libs.R")

rds1 <- list.files("./data/crossref_dumps/", full.names = T)
rds2 <- list.files("./data/crossref_dumps_2022/", full.names = T)
rds_files <- c(rds1, rds2)

parseRDStoFST <- function(file) {

  # Determine the file type.
  if (file %like% "end_2021") {
    date_string <- "end_2021"
  } else if (file %like% "2022-01-01_2022-06-29") {
    date_string <- "2022-01-01_2022-06-29"
  } else {
    date_string <- "legacy"
  }

  print(file)
  res <- as.data.table(read_rds(file) %>% clean_names())
  f_issn <- str_extract(res$issn[1], "[0-9]{4}-[0-9]{4}")

  # Save out pubs df
  cols <- names(res)[names(res) %chin% c(
    "container_title", "created", "deposited",
    "published_print", "published_online", "doi", "indexed", "issn", "issue",
    "isued", "member", "page", "prefix", "publisher", "score", "source",
    "reference_count", "references_count", "is_referenced_y_count", "subject",
    "title", "type", "url", "volume", "language", "abstract"
  )]
  pubs <- res[, .SD, .SDcols = cols]
  pubs_fileout <- glue("{here}/data/crossref_fst/{f_issn}_pubs_{date_string}.fst")
  if (!file.exists(pubs_fileout)) write_fst(pubs, pubs_fileout)
  print(glue("{pubs_fileout} out"))

  # Save out author df
  authors <- res[, .(author, doi)] %>%
    unnest(cols = author) %>%
    clean_names()
  authors_fileout <- glue("{here}/data/crossref_fst/{f_issn}_author_{date_string}.fst")
  if (!file.exists(authors_fileout)) write_fst(authors, authors_fileout)
  print(glue("{authors_fileout} out"))

  # Save out url/link df
  urls <- res[, .(link, doi)] %>%
    unnest(cols = link) %>%
    clean_names()
  urls_fileout <- glue("{here}/data/crossref_fst/{f_issn}_url_{date_string}.fst")
  if (!file.exists(urls_fileout)) write_fst(urls, urls_fileout)
  print(glue("{urls_fileout} out"))

  setDT(urls)
  return(invisible(NULL))
}
possiblyParse <- possibly(parseRDStoFST, otherwise = NA)

# Parse all the files.
map(rds_files, ~ possiblyParse(.x))


# Not run -----------------------------------------------------------------
# crossref_data <- list.files(glue("{here}/data/crossref_fst"), full.names = TRUE)
# crossref_pub_data <- crossref_data[crossref_data %notlike% "author|url"]
# crossref_author_data <- crossref_data[crossref_data %like% "author"]
# crossref_url_data <- crossref_data[crossref_data %like% "url"]
#
# pubs <- as.data.table(map_df(crossref_pub_data, ~read_fst(.x)))
# authors <- as.data.table(map_df(crossref_author_data, ~read_fst(.x)))
# urls <- as.data.table(map_df(crossref_url_data, ~read_fst(.x)))
