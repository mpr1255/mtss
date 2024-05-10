# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    4.1_query-dataverse-with-titles.R                  :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 18:05:43 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/30 16:11:23 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# To recap: at this point we have pulled all the metadata from crossref (step 1);
# We've gathered the fulltext data with a combination of zotero getting pdfs + downloading html files (step 2 files.)
# We've classified them as papers that use statistical analysis or not (step 3 files.)
# So what's left now is figuring out which of the papers with statistical analysis have open data with them.
# There are six files that do this: 4.1—4.6.
#
# This first step involves querying APIs to determine the availability of replication materials.
# This only applies to the papers that have been classified as stat_bool = TRUE in 3.1_classify-fulltext-papers.
# This will only use one of the open science resources in the transparentr package.
# But your own domain may use other databases; these may be handled by transparentr too.
#
# Here is a general explanation of what is going on in files 4.1-4.5.
# 1. Query each of these papers in the Harvard Dataverse (transparentr function) (this file);
# 2. Pull dataverse links from each paper (4.2)
# 3. Check journal websites for data (4.3);
# 4. Check papers for precarious data (4.4).
# 5. Check DART-signatory journals specifically (4.5)
# The remainder will be said not to have open data.
# **************************************************************************** ##

devtools::load_all("/Volumes/t7/projects/transparentr")
library(transparentr)
source("./code/libs.R")
plan(multisession, workers = 8)
options(future.rng.onMisuse = "ignore")

# 1.  Filter target publications ------------------------------------------
pubs <- data.table(read_fst("./out/pubs.fst"))
pubs <- unique(pubs, by = c("doi", "issn", "title"))

classified_papers <- as.data.table(read_fst("./out/classified_papers.fst"))
classified_papers[file %notlike% ".txt", file := paste0(file, ".txt")]

stat_papers <- classified_papers[stat_bool == 1, .(file)]
target_publications <- pubs[stat_papers, on = c("txt_file" = "file")]
target_publications[, got_fulltext := ifelse(file.exists(paste0(
  "./data/txt/",
  txt_file
)), T, F)]
 # Make sure got all those text files..
 stopifnot(all(target_publications[, .N, got_fulltext]))
target_publications <- target_publications[!is.na(title)]

############ TESTING. YOU MAY IGNORE ############
# pubs[title %like% "Which Identity Frames Boost Support for and Mobilization in the"]$doi
# apsr_names_to_test <- c("Gone For Good: Deindustrialization, White Voter Backlash, and US Presidential Voting",
#  "Do Commodity Price Shocks Cause Armed Conflict? A Meta-Analysis of Natural Experiments",
# "A Dynamic Model of Speech for the Social Sciences",
# "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan",
# "Quantifying Political Relationships",
# "*?!?!?!?!jdkjdkjk>>>>???",
# "Designing Social Inquiry: Scientific Inference for Qualitative Research")
# target_publications <- pubs[stat_bool == T]
############ TESTING. YOU MAY IGNORE ############

# 2.  Query dataverse -----------------------------------------------------
# Optional: here I'm filtering out everything already checked.
# In this step, we hit the dataverse API with all of the titles in our target publication list.
# It examines the string matches and uses a reasonable cutoff for the results — but be sure to 
# inspect the matches.
# The files ./log/notfound.csv and ./log/dataverse.fst are created by
# the `search_dataverse` function in the `transparentr` package.

already_checked1 <- fread("./log/not_found.csv")
already_checked2 <- data.table(read_fst("./log/dataverse.fst"))
target_publications <- target_publications[doi %notchin% already_checked1$doi] %>%
  .[doi %notchin% already_checked2$publication_doi]
# This particular file was problematic for some reason...
target_publications <- target_publications[title %notlike% "RESIGNED BUT SATISFIED"]

dataverse_matches <- map2_dfr(target_publications$title, target_publications$doi, ~
  transparentr::search_dataverse(.x, .y, logdir = glue("{here}/log")))
log <- as.data.table(read_fst("./log/dataverse.fst"))
rejects <- fread("./log/not_found.csv")
unique(log, by = "publication_doi")

# 3. Pull entire journal history from dataverse and match -----------------
# For this you will need to have figured out which (if any) of your journals of interest have a Dataverse of their own.
# Then you can pull all the papers from them.
# Note that this is obviously only going to work for _some_ of the journals in the corpus.
# The vast majority of journals, it turns out, do not have their own dataverse, and thus this step does not
# pertain to them.

dataverse_journal_links <- fread("./out/dataverse_global_ids.csv")
dataverse_journal_links <- dataverse_journal_links[, .(name, identifier, match_candidate,
  diff2 = map2_dbl(name, match_candidate, ~ stringdist(.x, .y, method = "jw"))
)] %>%
  .[order(diff2)] %>%
  .[diff2 < 0.17 | match_candidate %like% "Journal Of Information Technology & Politics"]

# Already run; checks to see if the file exists. Obviously delete the file if running again.
if (!file.exists("./out/journal_datasets_on_dataverse.fst")) {
  getJournalDataverseContent <- function(dataverse_identifier, journal_name) {
    res <- rbindlist(dataverse_contents(dataverse_identifier,
      server = "dataverse.harvard.edu"
    ), fill = T)
    data.table(res, journal_name, dataverse_identifier)
  }

  possibly_getJournalDataverseContent <- possibly(getJournalDataverseContent, otherwise = NA)

  all_dataverse_files <- future_map2_dfr(
    dataverse_journal_links$identifier, dataverse_journal_links$name,
    ~ PossiblygetJournalDataverseContent(.x, .y)
  )
  # all_dataverse_files %>% write_fst("./out/journal_datasets_on_dataverse.fst")
} else {
  all_dataverse_files <- data.table(read_fst("./out/journal_datasets_on_dataverse.fst"))
}

# These have already been matched; it's written so that more matching will only be on new files
metadata_already_got <- fread("./out/dataset_links.csv")
metadata_already_tried <- fread("./out/dataset_error_links.csv")
metadata_to_get <- all_dataverse_files[persistentUrl %notchin% metadata_already_got$dataset_url & persistentUrl %notchin% metadata_already_tried$url & !is.na(persistentUrl)]

getDatasetMetadata <- function(dataset_url) {
  # dataset_url <- metadata_to_get$persistentUrl[1]
  print(dataset_url)
  if (is.na(dataset_url)) {
    fwrite(data.table(url = dataset_url), "./out/dataset_error_links.csv", append = T)
    return()
  }

        tryCatch(
          expr = {
            res <- dataset_metadata(dataset_url, server = "dataverse.harvard.edu")
          },
          error = function(e) {
            message(glue("error"))
            print(dataset_url)
            fwrite(data.table(url = dataset_url), "./out/dataset_error_links.csv", append = T)
            print(e)

          },
          warning = function(w) {
            message(glue("warning"))
            print(w)
          }
        )
  
  title <- res$fields$value[[1]]
  res2 <- data.table(dataset_url, title)
  fwrite(res2, "./out/dataset_links.csv", append = T)
  # return(res2)
}
possibly_getDatasetMetadata <- possibly(getDatasetMetadata, otherwise = NA)

walk(metadata_to_get$persistentUrl, ~ possibly_getDatasetMetadata(.x))

