library(dataverse)
library(tidyverse)
library(data.table)
library(curl)
library(jsonlite)
library(glue)
library(furrr)
library(stringdist)
library(stringi)

####################################################################################
# 
# Explanation of this code: 
# We need to find every corresponding dataverse entry for each paper in our dataset. 
# There are two ways to tackle this, and this code does both:
# 1. Collecting all papers in each dataverse corresponding to each journal, and matching those 
# to the papers in our dataset.
# 2. Working from each paper in our dataset to query for its existence in the dataverse;
# 
# It is a bit neater to start with the first, so that's where it begins. 
# It still needs to perform the matching to the individual paper by string matching.
# We use Jaro-Winkler with the journal as blocking variable.
# 
# The remainder of papers are then queried in the dataverse directly using their title. 
# The string matching process is similar -- the best match for each search under a jw distance of 0.29. 
# There are no blocking variables going in this direction; we validated a sample of the matches and got 
# PLACEHOLDER
# 
####################################################################################

# title <- "On Political Methodology"
# doi <- "10.1093/pan/2.1.1"
# test_doi <- all_refs[journal == "Political Analysis"]$doi[1:50]
# test_title <- all_refs[journal == "Political Analysis"]$title[1:50]
# test_title <- "Fractional integration methods and short time series: evidence from a simulation study"
# dataverse_res <- future_map2_dfr(all_refs[journal == "Political Analysis"]$doi, all_refs[journal == "Political Analysis"]$title, ~get_results(.x, .y))
# dataverse_res[!is.na(name)]$searched_title


# Match journals to dataverse entries -------------------------------------
# We do this and write it out as dataverse_global_ids.csv. This contains each journal and its appearance in the dataverse. 
get_dataverse_ids <- function(journal_name){
  journal_name_for_search <- str_replace_all(journal_name, "\\s+", "+")
  res <- try(curl_fetch_memory(glue("https://dataverse.harvard.edu/api/search?q={journal_name_for_search}&type=dataverse")))
  res2 <- fromJSON(read_lines(res$content))
  res3 <- as.data.table(res2$data$items)
  res4 <- res3[amatch(journal_name, str_remove(res3$name, "Dataverse"), method = "jw", maxDist = 0.30)]
  diff <- stringdist(res4$name, journal_name, method = "jw")
  res4 <- cbind(res4, match_candidate = journal_name, diff)
  return(res4)
}
  
all_journal_names <- fread("./data/jcr_combined_list.csv")$journal_name

all_dataverse_global_ids <- future_map_dfr(all_journal_names, ~get_dataverse_ids(.x))

dataverse_global_ids <- all_dataverse_global_ids %>% 
  drop_na(identifier) %>% 
  distinct(identifier, .keep_all = T)

#### WRITE OUT FILE ###
# dataverse_global_ids %>%
#   distinct(identifier, .keep_all = TRUE) %>%
#   fwrite("./data/dataverse_global_ids.csv")


dataverse_global_ids <- fread("./data/dataverse_global_ids.csv")
dataverse_global_ids1 <- dataverse_global_ids %>% 
  arrange(diff) %>% 
  slice(1:19)

dataverse_global_ids2 <- dataverse_global_ids %>%
  filter(match_candidate == "Journal Of Information Technology & Politics")

dataverse_global_ids <- rbind(dataverse_global_ids1, dataverse_global_ids2) %>% 
  select(name, identifier, match_candidate, diff, published_at)

# Gather contents of matched journals -------------------------------
# This then gathers the contents of each dataverse based on the unique journal identifier found 
# and saved in dataverse_global_ids.

get_dataverse_contents <- function(dataverse_identifier){
  res1 <- dataverse_contents(dataverse_identifier, server = "dataverse.harvard.edu")
  res2 <- rbindlist(res1, fill = TRUE)
  res3 <- cbind(dataverse_identifier, res2)
  res3 <- res3[,-c("protocol", "publisher", "type")]
  return(res3)
}

all_datasets <- future_map_dfr(dataverse_global_ids$identifier, ~get_dataverse_contents(.x))

all_datasets$dataset_global_id <- paste0(all_datasets$authority, "/", all_datasets$identifier)


### WRITE OUT FILE ###
all_datasets %>% fwrite("./data/all_datasets_of_20_journals_on_dataverse.csv")

# Gather files from those datasets ---------------------------------------
get_file_names_from_dataset <- function(dataset_global_id, dataverse_identifier){
  # dataset_global_id <- all_datasets$dataset_global_id[500]
  # dataset_global_id <- "10.7910/DVN/SGXVVT"
  
  print(paste0("this id is ", dataset_global_id, " in ", dataverse_identifier))
  
  res1 <- try(curl_fetch_memory(glue('https://dataverse.harvard.edu/api/search?q=filePersistentId:"{dataset_global_id}"&per_page=100')))
  
  res2 <- fromJSON(read_lines(res1$content))
  res3 <- as.data.table(res2$data$items)
  
  
  if(nrow(res3) == 0){
    print(paste0(dataset_global_id, " empty"))
    res3 <- as.data.table(cbind(file_name = NA, dataset_global_id))
    return(res3)
  }

  res3 <- cbind(res3, dataverse_identifier)
  return(res3)
}

# dataset_global_id <- "10.7910/DVN/SGXVVT"

all_file_names <- future_map2_dfr(all_datasets$dataset_global_id, all_datasets$dataverse_identifier, ~get_file_names_from_dataset(.x, .y))


all_file_names[,uniqueN(dataset_persistent_id)]

# all_file_names %>% fwrite("./data/all_file_names_and_dataverse_identifiers.csv")

# all_file_names[,uniqueN(dataset_global_id)]

# dataverse_res <- future_map2_dfr(doi, title, ~get_results(.x, .y))
######################################################################
# This has completed the first step -- gathering all the data on the

get_dataverse_entries_from_titles <- function(doi, title){
  print(glue("querying '{title}' || doi {doi}"))
  title_for_search <- str_replace_all(title, "\\s+", "+")
  res <- try(curl_fetch_memory(glue("https://dataverse.harvard.edu/api/search?q={title_for_search}")))
  res2 <- fromJSON(read_lines(res$content))
  res3 <- as.data.table(res2$data$items)
  res3$name <- str_to_lower(res3$name)
  res3$name <- str_remove(res3$name, "replication data for:")
  res4 <- res3[amatch(str_to_lower(title), res3$name, method = "jw", maxDist = 0.29)]
  res4$doi <- doi
  res4$searched_title <- title
  return(res4)
}

all_refs <- fread("./data/all_dois_out.csv")
all_refs$title <- str_remove_all(all_refs$title, "\\*")
# all_refs_test <- sample_n(all_refs, 100)
all_journal_names <- unique(all_refs$journal)

all_refs[journal %like% "Review"] %>% distinct(journal)

dataverse_res <- as.data.table(future_map2_dfr(all_refs_test$doi, all_refs_test$title, ~get_results(.x, .y)))

dataverse_res[,.(searched_title, doi, name, global_id)]


all_dataverses <- unique(dataverse_res[!is.na(name)]$identifier_of_dataverse)