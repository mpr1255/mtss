source("./code/R/libs.R")

####################################################################################
# 
# Explanation:
# This file and dataverse_2_link_to_papers query the dataverse and then link the results to the papers -- and vice versa. # Note that "Dataverse" (capital) refers to the Harvard Dataverse, whereas "dataverse" is simply a regular noun,
#  typically referring to the dataverse belonging to each journal. I.e. the Harvard Dataverse hosts dataverses run 
#  by each journal. 
# 
# The problem: We need to find every dataverse entry corresponding to each paper in our dataset. 
# There are two ways to tackle this, and these two do each:
# 1. Collect all papers in each journal's dataverse, and match those to the papers in our dataset.
# 2. Use the Dataverse's search function to search for each title in the dataset.
# 
# It is a bit neater to start with the first, so that's where it begins. 
# After that, the script then queries all the titles in the Dataverse.
# 
# Also, just for information, the structure here is Harvard Dataverse > individual journal dataverse > hosted dataset (each corresponds to a paper) > files inside each dataset
# 
####################################################################################

# SECTION ONE: Collect papers in journal dataverses -------------------------------------
# 1.  Match journals to Dataverse entries 
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

all_journal_names <- fread("./output/jcr_combined_list.csv")$journal_name

all_dataverse_global_ids <- future_map_dfr(all_journal_names, ~get_dataverse_ids(.x))

dataverse_global_ids <- all_dataverse_global_ids %>% 
  drop_na(identifier) %>% 
  distinct(identifier, .keep_all = T)

#### WRITE OUT FILE ###
# dataverse_global_ids %>%
#   distinct(identifier, .keep_all = TRUE) %>%
#   fwrite("./output/dataverse_global_ids.csv")
  
# So, that file is the result of searching for the title of each journal in the Dataverse. 
# Next we have to match them to our journals and filter out the bad matches. Then we dump the contents of each journal's dataverse.

dataverse_global_ids <- fread("./output/dataverse_global_ids.csv")
dataverse_global_ids1 <- dataverse_global_ids %>% 
  arrange(diff) %>% 
  slice(1:19)

dataverse_global_ids2 <- dataverse_global_ids %>%
  filter(match_candidate == "Journal Of Information Technology & Politics")

dataverse_global_ids <- rbind(dataverse_global_ids1, dataverse_global_ids2) %>% 
  select(name, identifier, match_candidate, diff, published_at)


# Write this result out. Why slice(1:19 and then add the other? This is all by fuzzy string matching, 
# so we simply had to inspect it and figure out where the cutoff was. Most of them matched as expected.)

# dataverse_global_ids[,.(name, identifier, published_at)] %>% fwrite("./output/journal_dataverse_identifiers.csv")

# 2. Gather contents of matched journals 
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

# Write out. Read back in for analysis. 
# all_datasets %>% fwrite("./output/all_datasets_of_20_journals_on_dataverse.csv")

all_datasets <- fread("./output/all_datasets_of_20_journals_on_dataverse.csv")

get_dataset_info <- function(dataset_global_id){
  print(dataset_global_id)

  res1 <- try(dataset_metadata(dataset_global_id, server = "dataverse.harvard.edu"))
  if(res1[1] %like% "Error in dataset"){
    print(paste0(dataset_global_id, " throws error"))
    return(as.data.table(cbind(title = NA, dataset_global_id)))
  }
  all_fields <- as.data.table(res1$fields)
  title <- as.character(all_fields[1]$value)
  return(as.data.table(cbind(title = title, dataset_global_id)))
}

dataset_titles_ids <- map_df(all_datasets$dataset_global_id, ~try(get_dataset_info(.x)))

# Write it out. These are all the datasets in each journal's dataverse. There are 3570 of them. 
# dataset_titles_ids %>% fwrite("./output/dataset_titles_ids.csv")

# 3. Gather files from those datasets 

get_file_names_from_dataset <- function(dataset_global_id, dataverse_identifier){
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

all_file_names <- future_map2_dfr(all_datasets$dataset_global_id, 
                                  all_datasets$dataverse_identifier, 
                                  ~get_file_names_from_dataset(.x, .y))

# all_file_names %>% fwrite("./output/all_file_names_and_dataverse_identifiers.csv")
# all_file_names[,uniqueN(dataset_global_id)]
# dataverse_res <- future_map2_dfr(doi, title, ~get_results(.x, .y))
# 
############################################################################################################
#
# This has completed the first step -- gathering all the data from the journals' dataverses. There were 
# 
# The next step begins from the other end -- searching for the names of each individual publication 
# and assessing whether they get a hit on the dataverse.
# 
############################################################################################################


# SECTION TWO: Search Dataverse for paper titles --------------------------

# This function queries each title in the Dataverse from the full list of 109k papers in the entire corpus.
# It tries to match them using jaro-winkler and discards anything that's an obviously bad match.
# Close-ish matches are outputted to get_dataverse_entries_from_titles_incremental_matching.csv, 
# while all the others are logged at get_dataverse_entries_from_titles_incremental_NOT_matching.csv. 
# The function always checks if a given paper (by DOI) has already been searched (because it logs each search 
# in either of those files), so it's robust to stopping and starting. 
# The next script -- dataverse_2_link_to_papers.R -- completes the job and actually does all the linking 
# to papers in our corpus. 

get_dataverse_entries_from_titles <- function(search_doi){
  
  outfiles <- fread("./output/get_dataverse_entries_from_titles_incremental_NOT_matching.csv")
  if(search_doi %in% outfiles$search_doi){
    return()
  }
  outfiles <- fread("./output/get_dataverse_entries_from_titles_incremental_matching.csv")
  if(search_doi %in% outfiles$search_doi){
    print("already matched")
    return()
  }

  title <- all_refs[doi == search_doi]$title
  print(glue("querying '{title}' || doi {search_doi}"))
  
  title_for_search <- str_replace_all(title, "\\s+", "+")
  title_for_search <- str_replace_all(title_for_search, "�", "+")
  res <- try(curl_fetch_memory(glue('https://dataverse.harvard.edu/api/search?q={title_for_search}')))
  res2 <- fromJSON(read_lines(res$content))
  res3 <- as.data.table(res2$data$items)

  if(nrow(res3) == 0){
    print(paste0(search_doi, " with ", title, " empty"))
    res3 <- as.data.table(cbind(search_doi = search_doi, title = title, match = NA))
    return(res3)
  }
  
  res3$matched_name <- str_to_lower(res3$name)
  res3$matched_name <- str_trim(str_remove(res3$matched_name, "replication data for:"))
  res4 <- res3[amatch(str_to_lower(title), res3$matched_name, method = "jw", maxDist = 0.25)]
  diff <- stringdist(str_to_lower(title), res4$matched_name, method = "jw")
  
  if(identical(res4$name, NA) | identical(res4$name, NULL)){
    res5 <- data.table()
    # res5[1:26] <- NA
    res5 <- cbind(res5, search_doi = search_doi, search_title = title)
    fwrite(res5, "./output/get_dataverse_entries_from_titles_incremental_NOT_matching.csv", append = TRUE)
    return()
  }
  
  if(is.na(res4$name)){
    res5 <- data.table()
    # res5[1:26] <- NA
    res5 <- cbind(res5, search_doi = search_doi, search_title = title)
    fwrite(res5, "./output/get_dataverse_entries_from_titles_incremental_NOT_matching.csv", append = TRUE)
    return()
  }
  
  
  res4 <- map_dfr(res4, ~as.character(.x))
  res4 <- as.data.table(cbind(result_title = res4$name, result_doi = res4$global_id, result_authors = as.character(res4$authors), result_dataverse = res4$name_of_dataverse, search_title = title, search_doi = search_doi, diff = diff))
  
  fwrite(res4, "./output/get_dataverse_entries_from_titles_incremental_matching.csv", append = TRUE)
  return(res4)
}

all_refs <- fread("./output/zotero_lib_20210823_1717.csv") %>% clean_names()
all_refs$title <- str_remove_all(all_refs$title, "\\*")
all_refs$title <- str_remove_all(all_refs$title, "\\{|\\}")
all_refs$title <- str_replace_all(all_refs$title, "�", " ")

# Run it. 
map_dfr(all_refs$doi, ~try(get_dataverse_entries_from_titles(.x)))