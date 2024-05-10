source("./code/R/libs.R")

########################################################################################################
# 
# This script picks up where dataverse_1_query_it left off.
#
# Recall that none of the Dataverse metadata fields contain the actual doi of the article. 
# 
# Thus the only way to link a given dataset in the Dataverse (or a journal's dataverse) to an actual publication is 
# string matching. 
# 
# Yet also recall that this is not simple -- because for instance, the Dataverse dataset *title* will contain the 
# author names, etc. and so matching on the title alone is unreliable. 
# 
# However! There is yet one other way to go about it: pull out any actual URLs to Dataverse datasets found in the full 
# text of the papers themselves. That is the first thing done below.
# 
# After that, we pick the low-hanging fruit in get_dataverse_entries_from_titles_incremental_matching.csv, and then 
# manually establish a cut-off for when they're just no longer matching. 
# 
# By combining these three steps -- 1) pulling out any actual URLs to Dataverse datasets found in the full 
# text of the papers, 2) pulling every dataset from each journal's dataverse in the Harvard Dataverse 
# and linking them to our corpus by string matching; and 3) searching the Dataverse for titles of all papers 
# in our corpus and establishing matches  -- we will have attempted three ways of making a link between 
# all of our papers and any potential Dataverse entry. We could not think of additional ways of attempting 
# to find matches.
# 
# The first halves of steps 2 and 3 were done in dataverse_1_query_it, so for those steps now we just need to 
# find the matches.
# 
# The first thing below is to pull each dataset DOI in the dataverse from each full text paper.
# 
########################################################################################################

here <- rprojroot::find_rstudio_root_file()

# 1. Pull dataset DOIs from the fulltext of every paper ---------------------

print("getting file list")
filename_w_path <- as.data.table(tibble(file_w_path = list.files(glue("{here}/data/txt/"), full.names = TRUE)))

print("getting file list again")
filename_no_path <- as.data.table(tibble(file = list.files(glue("{here}/data/txt/")) %>% str_remove("\\.txt")))

files <- cbind(filename_w_path, filename_no_path)

# This first function searches inside each pdf for links to datasets hosted on the Dataverse.

get_dv_links <- function(file_in){
  
  print(paste0("file in is ", file_in))
  file_w_path <- files[file == file_in]$file_w_path
  
  if(!file.exists(file_w_path)){
    return()
  }
  fulltext <- fread(file_w_path, header = FALSE, sep = NULL)

  if(!fulltext %like% "[dD]ataverse"){
    return()
  }
  
    dataverse_mention <- stri_extract_all_regex(fulltext, "10\\.7910\\/DVN\\/.*?[A-Z0-9]{6}")

    # If that one doesn't catch, try a better one that will span lines.
    if (length(dataverse_mention[[1]]) == 0) {
      dataverse_mention <- stri_extract_all_regex(fulltext, "(?s)10\\.7910\\/DVN\\/.*?[A-Z0-9]{6}")
    }

  clean_url <- map(dataverse_mention, ~str_remove_all(.x, "\\s+"))
  # clean_url <- str_remove_all(clean_url, "\\.[^.]*$") # just for reference in case needed. Attempts
  
  urls <- map_df(clean_url, ~tibble(url = .x))
  
  return(cbind(file_in, urls))

}

res <- map_df(files$file, ~get_dv_links(.x))

# Now output this file.
# res2 %>% drop_na() %>% distinct(url, .keep_all = TRUE) %>% fwrite("./output/extracted_dv_links_and_dois.csv")
# length(unique(res$file_in))


# 2. Now Match the datasets discovered in dataverse_1_query ----------------------

# Retrieve all the potential matches, clean them up, try to match them again.
matches1 <- read_csv("./output/get_dataverse_entries_from_titles_incremental_matching.csv")
matches <- as.data.table(matches1)

matches[,clean_result := str_remove_all(str_to_lower(result_title), "replication data for|replication files for")]
matches[,clean_result := str_remove_all(clean_result, '"|\\:|“')]
matches[,clean_result := str_to_lower(clean_result)]
matches[,clean_result := str_trim(clean_result)]

matches[,clean_search := str_remove_all(search_title, '"|“|\\:')]
matches[,clean_search := str_to_lower(clean_search)]
matches[,diff2 := stringdist(clean_result, clean_search, method = "jw")]

# Pull off first set of matches based on dataset dois pulled from full text of journal articles.
# There are 1045 of these. (As of 20210830_2112 when the corpus was ~79k. It should get bigger when run again with the full corpus.)
known_matches <- fread("./output/extracted_dv_links_and_dois.csv")
known_matches[url == "10.7910/DVN/SJ5HPâ€‹1"]$url <- "10.7910/DVN/SJ5HP1"
known_matches[,file_in := str_replace_all(file_in, "_", "/")]
known_matches1 <- matches[!is.na(result_doi)][result_doi %in% paste0("doi:", known_matches$url)]
known_matches1 <- map_df(known_matches1, ~as.character(.x))


# 3. Pull off second set of matches based on Dataverse dataset doi --------

# The file all_datasets_of_20_journals_on_dataverse.csv contains all the datasets directly from the dataverse. These need to be matched with dataverse links pulled from the full-text of the papers.

journals_dv_names <- fread("./output/all_datasets_of_20_journals_on_dataverse.csv")
known_matches2 <- matches[result_doi %in% paste0("doi:", journals_dv_names$dataset_global_id)]
# known_matches2 <- as_tibble(known_matches2)

# Combine them
known_matches3 <- rbind(known_matches1, known_matches2)

# Note: The result of those two operations is that known_matches3 -- 3,643 rows -- are very definite matches based on either the dataset id being in the full text of the published paper, or the dataset that came from the dataverse of the journal being matched directly to one of the searched titles. The remainder will be those leftover in matches that are not in known_matches3. 

leftover <- matches[result_doi %notin% known_matches3$result_doi]

# Facts and logic (and basic visual inspection) tells us that matches with a diff2 of 0 or 0.1 are definite matches. We'll make those known_matches4.

known_matches4 <- leftover[diff2 < .1]
known_matches5 <- rbind(known_matches4, known_matches3)
# known_matches5 <- as_tibble(known_matches5)

all_known_matches <- as.data.table(known_matches5)[,.(result_doi = str_remove(result_doi, "doi:"), search_doi)]
names(all_known_matches) <- c("dv_doi", "paper_doi")
names(known_matches) <- c("paper_doi", "dv_doi")

unique_papers <- rbind(all_known_matches, known_matches) %>% distinct(paper_doi, .keep_all = TRUE)
unique_dv <- rbind(all_known_matches, known_matches) %>% distinct(dv_doi, .keep_all = TRUE)

# After manually inspecting the gap between these, it seems the more reliable version is unique_dv. For example, look at 10.7910/DVN/23025 and note that it also appears (mistakenly) as "10.7910/DVN/23025UNF". These are just problems with string-matching when pulling data from the text files, and they are unavoidable. 
# 
# The upshot is that we'll go with this df as the conclusion of all the clear matches that were either an almost-exact string match when searching the title, or where the dataverse DOI could easily be extracted from the full text of the paper. 
 
all_known_matches <- unique_dv

# all_known_matches %>% fwrite("./output/known_dataverse_journal_matches.csv")

# But what is the cut-off? We need to visually inspect them and see when they stop matching. 

# leftover[diff2 > .1 & diff2 < .24] %>% fwrite("./output/potential_dataverse_journal_matches.csv")

# This process demonstrates that it is actually quite difficult to fix on a definite match for any in this category. So we're going to put all those in a maybe_matches and leave it at that. 

# SECTION TWO: FROM DATAVERSE ---------------------------------------------

# Now we are going to do this process in reverse: gather all of the known entries from the dataverses, and match them to the crossref data. 
# First we're going to remove the matches already secured in all_known_matches.

all_known_matches <-  fread("./output/known_dataverse_journal_matches.csv")
most_papers <- fread("./output/all_papers_from_most_journals_without_regard_to_year.csv")

dv_entries <- fread("./output/all_file_names_and_dataverse_identifiers.csv")
dv_entries <- dv_entries %>% distinct(dataset_name, .keep_all = TRUE)
dv_entries[,`:=`(dataset_name = str_to_lower(dataset_name))]
dv_entries[,`:=`(dataset_name = str_remove_all(dataset_name, '"|\\:|“|replication data for|replication files for'))]

dv_entries[,dataset_persistent_id := str_remove(dataset_persistent_id, "doi:")]

# The ones that could not match on the DV side were ~780. 
dv_entries_to_match <- dv_entries[dataset_persistent_id %notin% all_known_matches$dv_doi]

dv_entries_to_match[,published_at := ymd(str_extract(published_at, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))]
dv_entries_to_match <- dv_entries_to_match[published_at > "2010-01-01"]

# Get dataverse names so they can be matched with the identifiers, so make a blocking variable for matching them with the references in crossref..... 

all_papers <- fread("./output/all_papers_clean.csv")

journal_dataverse_identifiers <- fread("./output/journal_dataverse_identifiers.csv")

dv_entries_to_match <- dv_entries_to_match[journal_dataverse_identifiers, on = c("dataverse_identifier" = "identifier")]

match_dataverse_crossref <- function(dataset_doi){
  # dataset_doi <- "10.7910/DVN/LGBXWX"
  print(dataset_doi)
  
  title_search <- dv_entries_to_match[dataset_persistent_id == dataset_doi]$dataset_name
  title_search <- str_remove_all(title_search, "replication package for|replication data for")
  title_search <- str_trim(title_search, "both")
  
  journal_search <- dv_entries_to_match[dataset_persistent_id %like% dataset_doi]$i.name
  journal_match <- all_papers[amatch(journal_search, all_papers$journal_name, method = "jw", maxDist = 0.30)]$journal_name
  
  journal_subset_to_search <- all_papers[journal_name %like% journal_match]
  journal_subset_to_search$title <- str_to_lower(journal_subset_to_search$title)
  title_match <- journal_subset_to_search[amatch(title_search, journal_subset_to_search$title, method = "jw", maxDist = .30)]$title
  
  if(is.na(title_match)){
    return()
  }
  
  diff <- stringdist(title_search, title_match, method = "jw")
  
  res <- cbind(as.data.table(tibble(dataset_doi, journal_search, journal_match, title_search, title_match, diff)))
  
  return(res)
  
}

test <- map_df(dv_entries_to_match$dataset_persistent_id[1:100], ~match_dataverse_crossref(.x))
test %>% select(title_search, title_match, diff) %>% arrange(diff)

# based on this the cutoff seems to be 0.220. Let's run them all. 
dv_entries_to_match <- dv_entries_to_match[!is.na(dataset_persistent_id)]
res <- map_df(dv_entries_to_match$dataset_persistent_id, ~try(match_dataverse_crossref(.x)))

other_matches <- res %>% filter(diff < .21)

dv_entries_to_match[dataset_persistent_id %notin% other_matches$dataset_doi] %>% fwrite("./output/orphan_dataverse_entries_not_matched_to_papers.csv")

options(datatable.prettyprint.char=60L)

# This demonstrates that there are 432 papers that were not found in the big match operation. Yet, they are indeed published in the journal. Upon inspection, it turns out that the crossref data did not include the PUBLISHED YEAR for these papers. So I had to recreate these and now have them all without regard to date in the variable "most_papers". So now I'm going to run the same matching function again -- journal as blocking var, etc. -- on that full 448k obs. dataset and see if we can't match these remaining ones. 

dv_orphans <- fread("./output/orphan_dataverse_entries_not_matched_to_papers.csv")

match_orphans <- function(dataset_doi){
  # dataset_doi <- "10.7910/DVN/NQT3UM"
  print(dataset_doi)
  
  title_search <- dv_orphans[dataset_persistent_id == dataset_doi]$dataset_name
  title_search <- str_remove_all(title_search, "replication package for|replication data for")
  title_search <- str_trim(title_search, "both")
  
  journal_search <- dv_orphans[dataset_persistent_id %like% dataset_doi]$i.name
  journal_match <- most_papers[amatch(journal_search, most_papers$container_title, method = "jw", maxDist = 0.30)]$container_title
  
  journal_subset_to_search <- most_papers[container_title %like% journal_match]
  journal_subset_to_search$title <- str_to_lower(journal_subset_to_search$title)
  title_match <- journal_subset_to_search[amatch(title_search, journal_subset_to_search$title, method = "jw", maxDist = .25)]$title
  
  if(is.na(title_match)){
    return()
  }
  
  journal_match_doi <- journal_subset_to_search[title %like% title_match]$doi
  
  
  diff <- stringdist(title_search, title_match, method = "jw")
  
  res <- cbind(as.data.table(tibble(dataset_doi, journal_search, journal_match, title_search, title_match, journal_match_doi, diff)))
  
  return(res)
  
}


orphan_matches <- map_df(dv_orphans$dataset_persistent_id, ~match_orphans(.x))
orphan_matches[,.(title_search, title_match, diff)] %>% View

orphan_matches_good <- orphan_matches[diff < 0.2294,.(dataset_doi, journal_match_doi)]

# There are a number of dupes in there, but it turns out they're often comments in response to the original paper. 
# There are only 5, so I'm just going to leave it. 
# orphan_matches_good[duplicated(orphan_matches_good$dataset_doi, )]

names(orphan_matches_good) <- c("dv_doi", "paper_doi")

# rbind(all_known_matches, orphan_matches_good) %>% distinct(paper_doi)

all_known_matches1 <- rbind(all_known_matches, orphan_matches_good)

all_known_matches1 %>% distinct(dv_doi, paper_doi) %>% fwrite("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")

# However, when simply comparing this figure -- with the orphans -- it turns out that they did not seem to add anything that wasn't already known, because the file known_dataverse_journal_matches.csv contains more unique dataverse identifiers.

# Disregard ---------------------------------------------------------------

# all_known_matches <-  fread("./output/known_dataverse_journal_matches.csv")

# unique(c(journals_dv_names$dataset_global_id, all_known_matches$dv_doi, dv_orphans$dataset_persistent_id))
# 
# known_matches <- all_known_matches %>% rbind(., orphan_matches_good) %>% distinct(dv_doi, .keep_all = T) 

# options(datatable.prettyprint.char=100L)
  
  
# all_papers <- read_rds("./data/all_pubs_post_2010.Rds")
# all_included <- fread("./output/zotero_lib_20210823_1717.csv") %>% clean_names()
# all_papers[doi %in% all_included$doi][,.(journal_name = container_title, published_print, published_online, doi, publisher, reference_count, is_referenced_by_count, subject, title, url)] %>% fwrite("./output/all_papers_clean.csv")



# all_papers[,`:=`(title = str_to_lower(title))]
# all_papers[,`:=`(title = str_remove_all(title, '"|\\:|“|replication data for|replication files for'))]
# 
# all_papers[title %like% "multilingual sentiment analysis"]$title
# 
# dv_entries[amatch(all_papers[title %like% "multilingual sentiment analysis"]$title, dv_entries$dataset_name, method = "jw", maxDist = 0.30)]
# 
# 
# 
# dataverse_search("jitp", server = 'dataverse.harvard.edu')
# 
# dv_entries$dataset_name[1]
