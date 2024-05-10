source("./code/libs.R")

# Get the data in and unique it ---------------------------------------------------------
pubs <- data.table(read_fst("./out/pubs.fst")) %>% distinct(doi, .keep_all = T)

# Add dataverse matches ---------------------------------------------------
# NOTE: There is a lot going on here.  is the key file really that has the data linking the dataverse doi pulled from the journal's dataverse account and the paper title in our database. But they have not been matched yet! So this script needs to match the TITLE of the paper with the title in pubs. It could probably be done quickly but anyway. 
# 
# THEN it needs to add on any papers (1) found directly from the dataverse title search (i.e. dataverse.fst) _and also_ (2) any titles from full text papers "dataverse_dois_from_papers.fst". 
# 

# These ones were matches by searching titles inside the dataverse
links_from_dataverse_title_matches <- data.table(read_fst("./log/dataverse.fst"))

# These are _all_ the dataverse DOIs from all of the journals in the dataset.
journal_datasets_on_dataverse <- data.table(read_fst("./out/journal_datasets_on_dataverse.fst"))[,dataset_url := persistentUrl]

# These ones were matches from pulling the journal's own dataverse repository
dataverse_title_matches <- fread("./out/dataset_links.csv")

# The above two go together: the first are all the datasets on the dataverse's of journals, and the second are the titles of them (on the Harvard dataverse)

# Therefore the first task is joining items 2 and 3 above. These are after all the same thing -- one just has titles with it. 
journal_datasets_on_dataverse[dataverse_title_matches, on = "dataset_url", title := i.title][]

# Now matching on pubs is a function taking two inputs -- the journal and then the title

matchTitleJournalWithPubs <- function(doi, f_title, journal){
  # f_title <- journal_datasets_on_dataverse$title[50]
  # journal <- journal_datasets_on_dataverse$journal_name[50]
  # Test stuff. 
  # f_title <- "Replication data for:  When Do Low Roll Rates Indicate Party Influence?   Evidence from Counterfactual Roll Rates Legislative Studies Quarterly"
  
  f_title <- str_trim(str_remove_all(f_title, 'Replication [Dd]ata for:?\\s+|Replication [Pp]ackage for:?\\s+|by.*?$|\\"'))
  print(f_title)
  
  matching_journal <- unique(pubs$container_title)[amatch(journal,unique(pubs$container_title), method = "jw", maxDist = 0.30)]
  journal_matchdist <- stringdist(journal, matching_journal, method = "jw")
  
  truncate_pubs <- pubs[container_title == matching_journal]
  
  matching_title <- truncate_pubs[amatch(f_title, truncate_pubs$title, method = "jw", maxDist = 0.30)]$title
  title_matchdist <- stringdist(f_title, matching_title, method = "jw")
  
  publication_doi <- truncate_pubs[title == matching_title]$doi
  
  data.table(publication_doi, dataverse_doi = doi, matching_journal, journal_matchdist, matching_title, dataverse_title = f_title, title_matchdist)
}

vect <- journal_datasets_on_dataverse
tictoc::tic()
dataverse_matches <- pmap_dfr(list(vect$persistentUrl, vect$title, vect$journal_name), matchTitleJournalWithPubs)
tictoc::toc()

# write_fst(matches, "./out/dataverse_matches.fst")

dataverse_matches <- data.table(read_fst("./out/dataverse_matches.fst"))

# The remaining dataverse dois are going to come from two other places: 
# 1. dataverse matches by searching titles directly on the dataverse, 2. dataverse dois obtained from searching inside publications directly. Let's look in turn. 




