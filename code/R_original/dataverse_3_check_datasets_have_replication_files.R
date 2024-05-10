library(glue)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/R/libs.R"))

# Search every dataset DOI in the Dataverse and pull the filenames ---------------------
known_dataverse_journal_matches <- fread(glue("{here}/output/known_dataverse_journal_matches.csv"))
known_dataverse_journal_matches <- known_dataverse_journal_matches %>% distinct(dv_doi)

get_file_names_from_dataset <- function(dataset_global_id){
  # dataset_global_id <- known_dataverse_journal_matches$dv_doi[1]
  # dataset_global_id <- "10.7910/DVN/WC8DPI"
  
  print(dataset_global_id)
  
  res1 <- try(curl_fetch_memory(glue('https://dataverse.harvard.edu/api/search?q=filePersistentId:"{dataset_global_id}"&per_page=100')))
  res2 <- fromJSON(read_lines(res1$content))
  res3 <- as.data.table(res2$data$items)
  
  if(nrow(res3) == 0){
    print(paste0(dataset_global_id, " empty"))
    res3 <- as.data.table(cbind(file_name = NA, dataset_global_id))
    return(res3)
  }
  
  return(res3)
}

# The result is already output as all_file_names_and_dataverse_identifiers.csv
all_files_in_datasets <- map_dfr(known_dataverse_journal_matches$dv_doi, ~get_file_names_from_dataset(.x))
# all_files_in_datasets %>% fwrite(glue("{here}/output/all_files_in_datasets.csv"))

all_files_in_datasets <- fread("./output/all_files_in_datasets.csv")

all_files_in_datasets[,uniqueN(dataset_persistent_id)]

# all_file_names1 <- all_file_names
# all_file_names2 <- all_file_names1 %>% rbindlist(., fill = T)
# all_file_names2[,dataset_persistent_id := str_remove(dataset_persistent_id, "doi:")]
# all_file_names2[dataset_persistent_id %in% known_dataverse_journal_matches$dv_doi]

# Find out how many datasets have only pdf or doc files -------------------

dv_entries <- fread("./output/all_file_names_and_dataverse_identifiers.csv")

dv_entries %>% distinct(dataset_persistent_id) %>% nrow

all_files_in_datasets[,file_ext := str_extract(name, "\\.[^.]*$")]

all_files_in_datasets[file_ext %like% "pdf|doc|docx", .(file_ext, dataset_persistent_id)]

all_extensions <- all_files_in_datasets %>%
  group_by(dataset_persistent_id) %>%
  distinct(file_ext) %>%
  ungroup()

only_doc_extensions <- all_extensions %>%
  group_by(dataset_persistent_id) %>%
  distinct(file_ext) %>%
  filter(file_ext %like% "pdf|doc|docx") %>%
  # mutate(all = paste0(file_ext, collapse = ";")) %>%
  ungroup()
# distinct(dataset_persistent_id, all)

no_doc <- all_extensions %>%
  filter(str_detect(file_ext, "pdf|doc|docx", negate = TRUE))

setDT(only_doc_extensions)

datasets_with_no_other_extensions <- only_doc_extensions[dataset_persistent_id %notin% no_doc$dataset_persistent_id]

datasets_with_no_other_extensions %>% fwrite("./output/datasets_with_only_doc_pdf_extensions.csv")

# Find out how many have no files at all ----------------------------------

potentially_missing <- known_dataverse_journal_matches[paste0("doi:",dv_doi) %notin% all_files_in_datasets$dataset_persistent_id]

# dv_entries[dataset_persistent_id %like% "doi:10.7910/DVN/HQXKJ9"]

# Pull the html files of datasets whose metadata could not be found by searching Dataverse --------

fetch_missing <- function(dataset_global_id){
  print(dataset_global_id)

  dataset_global_id_out <- str_replace_all(dataset_global_id, "/", "_")

  out_file <- glue("{here}/data/datasets_with_potentially_missing_files/{dataset_global_id_out}.html")

  if(file.exists(out_file)){
    print(paste0(out_file, " exists"))
    return()
  }

  res1 <- try(curl_fetch_memory(glue('https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:{dataset_global_id}')))

  write_lines(read_lines(res1$content), out_file)

  return()
}

map(potentially_missing$dv_doi, ~fetch_missing(.x))


definitely_missing_files <- system(glue("rg 'There are no files in this dataset.' -o {here}/data/datasets_with_potentially_missing_files/"), intern = T)

as.data.table(definitely_missing_files) %>% fwrite("./output/datasets_with_definitely_missing_files.csv")