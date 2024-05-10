librarian::shelf(data.table, fst, glue, here, tidyverse, janitor, DBI, RSQLite)
here <- here()
pubs <- data.table(read_fst("./out/pubs.fst"))
polisci_journals <- fread(glue("{here}/data/jcr/JCR_Political_Science.csv"))
ir_journals <- fread(glue("{here}/data/jcr/JCR_International_Relations.csv"), header = TRUE)

target_journals <- rbind(polisci_journals, ir_journals, fill = TRUE) %>%
    clean_names() %>%
    mutate(journal_name = str_to_title(journal_name)) %>%
    arrange(desc(x2020_jif)) %>%
    filter(x2020_jif != "n/a") %>%
    filter(x2020_jif != "") %>%
    group_by(category) %>%
    ungroup() %>%
    slice_head(n = 10) %>%
    data.table()

preferred_journals <- target_journals$journal_name
preferred_subset <- pubs[container_title %in% preferred_journals & got_fulltext == TRUE]
sample_preferred <- preferred_subset[sample(.N, min(500, nrow(preferred_subset)))]
rest_subset <- pubs[!container_title %in% preferred_journals]
sample_rest <- rest_subset[sample(.N, 500)]
final_sample <- rbind(sample_preferred, sample_rest)
final_sample$full_text <- NA

con <- dbConnect(RSQLite::SQLite(), "./out/pubs_sample.sqlite")
dbWriteTable(con, "pubs", final_sample, overwrite = TRUE)

# A function that takes a dataframe row, reads the associated text file, and updates the SQLite table
update_sqlite_with_txt <- function(txt_file) {
    # browser()
    fulltext <- readLines(glue("./data/txt/{txt_file}"))
    print(nchar(fulltext))
    if (!file.exists(glue("./data/txt/{txt_file}"))) {
        print("no file!!")
        # browser()
    }
    if (grepl("Australian National University", tail(fulltext, 500))) {
        regex_pattern <- "Australian National University, on \\d{1,2} [A-Za-z]{3} \\d{4} at \\d{2}:\\d{2}:\\d{2}, subject to the Cambridge Core terms of use, available at https://www\\.cambridge\\.org/core/terms\\. https://doi\\.org/\\d{2}\\.\\d{4}/[A-Za-z]+/\\w+"
        fulltext <- gsub(regex_pattern, "", fulltext)
    }
    dbExecute(con, "UPDATE pubs SET full_text = ? WHERE txt_file = ?",
        params = list(fulltext, txt_file)
    )
}

# Apply the function to each row of the dataframe
possibly_update_sqlite_with_txt <- possibly(update_sqlite_with_txt, otherwise = NA)
walk(final_sample$txt_file, possibly_update_sqlite_with_txt)

# Don't forget to disconnect
dbDisconnect(con)