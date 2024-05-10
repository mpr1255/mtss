librarian::shelf(data.table, fst, glue, here, tidyverse, janitor, DBI, RSQLite)

# Basic check on how many rows actually have fulltext
con <- dbConnect(RSQLite::SQLite(), "./out/pubs_sample.sqlite")
count_query <- "SELECT COUNT(*) FROM pubs WHERE full_text IS NOT NULL AND full_text <> ''"
non_empty_count <- dbGetQuery(con, count_query)[[1]]
print(paste("Number of rows with non-empty 'full_text':", non_empty_count))
dbDisconnect(con)


# Save it out as csv file
con <- dbConnect(RSQLite::SQLite(), "./out/pubs_sample.sqlite")
pubs_sample_df <- data.table(dbReadTable(con, "pubs"))
dbDisconnect(con)
fwrite(pubs_sample_df, "./out/pubs_sample.csv", row.names = FALSE)