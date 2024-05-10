
# Create sqlite database
map_type_to_sqlite <- function(r_type) {
  type_mapping <- c(
    "logical" = "INTEGER",
    "integer" = "INTEGER",
    "numeric" = "REAL",
    "factor" = "TEXT",
    "character" = "TEXT",
    "Date" = "TEXT",
    "POSIXct" = "TEXT",
    "data.frame" = "TEXT", # Assuming JSON for nested data.frames
    "list" = "TEXT"
  ) # Assuming JSON for lists

  return(type_mapping[r_type])
}

# NOTE
# Take care with the dates that you pass to the function. You may end up with data that you've already got.
# There is a key manual step here: identify the *last* date you ran the function
# The saved files will be labeled "issn_from_date_to_date.fst" and saved in the projects ./data directory.
# Note also that the initial two extractions from crossref did not use this date format. The last pull was June 2022.


# write_to_database <- function(dt) {
#   col_types <- sapply(dt, class)
#   length(col_types)
#   sqlite_types <- sapply(col_types, map_type_to_sqlite)

#   # Construct CREATE TABLE statement
#   table_name <- "crossref"
#   create_stmt <- paste("CREATE TABLE IF NOT EXISTS", table_name, "(",
#     paste(names(dt), sqlite_types, sep = " ", collapse = ", "),
#     ", PRIMARY KEY(doi)",
#     ")",
#     sep = " "
#   )

#   flog.info(glue("Creating table {table_name} in database"))
#   db <- dbConnect(SQLite(), glue("{here}/data/data.sqlite"))
#   dbExecute(db, "PRAGMA journal_mode=WAL;")
#   dbExecute(db, create_stmt)
#   dois_dt <- dt$doi
#   dois_db <- dbGetQuery(db, "SELECT doi FROM crossref")
#   dt_filtered <- dt[!dois_dt %in% dois_db$doi, ]
#   dbWriteTable(db, "crossref", dt_filtered, append = TRUE, row.names = FALSE)
#   dbDisconnect(db)
#   flog.info(glue("Wrote {nrow(dt_filtered)} rows to database"))

#   # TESTING PURPOSES ONLY
#   # delete_stmt <- "DELETE FROM crossref WHERE doi = '10.2307/2538553'"
#   # dbExecute(db, delete_stmt)
# }

write_to_database <- function(dt) {
  # dt <- res1
  # Testing above only

  db <- dbConnect(SQLite(), paste(here, "/data/data.sqlite", sep = ""))
  dbExecute(db, "PRAGMA journal_mode=WAL;")

  # Get existing columns in the database
  existing_cols <- dbGetQuery(db, "PRAGMA table_info(crossref)")
  existing_col_names <- existing_cols$name

  # Get column types from the data table
  col_types <- sapply(dt, class)
  sqlite_types <- sapply(col_types, map_type_to_sqlite)

  # Identify new columns in dt that are not in the database
  new_cols <- setdiff(names(dt), existing_col_names)

  # Dynamically add new columns to the database
  for (col in new_cols) {
    alter_stmt <- paste("ALTER TABLE crossref ADD COLUMN", col, sqlite_types[col])
    dbExecute(db, alter_stmt)
    flog.info(paste("Added new column", col, "to table crossref"))
  }

  # Construct CREATE TABLE statement if table doesn't exist
  table_name <- "crossref"
  create_stmt <- paste("CREATE TABLE IF NOT EXISTS", table_name, "(",
    paste(names(dt), sqlite_types, sep = " ", collapse = ", "),
    ", PRIMARY KEY(doi)",
    ")",
    sep = " "
  )

  # Execute CREATE TABLE statement
  dbExecute(db, create_stmt)

  # Filter out rows with DOIs already in the database
  dois_dt <- dt$doi
  dois_db <- dbGetQuery(db, "SELECT doi FROM crossref")
  dt_filtered <- dt[!dois_dt %in% dois_db$doi, ]

  # Write the filtered data table to the database
  dbWriteTable(db, table_name, dt_filtered, append = TRUE, row.names = FALSE)
  dbDisconnect(db)
  flog.info(paste("Wrote", nrow(dt_filtered), "rows to database"))
}
  expected_cols <- c("link", "reference", "license", "author", "update_to")

  # Extract column names from res1 that are in expected_cols
  available_json_cols <- intersect(names(res1), expected_cols)

  # Ensure no unexpected columns are present
  stopifnot(all(available_json_cols %in% expected_cols))

  # Apply JSON transformation to available columns
  for (col in available_json_cols) {
    res1[, (col) := as.character(lapply(.SD[[col]], function(x) toJSON(x)), .SDcols = col)]
  }

  # Just confirm that those are the only cols that need to be jsonified

  # # Jsonify all columns. Loops and sd cols and everything would NOT working not sure why.
  # res1[, link := as.character(lapply(link, toJSON))]
  # res1[, reference := as.character(lapply(reference, toJSON))]
  # res1[, license := as.character(lapply(license, toJSON))]
  # res1[, author := as.character(lapply(author, toJSON))]
  # res1[, update_to := as.character(lapply(update_to, toJSON))]

  write_to_database(res1)