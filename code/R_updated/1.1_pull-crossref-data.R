# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    1.1_pull-crossref-data.R                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 16:35:32 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/29 20:02:41 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# Here we first start pulling down data from the crossref API. This is the basic skeleton of metadata
# needed for all the rest of the analysis.
# Note well: If you're going to hit the crossref API you'll need to add crossref_email="your email"
# to file.edit("~/.Renviron"). Then hit source("~/.Renviron"). Then try Sys.getenv("crossref_email")
# to confirm it worked. Also note that the API can be finnicky.
# Get your list of journals however it is convenient. What we're ultimately using here are the unique
# ISSN identifiers per journal. You can do all this with _only_ those ISSNs.
# These lists were obtained from Journal Citation ports by Clarivate https://jcr.clarivate.com/
# as explained in the paper.
# **************************************************************************** #

librarian::shelf(
  rcrossref,
  DBI,
  RSQLite,
  futile.logger,
  here,
  glue,
  purrr,
  furrr,
  data.table,
  tidyverse,
  curl,
  janitor,
  jsonlite,
  future,
  quiet = TRUE
)
# here <- "/Volumes/t7/projects/mtss"
here <- here()
# plan(multisession, workers = parallel::detectCores())
# plan(multisession, workers = 8)
# set.seed(42)

# source(glue("{here}/code/libs.R"))
# Set the path for the log file
log_file <- paste(here, "/logs/crossref.log", sep = "")
flog.appender(appender.file(log_file))
flog.layout(layout.simple)
flog.threshold(INFO)
print("libraries loaded")

# Read in journals in various fields
polisci_journals <- fread(paste(here, "/data/jcr/JCR_Political_Science.csv", sep = ""))
ir_journals <- fread(paste(here, "/data/jcr/JCR_International_Relations.csv", sep = ""), header = TRUE)

target_journals <- rbind(polisci_journals, ir_journals, fill = TRUE) %>%
  clean_names() %>%
  mutate(journal_name = str_to_title(journal_name)) %>%
  arrange(desc(x2020_jif)) %>%
  filter(x2020_jif != "n/a") %>%
  filter(x2020_jif != "") %>%
  group_by(category) %>%
  slice_head(n = 100) %>%
  ungroup() %>%
  filter(issn != "N/A") %>%
  data.table()

flog.info(glue("Number of journals: {nrow(target_journals)}"))
# if(!file.exists("./data/target_journal_names_issn.fst")){
#   unique(target_journals, by = c("journal_name", "issn"))[, .(journal_name, issn)] %>%
#   write_fst("./data/target_journal_names_issn.fst")
# }


pull_journal_data <- function(f_issn, from_date, to_date) {
  # browser()
  # f_issn <- target_journals$issn[3]
  # f_issn  <- "0020-5850"
  #  TESTING ABOVE
  outfile  <- paste0(here, "/data/crossref_dumps_1000-01-01--2023-12-11/", f_issn, ".rds") 
  if(file.exists(outfile)){
    file.info(glue("{outfile} exists!"))
    return()
  }
  file.create(outfile)
  msg <- paste("Processing", f_issn, "|", target_journals[issn == f_issn]$journal_name)[1]
  flog.info(msg)
  print(msg)

  # We have to get the total number of dois for the journal and plug into the function
  total_dois <- rcrossref::cr_journals(f_issn, works = FALSE) %>%
    pluck("data") %>%
    pluck("total_dois")

  flog.info(paste("Total dois:", total_dois))

  tryCatch(
    expr = {
      res <- data.table(V1 = cr_journals(f_issn,
        filter = c("from-print-pub-date" = from_date, "until-print-pub-date" = to_date), # See [1]
        cursor = "*",
        cursor_max = total_dois,
        works = TRUE,
        .progress = TRUE,
        # verbose = TRUE,
      ))
      res1 <- as.data.table(res[2] %>% unnest(V1) %>% clean_names())
      flog.info(paste("Downloaded", f_issn, "with", nrow(res1), "rows. Dates: ", from_date, "until", to_date))
      write_rds(res1, outfile)
    },
    error = function(e) {
          message(paste("error"))
          print(e)
          flog.info(paste(e))
        },
        warning = function(w) {
          message(paste("warning"))
          print(w)
        }
      )


  # # Find all the cols that are data frames
  # data_frame_cols <- sapply(res1, function(column) {
  #   if (is.list(column)) {
  #     any(sapply(column, is.data.frame))
  #   } else {
  #     FALSE
  #   }
  # })
  # data_frame_col_names <- names(res1)[data_frame_cols]

 
}  # End function

# Now, depending on how you've loaded in your issn (or just issn) data, you'll be passing your own vector of issn records through the function. It should behave the same. The result is a big folder of ISSNs with dates.
to_date <- format(Sys.Date(), "%Y-%m-%d")
from_date <- "1000-01-01"
walk(target_journals$issn, ~ pull_journal_data(.x, from_date, to_date))

# db <- dbConnect(SQLite(), paste(here, "/data/data.sqlite", sep = ""))

# # Get the most recent 'indexed' date for each journal ISSN in the database
# most_recent_dates <- dbGetQuery(db, "
#   SELECT issn, MAX(published_print) as latest_date
#   FROM crossref
#   GROUP BY issn
# ")
# setDT(most_recent_dates)
# dbDisconnect(db)

# convert_to_ymd <- function(date) {
#   if (nchar(date) == 4) {
#     # If the date is just a year, append -01-01 to make it January 1st of that year
#     return(paste0(date, "-01-01"))
#   } else {
#     # Otherwise, assume it's already in ymd format
#     return(date)
#   }
# }

# # Apply this function to your most_recent_dates data
# most_recent_dates$from_date <- sapply(most_recent_dates$latest_date, convert_to_ymd)
# print("Fixed most recent dates")


# Define a function to get the start date for each ISSN
# get_start_date <- function(f_issn) {
#   # browser()
#   # print(f_issn)
#   # f_issn <- "0162-2889"
#   # browser()
#   tryCatch(
#     expr = {
#       if (nrow(most_recent_dates[issn %like% f_issn]) > 0) {
#         res <- max(most_recent_dates[issn %like% f_issn]$latest_date)
#         return(res)
#       } else {
#         return("1000-01-01")
#       }
#     },
#     error = function(e) {
#       print(f_issn)
#       message(glue("error"))
#       print(e)
#     },
#     warning = function(w) {
#       message(glue("warning"))
#       print(w)
#     }
#   )
# }
# journal_to_scrape <- target_journals[, .(issn)]
# journal_to_scrape[, start_date := map_chr(issn, get_start_date)]


# More info about the crossref api routes.  ------------------------------------------------------
# Valid filters for this route are: # Valid filters for this route are: until-approved-date, has-assertion, from-print-pub-date, until-deposit-date, from-accepted-date, has-authenticated-orcid, from-created-date, relation.object, issn, ror-id, lte-award-amount, until-online-pub-date, group-title, full-text.application, until-created-date, license.version, from-deposit-date, has-abstract, from-awarded-date, has-event, from-approved-date, funder, assertion-group, from-online-pub-date, from-issued-date, directory, content-domain, license.url, reference-visibility, from-index-date, full-text.version, full-text.type, until-posted-date, has-orcid, has-archive, type, has-ror-id, is-update, until-event-start-date, update-type, from-pub-date, has-license, funder-doi-asserted-by, isbn, has-full-text, doi, orcid, has-content-domain, prefix, until-event-end-date, has-funder, award.funder, clinical-trial-number, member, has-domain-restriction, until-accepted-date, container-title, license.delay, from-posted-date, has-affiliation, from-update-date, has-award, until-print-pub-date, from-event-start-date, gte-award-amount, has-funder-doi, until-index-date, has-update, until-update-date, until-issued-date, until-pub-date, award.number, has-references, type-name, has-relation, alternative-id, archive, relation.type, updates, relation.object-type, category-name, until-awarded-date, has-clinical-trial-number, assertion, article-number, has-update-policy, from-event-end-date

# test_function <- function(x) {
#   print(glue::glue("Processing item {x} on PID: {Sys.getpid()}"))
#   Sys.sleep(1)  # Simulate some work
# }

# future_walk2(1:10, rep(NA, 10), ~ test_function(.x))
#