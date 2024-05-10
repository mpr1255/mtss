# /**
#* #/@ Author: Matthew Robertson
#* #/@ Create Time: 2024-05-08 21:22:55
#* #/@ Modified by: Matthew Robertson
#* #/@ Modified time: 2024-05-08 21:29:46
#* #/@ Description: This is justify the choice of the field we used for publication date
# And to analyse our use of crossref metadata, as well as exploring missing data.
# /**

# We will put this folder into Zenodo and so anyone can recreate this by just downloading it and pointing this script at it.
DATA_FOLDER <- "/Volumes/t7/data/mtss"
librarian::shelf(data.table, readr, tidyverse, janitor, glue)
dt <- data.table(read_rds(glue("{DATA_FOLDER}/crossref_dumps/American Political Science Review.Rds")))
nrow(dt)
nrow(dt[published.print != ""])
nrow(dt[published.online != ""])

# So, the published.print field has ~2k more than than published.online field. And the rest are simply unreliable:
#  for instance created appears to refer to created in the crossref db or similar, since papers published in the 1990s are getting 2006 creation dates.
#  Deposited is another artefact. So the only two dates about the actual publication are online and print.
# But the online ones can evidently be many years after the fact.
# So it's only publishedprint that has an actually reliable date of when the thing was published.

dt <- data.table(read_rds("{DATA_FOLDER}/crossref_dumps/Journal Of Politics.Rds"))
nrow(dt)
nrow(dt[published.print != ""])
nrow(dt[published.online != ""])

# Here published.online has only 291 compared to 11.5k total published.
# The other fields are similarly unreliable in detecting actual publication date.

dt <- fread("./output/109k_papers_all_coded_for_pub1.csv")
dt[, .N, by = pd_bool]
# What about online only journals?

dt <- fread("{DATA_FOLDER}/all_pubs2.csv")

# Group by journal and calculate the count of entries and non-empty published.print entries
journal_stats <- dt[, .(
    total_entries = .N,
    non_empty_published.print = sum(!is.na(published_print) & published_print != "")
), by = c("issn", "container_title")]

journal_stats[, c("issn1", "issn2", "issn3") := tstrsplit(issn, ",")]
journal_stats <- unique(journal_stats, by = c("issn1", "issn2", "issn3"))

journal_stats[non_empty_published.print == 0]
# Calculate the number of journals with 0 entries for published.print
zero_entries_journals <- journal_stats[non_empty_published.print == 0, .N]

# Calculate the average proportion of published.print entries for each journal
journal_stats[, avg_proportion := non_empty_published.print / total_entries]
journal_stats[container_title %like% "Journal of International Studies"]

# Print the results
cat("Number of journals with 0 entries for published.print:", zero_entries_journals, "\n")
cat("Average proportion of published.print entries for each journal:\n")
print(journal_stats[, .(container_title, avg_proportion)])
journal_stats[order(-avg_proportion)] %>% View()

# Findings:
# Broadly, we can see that say, 355/440 journals have over 80% published.print entries.
# And the remaining journals seem to be low impact.

# Let's check this once and for all, a different way.
# We'll filter on the journals in the PS and IR datasets by clarivate, in the top 100 for each field.

# Read in those files
ir <- fread("./output/JCR_International_Relations.csv", header = T) %>% janitor::clean_names()
ps <- fread("./output/JCR_Political_Science.csv") %>% janitor::clean_names()

#  we only want the top 100 in each cat. Need to clean up the issn data which is messy
ps_filtered <- ps[x2020_jif != "n/a" & x2020_jif > 1][order(-x2020_jif)][1:100]
ir[, c("issn", "e_issn") := tstrsplit(ISSN, ",")]
ir_filtered <- ir[order(-x2020_jif)][1:100]

# Isolate the values we want to filter on
issn_values <- c(ps_filtered$issn, ps_filtered$e_issn, ir_filtered$issn, ir_filtered$e_issn)

# Get them out. Then have a look.
journal_stats_filtered <- journal_stats[issn1 %chin% issn_values | issn2 %chin% issn_values]
journal_stats_filtered[order(-avg_proportion)] %>% View()
journal_stats_filtered[order(-avg_proportion) & avg_proportion == 0]$container_title

# Final cut
journal_stats_filtered %>%
    distinct(container_title, .keep_all = T) %>%
    .[order(-avg_proportion)] %>%
    .[avg_proportion == 0]

# So here we can see that there are 10, but one is blank and the other only has 1 in it.
# Also, looking at that one in the main dt we can see it has the vast majority in print -- it's an ISSN issue that makes it show up here.
# So dropping those two, we get 8 that have a legitimate absence of data in our dataset. Unfortunate, and frustrating.


# Just looking at a couple...
dt[container_title %like% "Azimuth"]
dt[container_title %like% "Journal of International Studies"]
