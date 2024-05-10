devtools::load_all("/Volumes/t7/projects/transparentr")
library(transparentr)

source("./code/libs.R")
library(furrr)
plan(multisession, workers = 8)
options(future.rng.onMisuse = "ignore")


# This script brings together all the different steps of the analysis
# to create the final table on which the results are based.
# The following steps are integrated in this script:
# 1. pubs.fst -- this is the raw crossref data. This script cleans it up.
# 2. classified_papers.fst -- this applies stat_bool to all of the papers (i.e. whether it is a quant paper or not)
# dataverse_dois_from_api --
# . dataverse_dois_from_papers.fst -- these are all the dataverse_dois extracted from the txt files of papers.
# .  -- reports whether a paper has precarious data and/or supplementary materials
# . journal_data.fst -- reports replication data for the few journals that host their own materials


# Get the data in and unique it ---------------------------------------------------------
pubs <- data.table(read_fst("./out/pubs.fst")) %>% distinct(doi, .keep_all = T)


# Now add all of the various variables that have been determined through the
# analysis:
# Stat_bool
# OD_bool
# exp_bool

# Fix misclassifications of true stat papers --------------------------------------------------









# [replication_link %notlike% "dataverse|harvard|edu|cambridge|prio|sage|10.7910|doi.org|oup.com"]












# At that point all of the _dataverse_ links will be done. BUt that is only dataverse ones!! The next ones to join are the jcr, jpr, dart, and then finally precarious.
#
# Then join the experimental ones
#
# The final is going to be called final.fst.
#
# And then finally finally finally, create the key_values.fst file which has all of the key variables that are going to be used throughout the paper. That gets created in this file as well. Sheesh!!!

# Add matches of dataverse links pulled from fulltext ---------------------

dataverse_dois_from_papers <- read_fst("./out/dataverse_dois_from_papers.fst")














# Truncate to stat papers only --------------------------------------------

stat_papers <- read_fst("./out/classified_papers.fst")




# Add JPR and JCR ---------------------------------------------------------
jcr <- read_fst("./out/jcr.fst")
jpr <- read_fst("./out/jpr.fst")



# Add DART journals -------------------------------------------------------




# Add precarious data -----------------------------------------------------

precarious <- read_fst("./out/precarious.fst")




res1 <- all_dv_links[all_files, on = c("paper_doi" = "doi")]
