# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    4.4_precarious-data.R                              :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 23:33:00 by matthew p r       #+#    #+#              #
#    Updated: 2022/09/09 23:53:58 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# **************************************************************************** #
# EXPLANATION
# Goes through all classified papers and pulls out references to what we call
# 'precarious data' — i.e. data hosted on the author's personal website, etc.
# **************************************************************************** #

devtools::install("/Volumes/t7/projects/transparentr", reset = TRUE)
library(transparentr)
source("./code/libs.R")
library(furrr)
plan(multisession, workers = 8)
options(future.rng.onMisuse = "ignore")

# Setup -------------------------------------------------------------

pubs <- as.data.table(read_fst("./out/pubs.fst"))
pubs <- distinct(pubs, doi, .keep_all = T)
stat_papers <- as.data.table(read_fst("./out/classified_papers.fst"))[
    stat_bool == 1,
    .(file, title, journal)
]
stat_papers[, doi := str_remove(str_replace_all(file, "_", "/"), "\\.txt$")]
setcolorder(stat_papers, "doi")
pubs <- pubs[doi %chin% stat_papers$doi]

# Define the words I'm looking for.
related_words <- c(
    "data", "file", "code", "material",
    "information", "do-files", "folder",
    "author", "result", "archive", "script"
)
# Define only for the files where dataverse_doi = NA -----------------------
# Define the path where all the fulltxt files are.
txtpath <- "/Volumes/t7/projects/mtss/data/txt/"
# Run the main function to find the precarious data and time it.
tictoc::tic()
res <- future_map2_dfr(pubs$txt_file, pubs$doi, ~
    transparentr::search_precarious(.x, .y,
        path_to_fulltext = txtpath,
        related_words
    ))
tictoc::toc()

unique(res, by = "doi")[!is.na(replication_link)]
unique(res, by = "doi")[!is.na(supplementary_link)]

res <- res[doi %chin% stat_papers$doi][!is.na(replication_link) | !is.na(supplementary_link)]
write_fst(res, "./out/precarious.fst")
