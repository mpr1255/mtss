# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    4.5_query-dart-journals.R                          :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 22:26:31 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/31 16:06:56 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# This code looks individually at each of the six DA-RT journals that got consistently
# low scores, and looks at the supplementary files on their website for replication
# materials. The R script that builds the final table reads this data in and uses it for
# the final graphs.
#
# These are all the DA-RT journals, but we're only interested in the low scoring ones.
# all_dart_journals <- c("American Journal Of Political Science", "American Political Science Review", "British Journal Of Political Science", "Comparative Political Studies", "Conflict Management And Peace Science", "European Journal Of Political Research", "European Union Politics", "International Interactions", "Journal Of Conflict Resolution", "Journal Of European Public Policy", "Journal Of Peace Research", "Party Politics", "Political Analysis", "Political Behavior", "Political Science Research And Methods", "The Journal Of Politics")
#
# Journals provide supplementary/replication materials in a number of different ways.
# The first way is directly at the article page.
# The journals to which this applies are: EJPR and JEPP.
# The others are all SAGE journals and have supplementary material at a separate link.
# This code checks for this distinction and crawls accordingly.
# We have to use browser emulation because these journals love their JavaScript.
# **************************************************************************** #

library(here)
library(glue)
here <- here()
source(glue("{here}/code/libs.R"))
library(furrr)
library(RSelenium)

pubs <- data.table(read_fst(glue("{here}/out/pubs.fst")))
pubs <- distinct(pubs, doi, .keep_all = T)

# These are the ones of interest because they had the low scores.
# It's these that we particularly want to make sure we analysed correctly.
dart_low_journals <- c("Conflict Management And Peace Science", "Comparative Political Studies", "European Journal Of Political Research", "European Union Politics", "Journal Of European Public Policy", "Party Politics")


# Define a general function to download html files of articles from DOIs --------
downloadDOI <- function(f_doi) {
  # f_doi <- dart_low_reg_pubs$doi[3]

  journal_name <- str_to_lower(str_replace_all(pubs[doi == f_doi]$container_title, "\\s+", "_"))
  journal_name1 <- pubs[doi == f_doi]$container_title

  out_file <- paste0(dart_html_dir, journal_name, "--", str_replace_all(f_doi, "/", "_"), ".html")

  if (file.exists(out_file)) {
    print(paste(basename(out_file), "exists!"))
    return()
  }

  if (journal_name1 %in% c("Journal Of European Public Policy", "European Journal Of Political Research")) {
    print("normal curl")
    doi_url <- paste0("https://www.doi.org/", f_doi)
    res <- curl_fetch_memory(doi_url)
    final_url <- res$url
    print(final_url)
    h <- get_handle()
    curl_download(final_url, out_file, handle = h)
  } else {
    print("going selenium route")
    final_url <- glue("https://journals.sagepub.com/doi/suppl/{f_doi}")
    driver$open()
    driver$navigate(final_url)
    driver$getCurrentUrl()
    page_source <- driver$getPageSource()
    write_lines(page_source, out_file)
  }

  doi_and_final <- data.table(doi_url, final_url)
  fwrite(doi_and_final, glue("{here}/data/dart_verification/doi_urls_and_final_urls.csv"), append = T, quote = T)


  if (file.exists(out_file)) {
    print(paste("downloaded", basename(out_file)))
  } else {
    print("FAILED STOP. ")
  }
}

dart_low_reg_pubs <- pubs[container_title %chin% dart_low_journals]
dart_html_dir <- glue("{here}/data/dart_verification/html/")
possibly_downloadDOI <- possibly(downloadDOI, otherwise = NA)
walk(dart_low_reg_pubs$doi, ~ possibly_downloadDOI(.x))

# Shut down docker
if (exists("selenium_id")) {
  system(glue("docker stop {selenium_id}"))
  system(glue("docker rm {selenium_id}"))
}
print("now killing the docker desktop app")
system("pkill -SIGHUP -f /Applications/Docker.app 'docker serve'")

# LOCAL ONLY --------------------------------------------------------------
# Now that we've downloaded all of the papers and/or replication/supplementary materials
# for the responsive ~2.5k papers (as of 20220910), it's time to pull the supplementary material off them.

dart_papers <- data.table(list.files(dart_html_dir, full.names = T))
dart_papers <- dart_papers[, doi := str_extract(V1, "10\\..*?(?=.html)")]
dart_papers[, pub_file := basename(V1)]

checkReplicationFiles <- function(file) {
  # file <- dart_papers[1]
  fulltext <- read_file(file)
  file_hit <- str_extract_all(fulltext, "\\.xls\\b|\\.xlsx\\b|\\.zip\\b|\\.csv\\b|\\.dta\\b|\\.do\\b|\\.Rds\\b|\\.py|\\.doc|\\.docx|\\.pdf")
  f <- unlist(file_hit)

  res <- as.data.table(cbind(basename(file), as.character(f)))
  return(res)
}
library(furrr)
rep_files <- map_df(dart_papers$V1, ~ checkReplicationFiles(.x))
names(rep_files) <- c("pub_file", "rep_file")

rep_files <- rep_files[target_dart_papers_w_dois, on = "pub_file"]

straight_doi_url_journals <- str_replace_all(str_to_lower(c("Journal Of European Public Policy", "European Journal Of Political Research")), "\\s+", "_")

rep_files[, od_doi := ifelse(str_detect(pub_file, straight_doi_url_journals), paste0("https://www.doi.org/", doi), glue("https://journals.sagepub.com/doi/suppl/{doi}"))][]

rep_files2 <- rep_files[rep_file %notlike% "pdf|doc", od_bool := 1][!is.na(od_bool)] %>% distinct(doi, od_bool)
rep_files2[rep_files, od_doi := i.od_doi, on = "doi"][]

write_fst(rep_files2, glue("{here}/out/dart_verification.fst"))
