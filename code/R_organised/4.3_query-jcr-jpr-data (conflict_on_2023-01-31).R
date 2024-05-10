# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    4.3_query-jcr-jpr-data.R                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 22:09:48 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/31 16:23:07 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# This is specifically for querying data from jcr and jpr — because
# (1) They have excellent replication practices;
# (2) They use their own sites, not the Dataverse;
# (3) They use a consistent format on their sites.
# **************************************************************************** ##

devtools::load_all("/Volumes/t7/projects/transparentr")
library(transparentr)
source("./code/libs.R")
library(furrr)
plan(multisession, workers = 8)
options(future.rng.onMisuse = "ignore")

# Find JPR replication materials ------------------------------------------
jpr_rep_files <- fread(glue("{here}/data/jpr_replication_papers.csv"), sep = "|")
jpr_rep_files[, title1 := str_extract(author_title, "(?<=[\\:|\\?]).*")][]
jpr_rep_files[, match_title := str_extract(title1, "[^\\:|\\?]*")][]
jpr_rep_files[is.na(match_title), match_title := author_title][]

# jpr_rep_files[author_title %like% "Basque"] # Test

pubs <- data.table(read_fst("./out/pubs.fst"))
pubs <- as.data.table(distinct(pubs, doi, .keep_all = T))

if (!file.exists("./out/jpr.fst")) {
  all_jpr_papers <- pubs[container_title %like% "Peace Research"]
  all_jpr_papers[title %like% "[\\:\\?]", match_title := str_extract(title, "[^\\:|\\?]*")][]
  all_jpr_papers[is.na(match_title), match_title := title][]

  fuzzyMatchJPRTitles <- function(search_title) {
    url <- jpr_rep_files[match_title == search_title]$url

    search_title <- str_trim(search_title)
    print(search_title)

    title_match <- all_jpr_papers[amatch(str_to_lower(search_title), str_to_lower(all_jpr_papers$match_title), method = "jw", maxDist = 0.24)]$match_title

    doi <- all_jpr_papers[match_title == title_match]$doi
    diff <- stringdist(search_title, title_match, method = "jw")
    orig_title <- all_jpr_papers[match_title == title_match]$title

    data.table(orig_title, search_title, title_match, doi, diff, url)
  }

  matches <- map_df(jpr_rep_files$match_title, ~ fuzzyMatchJPRTitles(.x))
  setorder(matches, -diff)[]
  jpr_matches <- matches[!is.na(doi) & diff < .24]
  write_fst(jpr_matches, "./out/jpr.fst")
}

# Find and match JCR (Journal of Conflict Resolution) ------------------------------
dl_dir <- glue(("{here}/data/jcr_replication"))
pubs <- data.table(read_fst("./out/pubs.fst"))[container_title %like% "Conflict Resolution"]
jcr_urls <- map_chr(pubs$doi, ~ paste0("https://journals.sagepub.com/doi/suppl/", .x))

if(!file.exists("./out/jcr.fst")){

  getJCR_URL <- function(url) {
    # url <- jcr_urls[1]
    print(url)
    doi <- stri_replace_all_fixed(url, "https://journals.sagepub.com/doi/suppl/", "")
    outfile <- paste0(dl_dir, "/", str_replace_all(doi, "/", "_"), ".html")

    if (file.exists(outfile)) {
      if (file.size(outfile) > 15000) {
        print("got the html and it's good.")
        return()
      } else {
        # This means it exists but it's tiny, so it's a javascript thing. So
        # delete it and try again with selenium.
        print(glue("got the file but it's too small -- going selenium route -- getting {doi}"))
        driver$navigate(url)
        driver$getCurrentUrl()
        page_source <- driver$getPageSource()
        write_lines(page_source, outfile)
      }
    } else {
      print("file doesn't exist at all; getting now.")
      system(paste0("curl ", url, " --cookie-jar -c -L -o ", outfile))
    }
  }
  # Only run if there are files that did not get downloaded properly or
  # something.
  # driver <- getSeleniumDriver()
  # driver$open()

  walk(jcr_urls, ~ getJCR_URL(.x))

  system(glue('rg "suppl_file.*?>" -uio {dl_dir} > {here}/out/jcr_replication_files.txt'))
  jcr_rep <- fread(glue("{here}/out/jcr_replication_files.txt"), sep = ":", header = F)
  jcr_rep <- jcr_rep[V2 %notlike% "pdf"][] # negative look behind
  jcr_rep[, doi := str_extract(V1, "10.*?(?=\\.html)")][]
  jcr_rep[, od_doi := paste0(
    "https://journals.sagepub.com",
    str_extract(V2, "\\/doi\\/suppl\\/.*?\\.[a-z|7z]{2,3}")
  )][]
  jcr_rep$V1 <- NULL
  jcr_rep$V2 <- NULL
  jcr_rep <- jcr_rep %>% distinct(doi, .keep_all = T)
  write_fst(jcr_rep, "./out/jcr.fst")

}

data.table(read_fst("./out/jcr.fst"))