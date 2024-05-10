# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    2.1_gather-process-fulltext.R                      :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: matthew p robertson <mpr213@pm.me>         +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/09/09 17:16:48 by matthew p r       #+#    #+#              #
#    Updated: 2023/01/29 18:59:55 by matthew p r      ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# EXPLANATION
# This file look at what fulltext files are available for all of the papers in the database,
# finds those that are not present (from html) and links them to the database.
# It reads in all the fst files, extracts all the dois for each individual paper,
# then checks if it's in the ./data/txt directory.
# For the dois that _do not have_ a fulltext file, it tries to download them from the doi URL.
# It does this first using standard cURL stuff, and failing that spins up a Selenium
# instance via docker and tries to get it that way.
# (This was necessary because some journals shroud their papers in javascript for some unknown reason...
# Also to circumvent the standard restrictions on simple cURL calls.)
# As with previous cases, the main file being operated on here is pubs.fst.
#
# **************************************************************************** #

source("./code/libs.R")
library(furrr)

plan(multisession, workers = 8)

# Find which fulltext files you've already got ----------------------------
# This is looking for txt files in the ./data/txt directory, named per the DOI (with / instead of _)
crossref_data <- list.files(glue("{here}/data/crossref_fst"), full.names = TRUE)
crossref_pub_data <- crossref_data[crossref_data %notlike% "author|url"]
target_journals <- as.data.table(read_fst("./data/target_journal_names_issn.fst"))

readFSTData <- function(file) {
  res <- read_fst(file)
  res$fst_file <- file
  return(res)
}

pubs <- data.table(map_df(crossref_pub_data, ~ readFSTData(.x)))
pubs <- pubs[str_extract(issn, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}") %chin%
  target_journals$issn | str_extract(issn, "(?<=,)[0-9A-Za-z]{4}-[0-9A-Za-z]{4}") %chin%
  target_journals$issn]
pubs[, container_title := trimws(container_title)]
pubs[, published_year := str_extract(published_print, "\\d{4}")]
pubs[is.na(published_year), published_year := str_extract(published_online, "\\d{4}")]
pubs <- pubs[published_year >= 2010]

pubs[, txt_file := trimws(paste0(str_replace_all(doi, "/", "_"), ".txt"))]
pubs[, got_fulltext := ifelse(file.exists(paste0("./data/txt/", txt_file)), T, F)]

# Check how many txt files you've already got
pubs[, .N, by = got_fulltext]
# pubs %>% write_fst("./out/pubs.fst")

# Download the html files of those not already got text for --------------------------------
DownloadHTML <- function(url, doi) {
  outfile <- glue('./data/html/{str_replace_all(doi, "/", "_")}.html')
  if (!file.exists(outfile)) try(curl_download(url, outfile))
  return(invisible(NULL))
}

# Identify the ones we've got first...
got_htmls <- data.table(html_file = list.files("./data/html"))
got_htmls[, doi := str_remove(str_replace_all(html_file, "_", "/"), "\\.html$")]

pubs_to_get <- pubs[got_fulltext == F & doi %notchin% got_htmls$doi]
pubs_to_get <- pubs_to_get[sample(nrow(pubs_to_get))]
map2(pubs_to_get$url, pubs_to_get$doi, ~ DownloadHTML(.x, .y))

# Get the stragglers with Selenium ----------------------------------------

# Check if docker is running and if it's not, then start it.
try(docker_running <- system("pgrep -x docker", intern = T))
if (length(docker_running) == 0) system("open -a docker")
if (!exists("selenium_id")) selenium_id <<- system("docker run -d -p 5901:5900 -p 4445:4444 edwaldoalmeida/selenium-standalone-chromium-debug:3.141.59-iron-aarch64", intern = T)

driver <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = 4445L, version = "latest", browser = c("chrome"))
driver$open()

seleniumDownloadHTML <- function(url, doi) {
  outfile <- glue('{here}/data/html/{str_replace_all(doi, "/", "_")}.html')
  print(outfile)
  driver$navigate(url)
  driver$setImplicitWaitTimeout(5000)
  page <- driver$getPageSource()
  writeChar(page[1][[1]], outfile, useBytes = T)

  if (readr::read_file(outfile) %like% "Cloudflare") {
    print("going back and now waiting")
    driver$navigate(url)
    Sys.sleep(5)
    page <- driver$getPageSource()
    writeChar(page[1][[1]], outfile, useBytes = T)
  }
  if (readr::read_file(outfile) %like% "Cloudflare") print("STILl!!! CHECK!!")
}

# Identify the ones we've got first...
got_htmls <- data.table(html_file = list.files("./data/html"))
got_htmls[, file_size := file.size(paste0(here, "/data/html/", html_file))]
got_htmls <- got_htmls[file_size > 30000]
got_htmls[, doi := str_remove(str_replace_all(html_file, "_", "/"), "\\.html$")]
pubs_to_get <- pubs[got_fulltext == F & doi %notchin% got_htmls$doi]

possibly_seleniumDownloadHTML <- possibly(seleniumDownloadHTML, otherwise = NA)
walk2(pubs_to_get$url[1:50], pubs_to_get$doi[1:50], ~ possibly_seleniumDownloadHTML(.x, .y))

if (exists("selenium_id")) {
  system(glue("docker stop {selenium_id}"))
  system(glue("docker rm {selenium_id}"))
}
print("now killing the docker desktop app")
system("pkill -SIGHUP -f /Applications/Docker.app 'docker serve'")
system("docker rm $(docker ps -a -q)")

# Convert them to txt -----------------------------------------------------
html_files <- list.files("./data/html/", pattern = "html", full.names = TRUE)
ParseHTMLtoTXT <- function(file) {
  if (file.size(file) < 50000) {
    return(invisible(NULL))
  }
  txt_file_out <- stri_replace_all_charclass(paste("./data/txt/", str_replace(basename(file), "html$", "txt")), "\\p{WHITE_SPACE}", "") # trim all whitespace
  if (file.exists(txt_file_out)) {
    return("file exists!")
  }
  fulltxt <- try(gettxt(file, encoding = "UTF-8"))
  fulltxt <- stri_replace_all_fixed(fulltxt, "\n", " ") # 4x faster than str_replace_all(fulltxt, "\\n", " ")
  try(write_lines(fulltxt, txt_file_out))
  return(invisible(NULL))
}
future_map(html_files, ~ ParseHTMLtoTXT(.x))

# Find the remainder for which there are no txt --------------------------
# Filter by year (or other criteria)
# Create bib file
# Import into zotero
# Get full text pdf from your university system
# Then convert the pdf files into txt and put them in the txt folder.

pdfs_to_get <- pubs[got_fulltext == F]
names(pdfs_to_get) <- str_to_upper(names(pdfs_to_get))
pdfs_to_get[, BIBTEXKEY := map(nrow(pdfs_to_get), ~ stringi::stri_rand_strings(n = .x, length = 4))]
pdfs_to_get[, BIBTEXKEY := paste0(PUBLISHED_YEAR, "_", BIBTEXKEY)]
pdfs_to_get[, CATEGORY := "ARTICLE"]
setnames(pdfs_to_get, "CONTAINER_TITLE", "JOURNAL")
setcolorder(pdfs_to_get, c("CATEGORY", "BIBTEXKEY"))

# Imports and exports here ------------------------------------------------------------
# This means that you export the file from here, import it into zotero,
# get all the pdf files, then export it from zotero and import it again here so you have a link between the doi and the pdf.
#  Once you've got that link, you can go and convert all those pdfs to txt.
# pdfs_to_get %>% bib2df::df2bib("./out/zotero_import_20220713.bib")
# pdfs_to_get[JOURNAL %like% "Political Science Quarterly" & TITLE %notlike% "\\$"] %>% bib2df::df2bib("./out/zotero_import_20220706_psq.bib")

# Convert pdfs to text ----------------------------------------------------
pubs[, got_fulltext := ifelse(file.exists(paste0("./data/txt/", txt_file)), T, F)]
pdfs_to_convert <- pubs[got_fulltext == F]
zotero_files <- as.data.table(bib2df::bib2df("./out/zotero_export_20220714.bib")) # This is your zotero import.
zotero_files[!is.na(FILE) & FILE != "NA", file_exist := file.exists(FILE)]

ConvertPDFToTXT <- function(pdf_file, txt_file) {
  if (is.na(pdf_file) | is.na(txt_file)) {
    return(invisible(NULL))
    # if(file.exists(glue("{here}/data/txt/{txt_file}"))) return(glue("GOT {txt_file}"))
  }
  # if(file.exists(glue("{here}/data/txt/{txt_file}"))) return(glue("GOT {txt_file}"))
  system(glue('pdftotext -raw -nopgbrk "{pdf_file}" - | tr "\n" " " > {here}/data/txt/{txt_file}'))
  print(glue("outputted {txt_file}"))
  return(invisible(NULL))
}

pdfs_to_convert <- pdfs_to_convert[zotero_files[file_exist == T], on = c("doi" = "DOI")]

map2(pdfs_to_convert$FILE, pdfs_to_convert$txt_file, ~ ConvertPDFToTXT(.x, .y))

# Check again how many txt files we have ----------------------------------
pubs[, got_fulltext := ifelse(file.exists(paste0("./data/txt/", txt_file)), T, F)]
pubs[, .N, got_fulltext]

# NOTE WELL: This process will need to be done for the pdf files as well.
# In the code above, this was mostly already done via a laborious process in Zotero.
# The full text files unfortunately cannot be made available along with the publication, because of copyright.
# You could also forgo pdf conversion and simply try to gather all like HTML here.
# Some code to do large-scale pdf-to-text conversion can be found in ./code/pdftotxt_DEPRECATED.R.
# Note this calls on the command line so you would need the pdftotext package, and run it in Mac/Linux or WSL in Windows
