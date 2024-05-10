here <- rprojroot::find_rstudio_root_file()
library(glue)
library(RSelenium)

source(glue("{here}/code/R/libs.R"))


# setwd("/Volumes/PortableSSD/trust_us/output")
data <- fread(glue("{here}/output/109k_papers_all_coded_for_pub1.csv"))

# This code looks individually at each of the six DA-RT journals that got consistently low scores, and looks at the supplementary files on their website for replication materials. The R script that builds the final table reads this data in and uses it for the final graphs. 

all_dart_journals <- c("American Journal Of Political Science", "American Political Science Review", "British Journal Of Political Science", "Comparative Political Studies", "Conflict Management And Peace Science", "European Journal Of Political Research", "European Union Politics", "International Interactions", "Journal Of Conflict Resolution", "Journal Of European Public Policy", "Journal Of Peace Research", "Party Politics", "Political Analysis", "Political Behavior", "Political Science Research And Methods", "The Journal Of Politics")

# These are the ones of interest. 
dart_low_journals <- c("Conflict Management And Peace Science", "Comparative Political Studies", "European Journal Of Political Research", "European Union Politics", "Journal Of European Public Policy", "Party Politics")

#########################
##   GENERAL EXPLORATORY CODE ##
##   # There are six journals. There are two different ways of accessing the supplementary/replication materials in them. The first way is directly at the article page. The journals to which this applies are: EJPR and JEPP. The others are all SAGE journals and have supplementary material at a separate link. This function checks for this distintion and crawls accordingly. 
#########################

#Rselenium up in here. 
print("need a new cookie, rebooting selenium etc.")
print("first open docker, this will take a minute.........")
system("open -a Docker")
Sys.sleep(30)

selenium_id <- system("docker run -d -p 5901:5900 -p 4445:4444 edwaldoalmeida/selenium-standalone-chromium-debug:3.141.59-iron-aarch64", intern = T)
driver <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, version = "latest", browser=c("chrome"))
# The whole 'edwaldoal' thing changes to selenium/standalone-chrome-debug when using intel architecture.

# Sys.sleep(10)
driver$open()
# Sys.sleep(10)

# Shut down docker
if(exists("selenium_id")){
  system(glue("docker stop {selenium_id}"))
  system(glue("docker rm {selenium_id}"))
}
print("now killing the docker desktop app")
system("pkill -SIGHUP -f /Applications/Docker.app 'docker serve'")

get_handle <- function(){
  h <- new_handle()
  
  handle_setheaders(h,
                    'authority' = 'journals.sagepub.com' ,
                    'cache-control' = 'max-age=0' ,
                    'upgrade-insecure-requests' = '1' ,
                    'user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36' ,
                    'accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' ,
                    'sec-gpc' = '1' ,
                    'sec-fetch-site' = 'none' ,
                    'sec-fetch-mode' = 'navigate' ,
                    'sec-fetch-user' = '?1' ,
                    'sec-fetch-dest' = 'document' ,
                    'accept-language' = 'en-GB,en-US;q=0.9,en;q=0.8')
  
  handle_setopt(h, 
                "timeout" = 300,
                "connecttimeout" = 10,
                "ssl_verifyhost" = 0L,
                "ssl_verifypeer" = 0L)  
  return(h)
}


# Define a general function to download html files of articles from DOIs --------

DownloadDOI <- function(f_doi){
  # f_doi <- pubs_to_inspect$doi[3]

  journal_name <- str_to_lower(str_replace_all(pubs_to_inspect[doi == f_doi]$journal_name, "\\s+", "_"))
  journal_name1 <- pubs_to_inspect[doi == f_doi]$journal_name
  
  out_file <- paste0(dart_html_dir, journal_name, "--", str_replace_all(f_doi, "/", "_"), ".html")
  
  if(file.exists(out_file)){
    print(paste(basename(out_file),"exists!"))
    return()
  }
  
  if(journal_name1 %in% c("Journal Of European Public Policy", "European Journal Of Political Research")){
    print("normal curl")
    doi_url <- paste0("https://www.doi.org/", f_doi)
    res <- curl_fetch_memory(doi_url)
    final_url <- res$url
    print(final_url)
    h <- get_handle()
    curl_download(final_url, out_file, handle = h)
    
  }else{
    print("going selenium route")
    final_url <- glue("https://journals.sagepub.com/doi/suppl/{f_doi}")
    driver$navigate(final_url)
    driver$getCurrentUrl()
    page_source <- driver$getPageSource()
    write_lines(page_source, out_file)
  }
  
  
  doi_and_final <- as.data.table(cbind(doi_url, final_url))
  fwrite(doi_and_final, "./data/dart_verification/doi_urls_and_final_urls.csv", append = T, quote = T)
  
  
  if(file.exists(out_file)){
    print(paste("downloaded", basename(out_file)))  
  }else{
    print("FAILED STOP. ")
  }
  
}

dart_low_reg_pubs <- data[journal_name %like% paste0(dart_low_journals, collapse = "|")]

# this is for all the journals. If looking to do a subset then add head(.SD, 50) between , , 
pubs_to_inspect <- dart_low_reg_pubs[stat_bool == 1][order(-published_print_ym_date_format), (.SD), by = journal_name][,.(doi, journal_name, published_print, title, dart_year, published_print_ym_date_format)]
setDT(pubs_to_inspect)

dart_html_dir <- glue("{here}/data/dart_verification/html/")

# got_dois <- str_replace(str_extract(list.files(dart_html_dir), "10\\..*?(?=.html)"), "_", "/")
# pubs_to_inspect <- pubs_to_inspect[doi %notchin% got_dois]

SafelyDownloadDOI <- safely(DownloadDOI)
map(pubs_to_inspect$doi, ~SafelyDownloadDOI(.x))

# LOCAL ONLY --------------------------------------------------------------
# Now that we've downloaded all of the papers and/or replication/supplementary materials for the responsive ~2.2k papers, it's time to 


dart_papers <- as.data.table(list.files(dart_html_dir, full.names = T))
dart_papers_w_dois <- dart_papers[,doi := str_extract(V1, "10\\..*?(?=.html)")]
target_dart_papers_w_dois <- dart_papers_w_dois[doi %in% str_replace_all(pubs_to_inspect$doi, "/", "_")]
target_dart_papers_w_dois[,pub_file := basename(V1)]

CheckReplicationFiles <- function(file){
  # file <- dart_papers[1]
  fulltext <- read_file(file)
  file_hit <- str_extract_all(fulltext, "\\.xls\\b|\\.xlsx\\b|\\.zip\\b|\\.csv\\b|\\.dta\\b|\\.do\\b|\\.Rds\\b|\\.py|\\.doc|\\.docx|\\.pdf")
  f <- unlist(file_hit)

  res <- as.data.table(cbind(basename(file), as.character(f)))
  return(res)
}
library(furrr)
rep_files <- future_map_dfr(target_dart_papers_w_dois$V1, ~CheckReplicationFiles(.x))
names(rep_files) <- c("pub_file", "rep_file")

rep_files <- rep_files[target_dart_papers_w_dois, on = "pub_file"]

straight_doi_url_journals <- str_replace_all(str_to_lower(c("Journal Of European Public Policy", "European Journal Of Political Research")), "\\s+", "_")

rep_files[,od_doi := ifelse(str_detect(pub_file, straight_doi_url_journals), paste0("https://www.doi.org/",doi), glue("https://journals.sagepub.com/doi/suppl/{doi}"))][] 

rep_files2 <- rep_files[rep_file %notlike% "pdf|doc", od_bool := 1][!is.na(od_bool)] %>% distinct(doi, od_bool)
rep_files2[rep_files, od_doi := i.od_doi, on = "doi"][]

fwrite(rep_files2, "./output/dart_verification_od_bool_results.csv")


