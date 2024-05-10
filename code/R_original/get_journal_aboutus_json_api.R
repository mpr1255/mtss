require(searchConsoleR)
require(httr)
require(tidyverse)
require(data.table)
require(glue)
require(here)
require(janitor)
require(curl)

here <- here()
google_key <- Sys.getenv('google_key')
google_cx <- Sys.getenv('google_cx')

get_searches <- function(site, search_term, google_key, google_cx, country = "us"){
  keyword <- glue('{site} {search_term}')
  
  # keyword  keywords[10]; country = "us"
  url1 <<- paste0("https://www.googleapis.com/customsearch/v1?"
                  , "key=", google_key
                  # , "&q=", gsub(" ", "+", keyword)
                  , "&q=", keyword
                  , "&gl=", country         # Country
                  , "&hl=en"                # Language from Browser, english
                  , "&cx=", google_cx
                  # , "&fields=items(link)"
                  # , "&start=1",
                  , "&filter=0"
                  , "&start=1"
                  
  )
  
  d2 <<- url1 %>%
    httr::GET(ssl.verifypeer=TRUE) %>%
    httr::content(.) %>% .[["items"]] %>%
    data.table::rbindlist(., fill = TRUE) %>%
    mutate(keyword, serp = row_number(), search_engine = "Google") %>%
    # rename(source = link) %>%
    select(search_engine, keyword, serp, link, title)
  
  # pause <- round(runif(1, min = 1.1, max = 1.5), 1)
  pause <- 0
  if(nrow(d2) == 0)
  {cat("\nPausing", pause, "seconds. Failed for:", keyword)} else
  {cat("\nPausing", pause, "seconds. Successful for:", keyword)}
  
  Sys.sleep(pause)
  # rm(keyword, country, pause, url1, google.key, google.cx)
  return(d2)
}

search_terms <- c("author guidelines", "submission guidelines", "replication data policy")

journal_names <- fread(glue("{here}/data/jcr_combined_list.csv"))$journal_name

search_combo <- rbindlist(cross2(journal_names, search_terms))
names(search_combo) <- c("site", "search_term")

output <- map2_df(search_combo$site, search_combo$search_term, ~get_searches(.x, .y, google_key, google_cx))

output1 <- output %>% 
  mutate(journal = str_remove_all(keyword, "author|guidelines|submission|data|replication|policy"))

output2 <- output1 %>% distinct(link,.keep_all = TRUE)

# output1 <- output[1]

# output2 %>% writexl::write_xlsx(glue("{here}/data/journal_list_about_us_results.xlsx"))

