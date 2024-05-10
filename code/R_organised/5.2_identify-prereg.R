library(here)
library(glue)
here <- here()
source(glue("{here}/code/libs.R"))
library(furrr)

exp_papers <- data.table(read_fst("./out/experimental_papers_classified.fst"))


# Now figure out which of the experimental papers were prereg -------------

checkForPrereg <- function(file){
  # file <- exp_papers[exp_bool == 1]$doi[81]
  print(file)  
  file <- paste0(here,"/data/txt/",str_replace_all(file, "/", "_"), ".txt")
  fulltext <- fread(file, header = FALSE, sep = NULL)
  says_prereg <- ifelse(str_detect(fulltext, "prereg|pre-reg|pre reg"), 1, 0)
  has_link <- ifelse(str_detect(fulltext, "osf.io|egap.org|aspredicted.org|e-gap"), 1, 0)
  
  prereg_context <- str_extract_all(fulltext, ".{1,50}prereg.{1,50}|.{1,50}pre-reg.{1,50}|.{1,50}pre reg.{1,50}")
  link_context <- str_extract_all(fulltext, ".{1,50}osf\\.io.{1,50}|.{1,50}egap\\.org.{1,50}|.{1,50}aspredicted\\.org.{1,50}|.{1,50}e-gap.{1,50}")
  
  return(data.table(file, says_prereg, has_link, prereg_context, link_context))
  
}

res <- map_df(exp_papers[exp_bool == 1]$doi, ~checkForPrereg(.x))
res[,prereg_context := paste0(prereg_context)]
res[,link_context := paste0(link_context)]
res %>% write_csv("./out/prereg.csv")

res[says_prereg == 1 & has_link == 0]$prereg_context

options(datatable.prettyprint.char=20L)





# exp_count_edit[prereg_term_context %like% "osf.io|egap.org|aspredicted.org", uniqueN(file_in)]
# 
# exp_count_edit[prereg_term_context %like% "osf|egap|aspredicted", uniqueN(file_in)]
# 
# exp_count_edit[prereg_term_context %like% "osf|egap|aspredicted"] %>% fwrite("./output/experiment_prereg_context4.csv")
# 
# 
# exp_count_edit[, uniqueN(file_in)]
# 


# experimental_count_full <- fread("./output/experimental_count_full_20210907.csv")
# prereg_count <- map_df(experimental_count_full$file_in, ~try(get_prereg(.x)))
# 
# terms <- c("aspredicted", "osf", "egap", "e-gap")
# terms_ast <- paste0("***", terms, "***")
# for(i in seq_along(terms)){
#   prereg_count[,context := str_replace_all(context, terms[i], terms_ast[i])]
# }
# test1 <- prereg_count[context %like% paste(terms, collapse = "|"), relevant := TRUE]



# Load coded prereg -------------------------------------------------------

# prereg_codes <- fread("./output/prereg_count2_links.csv")
# prereg_codes[relevant == TRUE] %>% count(validate)
# 
# 
# test_for_terms <- map_df()
# 
# 
# fwrite(test1, "./output/prereg_count2.csv")
# 

# Couldn't figure out how to purrr it. 
# prereg_count[,context:= map2_dfr(terms, paste0("***", terms,"***"), ~str_replace_all(prereg_count$context, .x, .y))]
# map2(terms, paste0("***", terms,"***"), ~stri_replace_all_coll(prereg_count$context[1:2], .x, .y))
# map(prereg_count$context[1:20], ~stri_replace_all_coll(.x, terms, paste0("***", terms,"***")))


# prereg_count[context %like% "e-gap", .N]
# 
# files_w_egap <- read_lines("./data/misc/other_experimental_papers.txt")
# files_w_egap <- as.data.table(files_w_egap)
# 
# prereg_count[file_in %in% files_w_egap]
# 
# files_w_egap[files_w_egap %notin% experimental_count_full$file_in]
# 
# prereg_count[file_in %notin% ]
# 
# 
# 
# 
# prereg_count[title == "NULL"]$title <- "none"
# prereg_count[journal == "NULL"]$journal <- "none"
# 
# write_xlsx(prereg_count, "./output/prereg_context.xlsx")
# 
# setDT(prereg_count)
# 
# prereg_count[is.na(prereg_count)]
# 
# prereg_count %>% distinct(file_in)
# 
# prereg_count[context %like% "egap", .N]
# prereg_count[context %like% "osf", .N]
# prereg_count[context %like% "aspredicted", .N]
# 
# is.null()
# 
# View(prereg_count)
# 
# fwrite(prereg_count, "./output/prereg_count.csv")
# 
# prereg_count
# 
# 
# prereg_count[context %like% "http"]
# 
# prereg_count %>% View



