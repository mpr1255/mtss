source("./code/R/libs.R")

data1 <- fread("./output/109k_papers_all_coded_for_pub1.csv")

get_power_analysis_papers <- function(doi){
  doi <- str_replace_all(doi, "/", "_")
  file <- paste0(here, "/data/txt/", doi, ".txt")
  if(file.exists(file)){
    print(file)
    full_text <- fread(file, header = F, sep = NULL)
    res <- str_detect(full_text, "power analys")
    res1 <- as.data.table(cbind(doi, res))
    return(res1)
  }
  
}

exp_bool_papers <- data1[exp_bool == 1]
exp_bool_dois <- exp_bool_papers[,.(doi)]
doi <- exp_bool_dois[1]

res1 <- map_df(exp_bool_dois$doi, ~try(get_power_analysis_papers(.x)))
res2 <- res1[res == TRUE]
res2$doi %>% write_lines("./output/exp_bool_papers_mentioning_power_analysis.txt")
res2 <- as.data.table(map_df(test, ~tibble(bools = .x)))
nrow(res2[bools == TRUE])

# Link those dois to the paper names for easy discovery -------------------

pa <- read_lines("./output/exp_bool_papers_mentioning_power_analysis.txt")

data1[doi %in% str_replace_all(pa, "_", "/"),.(doi, title, journal_name)] %>% fwrite("./output/exp_bool_papers_mentioning_power_analysis.csv")

options(datatable.prettyprint.char=20L)
