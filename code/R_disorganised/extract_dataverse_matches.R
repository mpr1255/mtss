library(tidyverse)

matching_files <- fread("./data/get_dataverse_entries_from_titles_incremental_matching.csv", fill = TRUE, header = TRUE)

matching_files[1:10]


matching_files[,(ncol(matching_files)-3):ncol(matching_files)] 
matching_files[3,(ncol(matching_files)-3):ncol(matching_files)]

match2 <- rowwise(matching_files)

test <- match2 %>% 
  colwise() %>%
  ifelse(. %in% c(""," ","NA"), NA, .)
  # select((ncol(match2)-2):ncol(match2))
  
str(test)

as.data.table(test)




View(matching2)
