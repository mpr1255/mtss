# /**
#* #/@ Author: Matthew Robertson
#* #/@ Create Time: 2024-05-09 16:48:54
#* #/@ Modified by: Matthew Robertson
#* #/@ Modified time: 2024-05-09 22:58:35
#* #/@ Description:
#  This file counts up the number of rows where supplementary and replication
#  material are discussed, near a URL (identified by http/www), and reference is made
#  to alternative code/data repositories Figshare, Code Ocean, and Dryad.
# We can see that there are very few such cases, and most already have available data.
# /**

# Load in libs & files
librarian::shelf(data.table, tidyverse)

dt_full <- fread("./output/109k_papers_all_coded_for_pub1.csv")
dt_full[pd_context %like% "figshare"]

dt <- fread("./output/supplementary_and_replication_mentions.csv")
dt[replication %like% "[Cc]ode [Oo]cean", .N, by = file_in]
dt[replication %like% "[Ff]igshare", .N, by = file_in]
dt[replication %like% "[Dd]ryad", .N, by = file_in]

all_relevant_dois <- c(dt[replication %like% "[Cc]ode [Oo]cean", .N, by = file_in]$file_in, dt[replication %like% "[Ff]igshare", .N, by = file_in]$file_in, dt[replication %like% "[Dd]ryad", .N, by = file_in]$file_in)
all_relevant_dois <- str_replace_all(all_relevant_dois, "_", "/")

##########
# Result
dt_full[doi %chin% all_relevant_dois & od_bool != 1] # only five.
