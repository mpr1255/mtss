source("./code/R/libs.R")

###################################################################################################################
#
# This code brings all of the findings together into the single main table. The foundational table is the crossref publications;
# after that, each new piece of data just becomes another column. 
#
# 
#
#
#
###################################################################################################################

all_files <- fread("./output/all_109k_pubs_cleanish.csv")
all_dv_links <- fread("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")
all_dv_links <- all_dv_links %>% distinct(dv_doi, .keep_all = T)

res1 <- all_dv_links[all_files, on = c("paper_doi" = "doi")]

n_total_txt <- list.files("./data/txt")

# res1[!is.na(dv_doi)]

stat_bool_scores <-  fread("./output/production_stat_bool_prediction1_20210909.csv")

stat_scores_to_match <- stat_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), stat_bool = test_pred)]
res2 <- stat_scores_to_match[res1, on = "paper_doi"]

data_bool_scores <- fread("./output/production_data_bool_prediction1_20210909.csv")
data_scores_to_match <- data_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), data_bool = pred_production)]

res3 <- data_scores_to_match[res2, on = "paper_doi"]
res3[,dv_doi_bool := ifelse(str_detect(dv_doi, "10.7910"), 1, 0)]
res3[is.na(dv_doi_bool), dv_doi_bool := 0]


# Add experimental bools
exp_bool_scores <- fread("./output/exp_bool_predictions_20210911_1558.csv")
exp_bool_scores_to_match <- exp_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), exp_bool)]

# Add prereg bool
prereg_bool_scores1 <- readxl::read_excel("./output/potential_prereg_papers_final_round_edited.xlsx")
setDT(prereg_bool_scores1)
prereg_bool_scores1 <- prereg_bool_scores1[!is.na(validate),.(file_in, validate)]

prereg_bool_scores2 <- readxl::read_excel("./output/prereg_count2_links_edited.xlsx")
setDT(prereg_bool_scores2)
prereg_bool_scores2 <- prereg_bool_scores2[!is.na(validate),.(file_in, validate)]

prereg_bool_scores3 <- readxl::read_excel("./output/experiment_prereg_contextl4_links.xlsx")
setDT(prereg_bool_scores3)
prereg_bool_scores3 <- prereg_bool_scores3[!is.na(validate),.(file_in, validate)]

prereg_bool_scores <- rbind(prereg_bool_scores1, prereg_bool_scores2, prereg_bool_scores3)

prereg_bool_scores <- prereg_bool_scores[validate != "999"]
prereg_bool_scores <- prereg_bool_scores[,.(file_in, prereg_score = validate) ]

prereg_bool_scores_to_match <- prereg_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), prereg_score)]

# Add both experimental and prereg scores
res4 <- exp_bool_scores_to_match[res3, on = "paper_doi"]
res4[is.na(exp_bool), exp_bool  := 0]

res4 <- prereg_bool_scores_to_match[res4, on = "paper_doi"]
res4[is.na(prereg_score), prereg_score  := 0]

# Reclassify and declassify per correct coding of prereg status
res4[prereg_score == 1 | prereg_score == 2][exp_bool == 0]$exp_bool <- 1

# Add in DART scores
dart_2016_journals <-  str_to_title(c("American Journal of Political Science", "American Political Science Review", "British Journal of Political Science", "Comparative Political Studies", "Conflict Management and Peace Science", "Cooperation and Conflict", "European Journal of Political Research", "European Political Science", "European Union Politics", "International Interactions", "International Security", "Journal of Conflict Resolution", "Journal of European Public Policy", "Journal of Peace Research", "Journal of Theoretical Politics", "Quarterly Journal of Political Science", "Party Politics", "Political Analysis", "Political Behavior", "Political Science Research and Methods", "Research and Politics", "Rivista Italiana di Scienza Politica", "State Politics and Policy Quarterly", "Security Studies", "The Journal of Politics", "The Political Methodologist"))

#NOTE: These came from  https://www.cambridge.org/core/journals/political-science-research-and-methods/article/data-access-and-research-transparency-dart-a-joint-statement-by-political-science-journal-editors/5F36CC53B72CDB9A638AF99E363317E2 There are six missing: 
setnames(res4,  "container_title", "journal_name")
names_in <- res4[journal_name %in% dart_2016_journals, unique(journal_name)]
# names_in %>% write_lines("./output/dart_2016_journals_in_our_data.txt")
setdiff(dart_2016_journals, names_in)

# add 2016 as the year these journals signed onto DA-RT
res4[journal_name %in% dart_2016_journals, dart_year := "2016"]

# make the bool for any published after 2016
res4[!is.na(dart_year), dart_bool := ifelse(published_print >= 2016, 1, 0)]

# diversion: Note that this is anomalous 
res4[dv_doi_bool == 1 & stat_bool == 0 & data_bool == 0]

# This is resolved in identify_problems_with_stat_bool_scores.R and the output is read in below. It adds another ~200 papers to stat_bool. 
recoded_misclassified_ml_papers_from_443 <- fread("./output/recoded_misclassified_ml_papers_from_443.csv")
names(recoded_misclassified_ml_papers_from_443) <- c("paper_doi", "stat_bool")
recoded <- recoded_misclassified_ml_papers_from_443[,.(paper_doi = str_replace_all(paper_doi, "_", "/"), stat_bool)]

# Do that 
res4[paper_doi %in% recoded$paper_doi]$stat_bool <- recoded$stat_bool

res4[is.na(stat_bool)]$stat_bool <- 0
res4[is.na(data_bool)]$data_bool <- 0

# fix dates
# take a look. 
res4[published_print %like% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"]

# Fix them
res4[published_print %like% "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",published_print_ym := stri_sub(published_print, 1, 7)]
res4[published_print %like% "^[0-9]{4}-[0-9]{2}$",published_print_ym := paste0(published_print)]
res4[published_print %like% "^[0-9]{4}$",published_print_ym := paste0(published_print, "-01")]
res4[,published_print_ym_date_format := parse_date_time(published_print_ym, "ym")]
res4[,published_print_year := stri_sub(published_print_ym, 1, 4)]
res4[,published_print_month := stri_sub(published_print_ym, 6, 7)]

# Add polisci and IR bools and abbreviated name from JCR data and the abbrev package
dois_psir_bools <- fread("./output/all_109k_pubs_cleanish.csv")[,.(doi, ir_bool, ps_bool, abbrev_name)]
res4 <- dois_psir_bools[res4, on = c("doi" = "paper_doi")]

# Clean column names of the final output
setnames(res4, "dv_doi_bool", "od_bool")
setnames(res4, "dv_doi", "od_doi")

# Add and clean JPR replication data
jpr_data <- fread("./output/jpr_replication_data_matches.csv")[,.(doi, od_doi = url)]
jpr_data <- jpr_data %>% distinct(doi, .keep_all = T)
jpr_to_join <- res4[doi %in% jpr_data$doi & is.na(od_doi),.(doi)]
jpr_to_join <- jpr_data[jpr_to_join, on = "doi"]
res5 <- jpr_to_join[res4, on = "doi"]

res5[is.na(i.od_doi), i.od_doi := od_doi]
res5[is.na(od_doi), od_doi := i.od_doi]
res5$i.od_doi <- NULL
res5 <- res5 %>% distinct(doi, .keep_all = T)

# Make sure this is good. 
# res4[!is.na(od_doi)] %>% nrow 
# res5[!is.na(od_doi)] %>% nrow
# res5[!is.na(i.od_doi) & !is.na(od_doi),.(i.od_doi, od_doi)]
# res6 <- rbind(res5, res4[!is.na(od_doi)])

# Add and clean JCR replication data
jcr_data <- fread("./output/jcr_replication_data_matches.csv")[,doi := str_replace_all(doi, "_", "/")]
jcr_to_join <- res5[doi %in% jcr_data$doi & is.na(od_doi),.(doi)]
jcr_to_join <- jcr_data[jcr_to_join, on = "doi"]

res5 <- jcr_to_join[res5, on = "doi"]
res5[is.na(i.od_doi), i.od_doi := od_doi]
res5[is.na(od_doi), od_doi := i.od_doi]
res5[!is.na(i.od_doi)]
res5$i.od_doi <- NULL
res5[,od_bool := ifelse(is.na(od_doi), 0, 1)]

# test it. 
res5[,sum(od_bool)]

# Inspecting...... --------------------------------------------------------
# Deduplicate the data. Something odd happened at some stage and duplicate entries were created. No longer needed.
# res4_deduped <- res4 %>% distinct(doi, .keep_all = T)
# res4_deduped <- res4_deduped[,-c("i.ps_bool", "i.ir_bool")]

# Inspecting....
# res4[,.(sum(od_bool), sum(ir_bool), sum(ps_bool), sum(exp_bool), sum(data_bool), sum(stat_bool))]
# res4_deduped[,.(sum(od_bool), sum(ir_bool), sum(ps_bool), sum(exp_bool), sum(data_bool), sum(stat_bool))]

# Inspecting....
# res4_deduped[od_bool ==1]
# res4_deduped[!is.na(od_doi)]
# res4_deduped[!is.na(i.od_doi)]
# 
# Done inspecting...... --------------------------------------------------------

# Add in precarious data
pd <- fread("./output/papers_w_precarious_data.csv")[,.(doi, context)] %>% distinct(doi, .keep_all = T)
setnames(pd, "context", "pd_context")
res5 <- pd[res5, on = "doi"]
res5[, pd_bool := ifelse(is.na(pd_context), 0, 1)]

# Add DART verification results -------------------------------------------
dart_results <- fread("./output/dart_verification_od_bool_results.csv")[,doi := str_replace(doi, "_", "/")]

# Join it. 
res5[dart_results, od_bool := i.od_bool, on = "doi"][] # proper way of joining in data.table join
res5[dart_results, od_doi := i.od_doi, on = "doi"][] # proper way of joining in data.table join

# Check if any of these were stat_bool 0
res5[od_bool == 1 & stat_bool == 0 & doi %chin% dart_results$doi]

# validate the join
res5[doi %chin% dart_results$doi]
res5 %>% count(od_bool)

# Figure out Journal of Conflict Resolution, which like JPR has its own data practices
res5[pd_bool == 1 & journal_name %like% "Conflict"]
res5[journal_name %like% "Conflict Resolution"] %>% nrow
res5[journal_name %like% "Conflict Resolution" & stat_bool == 1] %>% nrow
res5[journal_name %like% "Conflict Resolution" & stat_bool == 1 & od_bool == 1] %>% nrow
res5[journal_name %like% "Conflict Resolution" & stat_bool == 1 & pd_bool == 1] %>% nrow

# Final is clean, export. 
res5 %>% fwrite("./output/109k_papers_all_coded_for_pub1.csv")


# Ad hoc inspecting -------------------------------------------------------
# res4 %>% View()
# res4[journal_name %like% "Political", .(journal_name, abbrev_name)] %>% distinct(journal_name, .keep_all = T)
# res4 %>% select(data_bool) %>% sample_n(100)



