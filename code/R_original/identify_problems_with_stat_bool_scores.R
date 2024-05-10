###################################################################################################################
#
# What this code does: Recall that the SVM model had a ~90% classification accuracy. So if it categorised ~25,000 
# papers as stat_bool, then roughly 2.5k of them are going to be misclassified, either false positives or 
# false negatives. We appear to have found ~400 false negatives -- discovered because they also had a 
# Dataverse dataset. The existence of a Dataverse dataset is by itself a very strong indication that the paper is 
# stat_bool or at least data_bool. This code discovers those potentially misclassified papers, looks at the files that 
# are deposited in those datasets, looks at the SCORES that those papers received, and ultimately reclassifies most of 
# them as stat_bool. Thus, we're overwriting the ML model because we think it's obvious that it misclassified a bunch 
# of papers. Given that we are essentially using computational and statistical learning methods for inductive 
# classification -- NOT hypothesis testing -- there is no problem. 
#
###################################################################################################################

source("./code/R/libs.R")
source("./code/R/func_get_quant_terms.R")

all_files <- fread("./output/all_papers_clean.csv")
all_dv_links <- fread("./output/all_dv_doi_paper_doi_matches_w_orphans.csv")
all_dv_links <- all_dv_links %>% distinct(dv_doi, .keep_all = T)

res1 <- all_dv_links[all_files, on = c("paper_doi" = "doi")]

# res1[!is.na(dv_doi)]

stat_bool_scores <-  fread("./output/production_stat_bool_prediction1_20210909.csv")
stat_scores_to_match <- stat_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), stat_bool = test_pred)]
res2 <- stat_scores_to_match[res1, on = "paper_doi"]

data_bool_scores <- fread("./output/production_data_bool_prediction1_20210909.csv")
data_scores_to_match <- data_bool_scores[,.(paper_doi = str_replace_all(file_in, "_", "/"), data_bool = pred_production)]

res3 <- data_scores_to_match[res2, on = "paper_doi"]
res3[,dv_doi_bool := ifelse(str_detect(dv_doi, "10.7910"), 1, 0)]
res3[,paper_doi := str_replace_all(paper_doi, "/", "_")]

# res3[!is.na(dv_doi)]

dv_1_but_stat_data_0 <- res3[dv_doi_bool == 1 & stat_bool == 0 & data_bool == 0]

# figure_out_400 <- map_df(dv_1_but_stat_data_0$paper_doi, ~func_get_count(.x))
# figure_out_400 %>% fwrite("./output/442_potentially_misclassified_stat_bool_papers_w_dv_doi.csv")

figure_out_400 <- fread("./output/442_potentially_misclassified_stat_bool_papers_w_dv_doi.csv")

# Doing this a couple of times and simply looking at the files indicates that these included data and code
dv_1_but_stat_data_0 %>% select(dv_doi) %>% mutate(dv_doi = paste0("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:", dv_doi)) %>% sample_n(100)

# how about those with a very low stat_count_minus_ast score? Indeed these don't apear to be statistics papers.
figure_out_400[stat_count_minus_ast < 10] %>% select(file_in, title, stat_count_minus_ast, game_formal)

# how about those with a higher stat_count_minus_ast score? These all appear to be stat papers. So we'll code everything > 10 as stat_bool 1. 
figure_out_400[title %notlike% "[Ee]rratum|[Cc]orrigendum"][stat_count_minus_ast > 10 & stat_count_minus_ast < 30] %>% select(file_in, title, stat_count_minus_ast, game_formal)


figure_out_400[title %notlike% "[Ee]rratum|[Cc]orrigendum" & stat_count_minus_ast > 10, stat_bool := 1]

figure_out_400[stat_bool == 1,.(file_in, stat_bool)] %>% fwrite("./output/recoded_misclassified_ml_papers_from_443.csv")












test4 <- figure_out_400 %>% arrange(desc(stat_count_minus_ast)) %>% select(file_in, title, journal, data_count, stat_count, stat_count_minus_ast, game_formal) 





test4 %>% print(100)



res3[paper_doi %in% "10.1017_s0007123417000096"]

res3[paper_doi == ]


res3[title %like% "The Representation Gap and Political Sophistication"]











