library(tidyverse)
library(rbenchmark)
library(gmodels)
library(data.table)
library(gridExtra)
library(glue)
library(magrittr)
library(paletteer)

here <- rprojroot::find_rstudio_root_file()

data <- read.csv(glue("{here}/output/109k_papers_all_coded_for_pub1.csv"))

# Edit dataset variables ----

## Edit variables for SI and DA 
data$od_stat_bool <- ifelse(data$stat_bool!=1, NA, 
                            ifelse(data$od_bool == 1, 1, 0))

data$od_stat_bool <- recode_factor(data$od_stat_bool, "1" = "Open Data",
                                   "0" = "No Data")

data$od_data_bool <- ifelse(data$data_bool!=1, NA, 
                            ifelse(data$od_bool == 1, 1, 0))

data$journal_name <- factor(data$journal_name)

## Edit variables for preregistration
data$prereg_bool <- ifelse(data$exp_bool !=1, NA,
                           ifelse(data$prereg_score == 2, 1, 0))

data$prereg_bool <- recode_factor(data$prereg_bool, "1" = "Preregistered",
                                  "0" = "Not preregistered")

# Graph stat OD over time ----
## Graph total stat_bool papers for each year
## Note just use stat_bool instead of od_stat_bool! 
od_stat_summary <- with(data, CrossTable(published_print_year, od_stat_bool, 
                                         prop.chisq = FALSE, missing.include = FALSE))

stat_total <- data.table(od_stat_summary$t)

colnames(stat_total) <- c("year", "od_bool", "total")

stat_total_od <- stat_total %>% 
  filter(stat_total$od_bool == "Open Data")

colnames(stat_total_od) <- c("year", "od_bool", "od_total")

stat_total_nood <- stat_total %>% 
  filter(stat_total$od_bool == "No Data")

colnames(stat_total_nood) <- c("year", "od_bool", "nood_total")

stat_total_od <- subset(stat_total_od, select = -c(od_bool))

stat_total_nood <- subset(stat_total_nood, select = -c(od_bool))

stat_total_merge <- merge.data.table(stat_total_od, stat_total_nood)

stat_total_merge$total_stat_bool <- c(stat_total_merge$od_total+stat_total_merge$nood_total)

ggplot(stat_total_merge, aes(x=year, y=total_stat_bool, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity", fill = "grey") + 
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0,3000)) + 
  labs(title = "Total statistical inference papers by year",
       x = "Year of publication", y = "Number of statistical inference papers") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Graph OD for stat_bool
od_stat_summary <- with(data, CrossTable(published_print_year, od_stat_bool, 
                                         prop.chisq = FALSE, missing.include = FALSE))

od_percentage <- data.table(od_stat_summary$prop.row)

colnames(od_percentage) <- c("year", "od_bool", "percent")

od_perc_filtered <- od_percentage %>% 
  filter(od_percentage$od_bool == "Open Data")

od_perc_filtered$percent <- c(od_perc_filtered$percent*100)
 
ggplot(od_perc_filtered, aes(x=year, y=percent, group = 1)) + geom_line() + 
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) + 
        labs(title = "Percentage of statistical inference papers with open data",
             x = "Year of publication", y = "Percent") +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))

## Merge stat_total_merge and od_perc_filtered

g1 <- ggplot(data=od_perc_filtered, aes(x=year, y=percent, group = 1)) + 
  geom_bar(data = stat_total_merge, aes(x=year, y=total_stat_bool/40, group = 1), 
           position = 'dodge', stat="identity", fill = "grey92", width = 1) +
  geom_line(linewidth = 1, colour = "black") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100),  
                     sec.axis = sec_axis(~ . *40, name = "Total number of \npapers"), expand = c(0, 0)) +
  scale_x_discrete(breaks = c(2010, 2015, 2020)) +
  labs(title = "",
       x = "", y = "Percent of papers \nwith open data", colour = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# ## Graph stat OD over time by field (PS, IR) 
# 
# ### PS
# od_stat_bool_ps <- data.table(data$published_print_year, data$od_stat_bool, data$ps_bool)
# 
# colnames(od_stat_bool_ps) <- c("year", "percent", "ps")
# 
# od_stat_bool_ps <- filter(od_stat_bool_ps, ps == 1)
# 
# od_stat_summary_ps <- with(od_stat_bool_ps, CrossTable(year, percent, 
#                                          prop.chisq = FALSE, missing.include = FALSE))
# 
# od_percentage_ps <- data.table(od_stat_summary_ps$prop.row)
# 
# colnames(od_percentage_ps) <- c("year", "od_bool", "percent")
# 
# od_perc_filtered_ps <- od_percentage_ps %>% 
#   filter(od_percentage_ps$od_bool == "Open Data") %>%
#   mutate(discipline = "Political Science")
# 
# od_perc_filtered_ps$percent <- c(od_perc_filtered_ps$percent*100)
# 
# ggplot(od_perc_filtered_ps, aes(x=year, y=percent, group = 1)) + geom_line() + 
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) + 
#   labs(title = "Percentage of statistical inference papers with open data in political science journals",
#        x = "Year of publication", y = "Percent") +
#   theme_light() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ### IR
# 
# od_stat_bool_ir <- data.table(data$published_print_year, data$od_stat_bool, data$ir_bool)
# 
# colnames(od_stat_bool_ir) <- c("year", "percent", "ir")
# 
# od_stat_bool_ir <- filter(od_stat_bool_ir, ir == 1)
# 
# od_stat_summary_ir <- with(od_stat_bool_ir, CrossTable(year, percent, 
#                                                        prop.chisq = FALSE, missing.include = FALSE))
# 
# od_percentage_ir <- data.table(od_stat_summary_ir$prop.row)
# 
# colnames(od_percentage_ir) <- c("year", "od_bool", "percent")
# 
# od_perc_filtered_ir <- od_percentage_ir %>% 
#   filter(od_percentage_ir$od_bool == "Open Data") %>%
#   mutate(discipline = "International Relations")
# 
# od_perc_filtered_ir$percent <- c(od_perc_filtered_ir$percent*100)
# 
# ggplot(od_perc_filtered_ir, aes(x=year, y=percent, group = 1)) + geom_line() + 
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) + 
#   labs(title = "Percentage of statistical inference papers with open data in international relations journals",
#        x = "Year of publication", y = "Percent") +
#   theme_light() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ### Merge PS and IR graphs (put label text on graph)
# 
# ggplot() +
#   geom_line(data=od_perc_filtered_ps, aes(x=year, y=percent, group = 1, colour = 'Political Science')) +
#   geom_line(data=od_perc_filtered_ir, aes(x=year, y=percent, group = 1, colour = 'International Relations')) +
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) +
#   labs(title = "Percentage of statistical inference papers with open data by field",
#        x = "Year of publication", y = "Percent", colour = "") +
#   theme_light() +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
# 
# ### Combined with stat_total_merge
# 
# #### Title: Open data in statistical inference papers
# #### Define label titles from quantmod
# 
# od_perc_ps_ir <- bind_rows(od_perc_filtered_ps, od_perc_filtered_ir) %>%
#   left_join(stat_total_merge, by = "year")
# 
# g1 <- ggplot(od_perc_ps_ir, aes(x = year, y = percent, 
#                                 group = discipline, shape = discipline, color = discipline)) +
#   geom_bar(aes(y=total_stat_bool/40, group = 1), 
#            position = 'dodge', stat="identity", fill = "grey92", colour = "NA") +
#   scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(0, 0),
#                      sec.axis = sec_axis(~ . *40, name = "Total Papers")) +
#   geom_line(size = 0.15) +
#   geom_point(size = 2) +
#   labs(title = "",
#        x = "", y = "Percent") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8),
#         legend.title = element_blank(),
#         legend.position = "bottom") +
#   scale_color_paletteer_d("ggthemes::calc")
# 
# g1a <- ggplot() + 
#   geom_bar(data = stat_total_merge, aes(x=year, y=total_stat_bool/30, group = 1), 
#            position = 'dodge', stat="identity", fill = "grey92") +
#   geom_line(data=od_perc_filtered_ps, aes(x=year, y=percent,
#                                           linetype = "Political science",
#                                           group = 1)) +
#   geom_point(data=od_perc_filtered_ps, aes(x=year, y=percent,
#                                             group = 1)) +
#   geom_line(data=od_perc_filtered_ir, aes(x=year, y=percent,
#                                           linetype = "International relations",
#                                           group = 1)) +
#   geom_point(data=od_perc_filtered_ir, aes(x=year, y=percent,
#                                            group = 1)) +
#   scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), expand = c(0, 0), 
#                      name = "Percentage with open data",
#                      sec.axis = sec_axis(~ . *30, name = "Total number of papers")) +
#   scale_color_manual(
#     values = c("Political science" = "black", "International relations" = "black")) +
#   scale_linetype_manual("",values=c("Political science"=1,"International relations"=2)) +
#   labs(title = "",
#        x = "Year of publication", y = "Percent", colour = "") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") 

####  geom_text(aes(x = "2021", y = 32, label = "PS")) +
####  geom_text(aes(x = "2021", y = 18, label = "IR")) +


# Explore OD by journals ----

## Edit journal titles that were not abbreviated

od_stat_journals <- with(data, CrossTable(abbrev_name, od_stat_bool, 
                                          prop.chisq = FALSE, missing.include = FALSE))

## Remove journals that have no SI papers

od_stat_journals_zeros <- data.table(od_stat_journals$t)

colnames(od_stat_journals_zeros) <- c("journal_name", "od_bool", "total")

od_stat_journals_zeros <- od_stat_journals_zeros %>% 
  filter(od_stat_journals_zeros$od_bool == "No Data")

od_stat_journals_zeros$total <- ifelse(od_stat_journals_zeros$total != 0, 1, 2)

od_stat_journals_zeros <- subset(od_stat_journals_zeros, select = -c(od_bool))

## Create var with total stat_bool papers per journal

od_stat_journals_zeros_filtered <- CrossTable(data$abbrev_name, data$stat_bool, 
           prop.chisq = FALSE, missing.include = FALSE)

od_stat_journals_zeros_filtered <- data.table(od_stat_journals_zeros_filtered$t)

colnames(od_stat_journals_zeros_filtered) <- c("journal_name", "od_bool", "total_stat")

od_stat_journals_zeros_filtered <- filter(od_stat_journals_zeros_filtered, od_bool==1)

od_stat_journals_zeros_filtered <- subset(od_stat_journals_zeros_filtered, select = -c(od_bool))

od_stat_journals_zeros_filtered <- merge.data.table(od_stat_journals_zeros_filtered, 
                                                    od_stat_journals_zeros)

od_stat_journals_zeros_filtered <- filter(od_stat_journals_zeros_filtered, total!=2)

## Graph OD by journals

od_perc_journals <- data.table(od_stat_journals$prop.row)

colnames(od_perc_journals) <- c("journal_name", "od_bool", "percent")

od_journal_filtered <- od_perc_journals %>% 
  filter(od_perc_journals$od_bool == "Open Data")

od_journal_filtered <- merge.data.table(od_journal_filtered, od_stat_journals_zeros_filtered, by = "journal_name")

od_journal_filtered$percent <- c(od_journal_filtered$percent*100)

g3 <- ggplot(od_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + geom_bar(stat="identity") + 
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
        geom_text(
          aes(label = total_stat), position=position_dodge(width=0.9),
          colour = "black", size = 2.25,
          vjust = -0.25) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
        labs(title = "",
            x = "", y = "Percent") +
        theme(plot.title = element_text(hjust = 0.5))

### Percentage of statistical inference papers with open data by journal (2010 - 2021)

## Crosstab od_stat_bool for each journal type

## Filter OD graphs by journal for papers with over 200 SI papers

od_journal_filtered <- filter(od_journal_filtered, total_stat >=200)

g4 <- ggplot(od_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + geom_bar(stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_stat), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

# Graph prereg over time ----

prereg_summary_total <- with(data, CrossTable(published_print_year, exp_bool, 
                                         prop.chisq = FALSE, missing.include = FALSE))

prereg_total <- data.table(prereg_summary_total$t)

colnames(prereg_total) <- c("year", "prereg", "total")

prereg_total <- prereg_total %>% 
  filter(prereg_total$prereg == "1")

ggplot(prereg_total, aes(x=year, y=total, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity", fill = "grey") + 
  scale_y_continuous(breaks = seq(0, 500, by = 50), limits = c(0,500)) + 
  labs(title = "Total experimental papers by year",
       x = "Year of publication", y = "Total number of papers") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Graph prereg for experiments

prereg_check <- with(data, CrossTable(prereg_score, exp_bool, 
                                        prop.chisq = FALSE, missing.include = FALSE))

prereg_summary <- with(data, CrossTable(published_print_year, prereg_bool, 
                                          prop.chisq = FALSE, missing.include = FALSE))

prereg_percentage <- data.table(prereg_summary$prop.row)

colnames(prereg_percentage) <- c("year", "prereg", "percent")

prereg_perc_filtered <- prereg_percentage %>% 
  filter(prereg_percentage$prereg == "Preregistered")

prereg_perc_filtered$percent <- c(prereg_perc_filtered$percent*100)

ggplot(prereg_perc_filtered, aes(x=year, y=percent, group = 1)) + geom_line() + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100)) + 
  labs(title = "Percentage of experiments that were preregistered",
       x = "Year of publication", y = "Percent") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Combine total experiment papers over time with percentage prereg over time

### Title: Preregistration of experiments

prereg_ps_ir <- prereg_perc_filtered %>%
  left_join(prereg_total, by = "year")

g2 <- ggplot(prereg_ps_ir, aes(x = year, y = percent, group = 1)) +
  geom_bar(aes(y=total/5, group = 1), 
           position = 'dodge', stat="identity", fill = "grey92", colour = "NA", width = 1) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     sec.axis = sec_axis(~ . *5, name = "Total number of \npapers")) +
  scale_x_discrete(breaks = c(2010, 2015, 2020)) +
  geom_line(linewidth = 1, colour = "black") +
  labs(title = "",
       x = "", y = "Percent of experiments \npreregistered") +
  theme_bw() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# g2a <- ggplot() + 
#   geom_bar(data = prereg_total, aes(x=year, y=total/5, group = 1), 
#            position = 'dodge', stat="identity", fill = "grey92") +
#   geom_line(data=prereg_perc_filtered, aes(x=year, y=percent, 
#                                           group = 1)) +
#   scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), expand = c(0, 0), 
#                      name = "Percentage preregistered",
#                      sec.axis = sec_axis(~ . *5, name = "Total number of papers")) +
#   labs(title = "",
#        x = "Year of publication", y = "Percent", colour = "") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") 

# Explore prereg by journals ----
prereg_journals <- with(data, CrossTable(abbrev_name, prereg_bool, 
                                          prop.chisq = FALSE, missing.include = FALSE))
## Define journals that have no experiments

prereg_journal_zeros <- data.table(prereg_journals$t)

colnames(prereg_journal_zeros) <- c("journal_name", "prereg", "total")

## Create variable with total number of experiments by journal

prereg_journal_zeros_1 <- prereg_journal_zeros %>% 
  filter(prereg_journal_zeros$prereg == "Preregistered")

colnames(prereg_journal_zeros_1) <- c("journal_name", "prereg", "was_prereg")

prereg_journal_zeros_1 <- subset(prereg_journal_zeros_1, select = -c(prereg))

prereg_journal_zeros_2 <- prereg_journal_zeros %>% 
  filter(prereg_journal_zeros$prereg == "Not preregistered")

colnames(prereg_journal_zeros_2) <- c("journal_name", "prereg", "no_prereg")

prereg_journal_zeros_2 <- subset(prereg_journal_zeros_2, select = -c(prereg))

prereg_journal_zeros_filtered <- merge.data.table(prereg_journal_zeros_1, prereg_journal_zeros_2)

prereg_journal_zeros_filtered$total_exp <- c(prereg_journal_zeros_filtered$was_prereg+prereg_journal_zeros_filtered$no_prereg)

prereg_journal_zeros_filtered$no_prereg <- ifelse(prereg_journal_zeros_filtered$no_prereg != 0, 1, 2)

## Graph 

prereg_perc_journals <- data.table(prereg_journals$prop.row)

colnames(prereg_perc_journals) <- c("journal_name", "prereg", "percent")

prereg_journal_filtered <- prereg_perc_journals %>% 
  filter(prereg_perc_journals$prereg == "Preregistered")

prereg_journal_filtered <- merge.data.table(prereg_journal_filtered, prereg_journal_zeros_filtered, by = "journal_name")

prereg_journal_filtered <- filter(prereg_journal_filtered, no_prereg!=2)

prereg_journal_filtered$percent <- c(prereg_journal_filtered$percent*100)

prereg_journal_filtered$total <- as.numeric(prereg_journal_filtered$total)

g5 <- ggplot(prereg_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 2.25,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

### Title: Percentage of experiments that were preregistered by journal (2010 - 2021)

### Include only journals with 20 or more experimental papers
prereg_journal_filtered <- filter(prereg_journal_filtered, total_exp >=20)

g6 <- ggplot(prereg_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

### Title: Percentage of experiments that were preregistered in journals with 20 or more experiments (2010 - 2021)

# Format final graphs ----

g1 
g2 
g3
g4
g5
g6

ggsave("graphs/od_time.png",
       plot = g1,
       width = 5,
       height = 4,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/prereg_time.png",
       plot = g2,
       width = 5,
       height = 4,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/od_journal.png",
       plot = g4,
       width = 9,
       height = 6,
       limitsize = FALSE)

ggsave("graphs/prereg_journal.png",
       plot = g6,
       width = 9,
       height = 6,
       limitsize = FALSE)

ggsave("graphs/od_journal_all.png",
       plot = g3,
       width = 24,
       height = 12,
       limitsize = FALSE)

ggsave("graphs/prereg_journal_all.png",
       plot = g5,
       width = 18,
       height = 9,
       limitsize = FALSE)

# Explore OD by journals in 2020 ----

## Filter for 2020

data <- filter(data, published_print_year == "2020") 
            
## Edit journal titles that were not abbreviated

od_stat_journals <- with(data, CrossTable(abbrev_name, od_stat_bool, 
                                          prop.chisq = FALSE, missing.include = FALSE))

## Remove journals that have no SI papers

od_stat_journals_zeros <- data.table(od_stat_journals$t)

colnames(od_stat_journals_zeros) <- c("journal_name", "od_bool", "total")

od_stat_journals_zeros <- od_stat_journals_zeros %>% 
  filter(od_stat_journals_zeros$od_bool == "No Data")

od_stat_journals_zeros$total <- ifelse(od_stat_journals_zeros$total != 0, 1, 2)

od_stat_journals_zeros <- subset(od_stat_journals_zeros, select = -c(od_bool))

## Create var with total stat_bool papers per journal

od_stat_journals_zeros_filtered <- CrossTable(data$abbrev_name, data$stat_bool, 
                                              prop.chisq = FALSE, missing.include = FALSE)

od_stat_journals_zeros_filtered <- data.table(od_stat_journals_zeros_filtered$t)

colnames(od_stat_journals_zeros_filtered) <- c("journal_name", "od_bool", "total_stat")

od_stat_journals_zeros_filtered <- filter(od_stat_journals_zeros_filtered, od_bool==1)

od_stat_journals_zeros_filtered <- subset(od_stat_journals_zeros_filtered, select = -c(od_bool))

od_stat_journals_zeros_filtered <- merge.data.table(od_stat_journals_zeros_filtered, 
                                                    od_stat_journals_zeros)

od_stat_journals_zeros_filtered <- filter(od_stat_journals_zeros_filtered, total!=2)

## Graph OD by journals

od_perc_journals <- data.table(od_stat_journals$prop.row)

colnames(od_perc_journals) <- c("journal_name", "od_bool", "percent")

od_journal_filtered <- od_perc_journals %>% 
  filter(od_perc_journals$od_bool == "Open Data")

od_journal_filtered <- merge.data.table(od_journal_filtered, od_stat_journals_zeros_filtered, by = "journal_name")

od_journal_filtered$percent <- c(od_journal_filtered$percent*100)

ga3 <- ggplot(od_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + geom_bar(stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_stat), position=position_dodge(width=0.9),
    colour = "black", size = 2.25,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

### Percentage of statistical inference papers with open data by journal (2010 - 2021)

## Crosstab od_stat_bool for each journal type

## Filter OD graphs by journal for papers with over 200 or 300 SI papers

od_journal_filtered <- filter(od_journal_filtered, total_stat >=20)

ga4 <- ggplot(od_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + geom_bar(stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_stat), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

# Explore prereg by journals in 2020----

## Filter for 2020

data <- filter(data, published_print_year == "2020") 

prereg_journals <- with(data, CrossTable(abbrev_name, prereg_bool, 
                                         prop.chisq = FALSE, missing.include = FALSE))
## Define journals that have no experiments

prereg_journal_zeros <- data.table(prereg_journals$t)

colnames(prereg_journal_zeros) <- c("journal_name", "prereg", "total")

## Create variable with total number of experiments by journal

prereg_journal_zeros_1 <- prereg_journal_zeros %>% 
  filter(prereg_journal_zeros$prereg == "Preregistered")

colnames(prereg_journal_zeros_1) <- c("journal_name", "prereg", "was_prereg")

prereg_journal_zeros_1 <- subset(prereg_journal_zeros_1, select = -c(prereg))

prereg_journal_zeros_2 <- prereg_journal_zeros %>% 
  filter(prereg_journal_zeros$prereg == "Not preregistered")

colnames(prereg_journal_zeros_2) <- c("journal_name", "prereg", "no_prereg")

prereg_journal_zeros_2 <- subset(prereg_journal_zeros_2, select = -c(prereg))

prereg_journal_zeros_filtered <- merge.data.table(prereg_journal_zeros_1, prereg_journal_zeros_2)

prereg_journal_zeros_filtered$total_exp <- c(prereg_journal_zeros_filtered$was_prereg+prereg_journal_zeros_filtered$no_prereg)

prereg_journal_zeros_filtered$no_prereg <- ifelse(prereg_journal_zeros_filtered$no_prereg != 0, 1, 2)

## Graph 

prereg_perc_journals <- data.table(prereg_journals$prop.row)

colnames(prereg_perc_journals) <- c("journal_name", "prereg", "percent")

prereg_journal_filtered <- prereg_perc_journals %>% 
  filter(prereg_perc_journals$prereg == "Preregistered")

prereg_journal_filtered <- merge.data.table(prereg_journal_filtered, prereg_journal_zeros_filtered, by = "journal_name")

prereg_journal_filtered <- filter(prereg_journal_filtered, no_prereg!=2)

prereg_journal_filtered$percent <- c(prereg_journal_filtered$percent*100)

prereg_journal_filtered$total <- as.numeric(prereg_journal_filtered$total)

ga5 <- ggplot(prereg_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 2.25,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

### Title: Percentage of experiments that were preregistered by journal (2010 - 2021)

### Include only journals with 20 or more experimental papers
prereg_journal_filtered <- filter(prereg_journal_filtered, total_exp >=5)

ga6 <- ggplot(prereg_journal_filtered, aes(x=journal_name, y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

### Title: Percentage of experiments that were preregistered in journals with 20 or more experiments (2010 - 2021)

ggsave("graphs/od_journal_2020.png",
       plot = ga4,
       width = 9,
       height = 7,
       limitsize = FALSE)

ggsave("graphs/prereg_journal_2020.png",
       plot = ga6,
       width = 9,
       height = 6.5,
       limitsize = FALSE)
