library(tidyverse)
library(gmodels)
library(gridExtra)
library(glue)
library(magrittr)
library(paletteer)

# Load data
here <- rprojroot::find_rstudio_root_file()
data <- read_csv(glue("{here}/output/109k_papers_all_coded_for_pub1.csv"))

# Open data by year ====

# Edit dataset variables
data <- data %>%
  mutate(
    od_stat_bool = ifelse(stat_bool != 1, NA, ifelse(od_bool == 1, "Open Data", "No Data")),
    od_data_bool = ifelse(data_bool != 1, NA, ifelse(od_bool == 1, 1, 0)),
    prereg_bool = ifelse(exp_bool != 1, NA, ifelse(prereg_score == 2, "Preregistered", "Not preregistered")),
    pd_bool = ifelse(stat_bool != 1, NA, pd_bool),
    journal_name = factor(journal_name)
  )

# Graph total stat_bool papers for each year
od_stat_summary <- with(data, CrossTable(published_print_year, od_stat_bool, prop.chisq = FALSE, missing.include = FALSE))

# Transforming CrossTable output using data.table and dplyr
stat_total <- as_tibble(od_stat_summary$t) %>%
  rename(year = x, od_bool = y, total = n) %>%
  filter(od_bool %in% c("Open Data", "No Data")) %>%
  pivot_wider(names_from = od_bool, values_from = total) %>%
  mutate(total_stat_bool = `Open Data` + `No Data`) %>%
  select(year, total_stat_bool)

# Graph for total statistical inference papers by year
ggplot(stat_total, aes(x = year, y = total_stat_bool, group = 1)) + 
  geom_bar(stat = "identity", fill = "grey") + 
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  labs(title = "Total statistical inference papers by year",
       x = "Year of publication", y = "Number of statistical inference papers") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

# Transform percentage data
od_percentage <- as_tibble(od_stat_summary$prop.row) %>%
  rename(year = x, od_bool = y, percent = n) %>%
  filter(od_bool == "Open Data") %>%
  mutate(percent = percent * 100)

# Graph for percentage of statistical inference papers with open data
ggplot(od_percentage, aes(x = year, y = percent, group = 1)) + 
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  labs(title = "Percentage of statistical inference papers with open data",
       x = "Year of publication", y = "Percent") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

# Combined plot
od_perc_graph <- ggplot(data = od_percentage, aes(x = year, y = percent, group = 1)) + 
  geom_bar(data = stat_total, aes(y = total_stat_bool / 40), stat = "identity", fill = "grey92", width = 1) +
  geom_line(linewidth = 1, colour = "black") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100),
                     sec.axis = sec_axis(~ . * 40, name = "Total number of papers"), expand = c(0, 0)) +
  scale_x_discrete(breaks = c(2010, 2015, 2020)) +
  labs(x = "", y = "Percent of papers with open data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Open data by journals ====

od_stat_journals <- with(data, CrossTable(abbrev_name, od_stat_bool, prop.chisq = FALSE, missing.include = FALSE))

# Working with the CrossTable output using tidyverse
od_stat_journals_df <- as_tibble(od_stat_journals$t) %>%
  rename(journal_name = x, od_bool = y, total = n)

# Filter journals that have no SI papers
od_stat_journals_zeros <- od_stat_journals_df %>%
  filter(od_bool == "No Data") %>%
  mutate(total = ifelse(total != 0, 1, 2)) %>%
  select(-od_bool)

## Calculate total statistical inference papers per journal
total_stat_by_journal <- as_tibble(CrossTable(data$abbrev_name, data$stat_bool, prop.chisq = FALSE, missing.include = FALSE)$t) %>%
  rename(journal_name = x, stat_bool = y, total_stat = n) %>%
  filter(stat_bool == 1) %>%
  select(-stat_bool)

# Merge and filter the datasets
merged_journal_data <- left_join(total_stat_by_journal, od_stat_journals_zeros, by = "journal_name") %>%
  filter(total != 2)

## Graph Open Data by journals
od_perc_journals <- as_tibble(od_stat_journals$prop.row) %>%
  rename(journal_name = x, od_bool = y, percent = n) %>%
  filter(od_bool == "Open Data")

# Merging with the filtered data from above
od_journal_filtered <- left_join(merged_journal_data, od_perc_journals, by = "journal_name") %>%
  mutate(percent = percent * 100)

# Generate the graph
od_journal_graph <- ggplot(od_journal_filtered, aes(x = reorder(journal_name, -percent), y = percent, group = 1)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100), expand = c(0, 0)) +
  geom_text(aes(label = total_stat), position = position_dodge(width = 0.9), color = "black", size = 2.25, vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 8)) +
  labs(title = "", x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Filter OD graphs by journal for papers with over 200 SI papers

od_journal_filtered_200 <- filter(od_journal_filtered, total_stat >=200)

od_journal_200_graph <- ggplot(od_journal_filtered_200, aes(x = reorder(journal_name, -percent), y=percent, group = 1)) + 
  geom_bar(stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_stat), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Preregistration by year ====

experiment_summary_total <- with(data, CrossTable(published_print_year, exp_bool, 
                                              prop.chisq = FALSE, missing.include = FALSE))

# Converting and filtering using dplyr
experiment_total <- as_tibble(experiment_summary_total$t) %>%
  rename(year = x, prereg = y, total = n) %>%
  filter(prereg == "1")

# Plotting total experimental papers
ggplot(experiment_total, aes(x = year, y = total, group = 1)) + 
  geom_bar(stat = "identity", fill = "grey") + 
  scale_y_continuous(breaks = seq(0, 500, by = 50), limits = c(0, 500)) + 
  labs(title = "Total experimental papers by year",
       x = "Year of publication", y = "Total number of papers") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Graph percentage of experiments that were preregistered
prereg_summary <- with(data, CrossTable(published_print_year, prereg_bool, 
                                        prop.chisq = FALSE, missing.include = FALSE))

# Converting CrossTable output to tibble and manipulating with dplyr
prereg_percentage <- as_tibble(prereg_summary$prop.row) %>%
  rename(year = x, prereg = y, percent = n) %>%
  filter(prereg == "Preregistered") %>%
  mutate(percent = percent * 100)

# Plotting percentage preregistration over time
ggplot(prereg_percentage, aes(x = year, y = percent, group = 1)) + 
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  labs(title = "Percentage of experiments that were preregistered",
       x = "Year of publication", y = "Percent") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine total experiment papers over time with percentage prereg over time

# Joining data for combined graph
prereg_year_combined <- left_join(prereg_percentage, experiment_total, by = "year")

# Plotting
prereg_perc_graph <- ggplot(prereg_year_combined, aes(x = year, y = percent, group = 1)) +
  geom_bar(aes(y = total / 5, group = 1), 
           stat = "identity", fill = "grey92", colour = "NA", width = 1) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     sec.axis = sec_axis(~ . * 5, name = "Total number of papers")) +
  geom_line(linewidth = 1, colour = "black") +
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

# Preregistration by journals ====

prereg_journals <- with(data, CrossTable(abbrev_name, prereg_bool, prop.chisq = FALSE, missing.include = FALSE))

# Working with CrossTable output using dplyr
prereg_journal_totals <- as_tibble(prereg_journals$t) %>%
  rename(journal_name = x, prereg = y, total = n)

# Create a summary of preregistration status by journal
prereg_summary_by_journal <- prereg_journal_totals %>%
  pivot_wider(names_from = prereg, values_from = total, values_fill = list(total = 0)) %>%
  rename(was_prereg = `Preregistered`, no_prereg = `Not preregistered`) %>%
  mutate(
    total_exp = was_prereg + no_prereg)

# Filter out journals with no experiments at all
prereg_summary_by_journal <- filter(prereg_summary_by_journal, total_exp != 0)

# Extract percentage of preregistration
prereg_perc_journals <- as_tibble(prereg_journals$prop.row) %>%
  rename(journal_name = x, prereg = y, percent = n) %>%
  filter(prereg == "Preregistered") %>%
  mutate(percent = percent * 100)

# Combine total experiments with percentage preregistered
prereg_journal_combined <- left_join(prereg_summary_by_journal, prereg_perc_journals, by = "journal_name")

# Plotting

prereg_journal_graph <- ggplot(prereg_journal_combined, aes(x = reorder(journal_name, -percent), y=percent, group = 1)) +
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 2.25,
    vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Include only journals with 20 or more experimental papers

prereg_journal_filtered <- filter(prereg_journal_combined, total_exp >=20)

prereg_journal_20_graph <- ggplot(prereg_journal_filtered, aes(x = reorder(journal_name, -percent), y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 8)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Precarious data by year ====

pd_stat_summary <- with(data, CrossTable(published_print_year, pd_bool, prop.chisq = FALSE, missing.include = FALSE))


pd_perc_graph <- as_tibble(pd_stat_summary$prop.row) %>%
  rename(year = x, pd_bool = y, percent = n) %>%
  filter(pd_bool == 1) %>%
  mutate(percent = percent * 100) %>% 
  ggplot(aes(x = year, y = percent, group = 1)) + 
  geom_line(linewidth = 1, colour = "black") +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  scale_x_discrete(breaks = c(2010, 2015, 2020)) +
  labs(x = "", y = "Percent of papers with \nprecarious data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Open data by journal in 2020 ====

# Filter for 2020
data_2020 <- data %>% filter(published_print_year == "2020")

# Edit journal titles that were not abbreviated
od_stat_journals_2020 <- with(data_2020, CrossTable(abbrev_name, od_stat_bool, 
                                                    prop.chisq = FALSE, missing.include = FALSE))

# Convert to data.table and rename columns
od_stat_journals_zeros_2020 <- as.data.table(od_stat_journals_2020$t)
colnames(od_stat_journals_zeros_2020) <- c("journal_name", "od_bool", "total")

# Remove journals that have no SI papers and adjust total
od_stat_journals_zeros_2020 <- od_stat_journals_zeros_2020 %>% 
  filter(od_bool == "No Data") %>% 
  mutate(total = ifelse(total != 0, 1, 2)) %>% 
  select(-od_bool)

# Create var with total stat_bool papers per journal
od_stat_journals_zeros_filtered_2020 <- CrossTable(data_2020$abbrev_name, data_2020$stat_bool, 
                                                   prop.chisq = FALSE, missing.include = FALSE)

od_stat_journals_zeros_filtered_2020 <- as.data.table(od_stat_journals_zeros_filtered_2020$t)
colnames(od_stat_journals_zeros_filtered_2020) <- c("journal_name", "stat_bool", "total_stat")

od_stat_journals_zeros_filtered_2020 <- od_stat_journals_zeros_filtered_2020 %>% 
  filter(stat_bool == 1) %>% 
  select(-stat_bool)

# Merge and filter
od_stat_journals_zeros_filtered_2020 <- od_stat_journals_zeros_filtered_2020 %>% 
  merge(od_stat_journals_zeros_2020, by = "journal_name") %>% 
  filter(total != 2)

# Graph OD by journals
od_perc_journals_2020 <- as.data.table(od_stat_journals_2020$prop.row)
colnames(od_perc_journals_2020) <- c("journal_name", "od_bool", "percent")

od_journal_filtered_2020 <- od_perc_journals_2020 %>% 
  filter(od_bool == "Open Data") %>% 
  merge(od_stat_journals_zeros_filtered_2020, by = "journal_name") %>% 
  mutate(percent = percent * 100)

# Plot
od_journal_graph_2020 <- ggplot(od_journal_filtered_2020, aes(x = reorder(journal_name, -percent), y = percent, group = 1)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100), expand = c(0, 0)) +
  geom_text(aes(label = total_stat), position = position_dodge(width = 0.9),
            colour = "black", size = 2.25, vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  labs(title = "", x = "", y = "Percent") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Filter OD graphs by journal for papers with over 20 SI papers

od_journal_filtered_2020_20 <- filter(od_journal_filtered_2020, total_stat >=20)

od_journal_graph_2020_20 <- ggplot(od_journal_filtered_2020_20, aes(x=reorder(journal_name, -percent), y=percent, group = 1)) + geom_bar(stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_stat), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Preregistration by journal in 2020 ====

# Edit journal titles that were not abbreviated
prereg_journals_2020 <- with(data_2020, CrossTable(abbrev_name, prereg_bool, 
                                                   prop.chisq = FALSE, missing.include = FALSE))

# Convert to data.table and rename columns
prereg_journal_zeros_2020 <- as.data.table(prereg_journals_2020$t)
colnames(prereg_journal_zeros_2020) <- c("journal_name", "prereg", "total")

# Create variable with total number of experiments by journal
prereg_journal_zeros_1_2020 <- prereg_journal_zeros_2020 %>% 
  filter(prereg == "Preregistered") %>% 
  rename(was_prereg = total) %>% 
  select(-prereg)

prereg_journal_zeros_2_2020 <- prereg_journal_zeros_2020 %>% 
  filter(prereg == "Not preregistered") %>% 
  rename(no_prereg = total) %>% 
  select(-prereg)

# Merge and calculate totals
prereg_journal_zeros_filtered_2020 <- merge(prereg_journal_zeros_1_2020, prereg_journal_zeros_2_2020, by = "journal_name")
prereg_journal_zeros_filtered_2020 <- prereg_journal_zeros_filtered_2020 %>% 
  mutate(total_exp = was_prereg + no_prereg,
         no_prereg = ifelse(no_prereg != 0, 1, 2))

# Graph
prereg_perc_journals_2020 <- as.data.table(prereg_journals_2020$prop.row)
colnames(prereg_perc_journals_2020) <- c("journal_name", "prereg", "percent")

prereg_journal_filtered_2020 <- prereg_perc_journals_2020 %>% 
  filter(prereg == "Preregistered") %>% 
  merge(prereg_journal_zeros_filtered_2020, by = "journal_name") %>% 
  filter(no_prereg != 2) %>% 
  mutate(percent = percent * 100)

# Plot
prereg_journal_graph_2020 <- ggplot(prereg_journal_filtered_2020, aes(x = reorder(journal_name, -percent), y = percent, group = 1)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100), expand = c(0, 0)) +
  geom_text(aes(label = total_exp), position = position_dodge(width = 0.9),
            colour = "black", size = 2.25, vjust = -0.25) +
  theme_bw() +
  labs(title = "", x = "", y = "Percent") +
  theme(axis.text.x = element_text(angle = 55, hjust=1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Include only journals with 20 or more experimental papers
prereg_journal_filtered_2020_20 <- filter(prereg_journal_filtered_2020, total_exp >=5)

prereg_journal_graph_2020_20 <- ggplot(prereg_journal_filtered_2020_20, aes(x=reorder(journal_name, -percent), y=percent, group = 1)) + 
  geom_bar(position = 'dodge', stat="identity") + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0, 0)) +
  geom_text(
    aes(label = total_exp), position=position_dodge(width=0.9),
    colour = "black", size = 3,
    vjust = -0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust=1)) +
  labs(title = "",
       x = "", y = "Percent") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Save graphs ====

ggsave("graphs/od_time.png",
       plot = od_perc_graph,
       width = 5,
       height = 4,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/prereg_time.png",
       plot = prereg_perc_graph,
       width = 5,
       height = 4,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/od_journal.png",
       plot = od_journal_200_graph,
       width = 9,
       height = 6,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/prereg_journal.png",
       plot = prereg_journal_20_graph,
       width = 6,
       height = 4,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/od_journal_all.png",
       plot = od_journal_graph,
       width = 24,
       height = 12,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/prereg_journal_all.png",
       plot = prereg_journal_graph,
       width = 18,
       height = 9,
       limitsize = FALSE,
       dpi = 900)

ggsave("graphs/pd_time.png",
       plot = pd_perc_graph,
       width = 5,
       height = 4,
       limitsize = FALSE,
       dpi = 900)
