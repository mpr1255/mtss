library(tidyverse)
library(glue)
library(purrr)
library(gmodels)
library(data.table)

here <- rprojroot::find_rstudio_root_file()

data <- read.csv(glue("{here}/output/109k_papers_all_coded_for_pub1.csv"))

# Edit dataset variables
data <- data %>%
  mutate(
    od_stat_bool = ifelse(stat_bool != 1, NA, ifelse(od_bool == 1, "Open Data", "No Data")),
    od_data_bool = ifelse(data_bool != 1, NA, ifelse(od_bool == 1, 1, 0)),
    prereg_bool = ifelse(exp_bool != 1, NA, ifelse(prereg_score == 2, "Preregistered", "Not preregistered")),
    pd_bool = ifelse(stat_bool != 1, NA, pd_bool),
    journal_name = factor(journal_name)
  )

# OD just for journals that are DART signatories over time ---

data %>% 
  pull(journal_name) %>% 
  unique %>% 
  sort

dart_signed <- c("American Journal Of Political Science", "American Political Science Review", "British Journal Of Political Science", "Comparative Political Studies", "Conflict Management And Peace Science", "European Journal Of Political Research", "European Union Politics", "International Interactions", "Journal Of Conflict Resolution", "Journal Of European Public Policy", "Journal Of Peace Research", "The Journal Of Politics", "Party Politics", "Political Analysis", "Political Behavior", "Political Science Research And Methods")

data_dart <- data %>%
  group_by(journal_name) %>%
  mutate(dart_signed = if_else(journal_name %in% dart_signed, 1, 0)) %>%
  filter(dart_signed == 1)

# CrossTable(data_dart$journal_name, data_dart$od_stat_bool,  
#            prop.chisq = FALSE, missing.include = FALSE)

# Function to process data for each journal
process_journal_data <- function(data_filtered) {
  if (any(table(data_filtered$published_print_year, data_filtered$od_stat_bool) > 0)) {
    # Create a cross table of year and open data status
    dart_time <- CrossTable(data_filtered$published_print_year, data_filtered$od_stat_bool, 
                            prop.chisq = FALSE, missing.include = FALSE)
    
    # Convert the cross table output to a data frame and select open data rows
    dart_time <- data.table(dart_time$prop.row)
    colnames(dart_time) <- c("year", "od_bool", "percent")
    dart_time <- filter(dart_time, od_bool == "Open Data")
    
    # Convert the proportion to a percentage
    dart_time <- mutate(dart_time, percent = percent * 100)
    
    # Add the journal name to the dataframe
    dart_time$journal <- unique(data_filtered$abbrev_name)
    
    return(dart_time)
  } else {
    return(data.frame(year = integer(), od_bool = character(), percent = numeric(), journal = unique(data_filtered$abbrev_name)))
  }
}

# Main function to process all journals
process_all_journals <- function(data_dart) {
  # Split data by journal and process each one
  data_split_by_journal <- split(data_dart, data_dart$abbrev_name)
  
  # Map process_journal_data over each split and combine results
  final_data <- map_df(data_split_by_journal, process_journal_data)
  
  return(final_data)
}

final_data <- process_all_journals(data_dart)

final_data <- final_data %>%
  mutate(percent = as.numeric(percent))

grid <- ggplot(final_data, aes(x = year, y = percent)) +
  geom_vline(aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  geom_line(aes(group = journal), linewidth = 1, colour = "black") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_discrete(breaks = c(2010, 2015, 2020)) +
  facet_wrap(~journal, scales = c("fixed")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "", y = "Percent of papers with open data", title = "")

ggsave("graphs/dart_grid.png",
       plot = grid,
       width = 7,
       height = 5,
       limitsize = FALSE)