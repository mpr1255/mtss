library(tidyverse)
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

# OD just for journals that are DART signatories over time ---

data_dart <- data

CrossTable(data_dart$journal_name, data_dart$dart_bool, 
           prop.chisq = FALSE, missing.include = FALSE)

## AJPS
data_dart <- filter(data_dart, journal_name == "American Journal Of Political Science")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

AJPS <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) +
  labs(title = "AJPS",
       x = "", y = "Percent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))



## APSR
data_dart <- data

data_dart <- filter(data_dart, journal_name == "American Political Science Review")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

APSR <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "APSR",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## BJPS
data_dart <- data

data_dart <- filter(data_dart, journal_name == "British Journal Of Political Science")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

BJPS <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "BJPS",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## CPS
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Comparative Political Studies")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

CPS <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "CPS",
       x = "", y = "Percent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Conflict Management and Peace Science
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Conflict Management And Peace Science")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

CMPS <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "CMPS",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## European Journal of Political Research
data_dart <- data

data_dart <- filter(data_dart, journal_name == "European Journal Of Political Research")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

EJPR <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "EJPR",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## European Union Politics 
data_dart <- data

data_dart <- filter(data_dart, journal_name == "European Union Politics")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

EUP <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "EUP",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## International Interactions
data_dart <- data

data_dart <- filter(data_dart, journal_name == "International Interactions")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

II <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "II",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Journal of Conflict Resolution
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Journal Of Conflict Resolution")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

JCR <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "JCR",
       x = "", y = "Percent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Journal of European Public Policy 
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Journal Of European Public Policy")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

JEPP <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "JEPP",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Journal of Peace Research 
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Journal Of Peace Research")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

JPR <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "JPR",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Party Politics
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Party Politics")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

PP <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "PP",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Political Analysis 
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Political Analysis")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

PA <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "PA",
       x = "", y = "Percent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Political Behavior
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Political Behavior")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

PB <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "PB",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Political Science Research and Methods
data_dart <- data

data_dart <- filter(data_dart, journal_name == "Political Science Research And Methods")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

PSRM <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) + 
  labs(title = "PSRM",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## The Journal of Politics
data_dart <- data

data_dart <- filter(data_dart, journal_name == "The Journal Of Politics")

dart_time <- CrossTable(data_dart$published_print_year, data_dart$od_stat_bool, 
                        prop.chisq = FALSE, missing.include = FALSE)

dart_time <- data.table(dart_time$prop.row)

colnames(dart_time) <- c("year", "od_bool", "percent")

dart_time <-  filter(dart_time, od_bool == "Open Data")

dart_time$percent <- c(dart_time$percent*100)

JoP <- ggplot(dart_time, aes(x=year, y=percent, group = 1)) + geom_line() +
  geom_vline(data=dart_time, aes(xintercept = c("2016")), color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0,100)) +
  
  labs(title = "JoP",
       x = "", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

## Graph all papers
grid <- grid.arrange(AJPS, APSR, BJPS, CMPS, CPS, EJPR, EUP, II, JCR,
             JEPP, JoP, JPR, PA, PB, PP, PSRM, nrow = 4)

ggsave("dart_grid.png",
       plot = grid,
       width = 10,
       height = 7,
       limitsize = FALSE)



## DART signatories 
### Total SI papers with OD in DART journals

dart_time_all <- data

dart_time_all <- dart_time_all %>% 
  filter(dart_time_all$dart_year == 2016)

dart_time_all_od_stat <- CrossTable(dart_time_all$journal_name, dart_time_all$od_stat_bool, 
                            prop.chisq = FALSE, missing.include = FALSE)

### Total papers per journal before and after

dart_time_all <- CrossTable(data_dart$journal_name, data_dart$dart_bool, 
                            prop.chisq = FALSE, missing.include = FALSE)

dart_time_all <- data.table(dart_time_all$t)

colnames(dart_time_all) <- c("year", "od_bool_0", "total_0")

dart_time_all_1 <-  filter(dart_time_all, od_bool_0 == 1)

colnames(dart_time_all_1) <- c("year", "od_bool_1", "total_1")

dart_time_all <-  filter(dart_time_all, od_bool_0 == 0)

dart_time_all <- merge.data.table(dart_time_all, dart_time_all_1)

dart_time_all <- subset(dart_time_all, select = -c(od_bool_0, od_bool_1))

dart_time_all <-  filter(dart_time_all, total_0 != 0)


