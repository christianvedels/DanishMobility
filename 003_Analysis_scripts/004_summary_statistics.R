# Summary statistics of various data sources
# Date updated:   2024-04-16
# Author:         MHK
#
# Purpose:        Produce summary statistics describing the data sources used to estimate social mobility

# ==== Libraries ====
library(tidyverse)
library(readxl)
library(ggplot2)

# ==== Load data ====
census = read.csv("../Data/population/census_count.csv")
cemetery = read.csv("../Data/population/cemetery/cemetery_count.csv")
dst = read.csv("../Data/population/pop2002_2023/dst_count.csv")
census_pre1820_ses = read.csv("../Data/population/census_pre1820_count_ses.csv")
dld = readRDS("../Data/elite/DLD/member1849-2022.rds")
manor = read.csv("../Data/elite/manor_owners_cleaned.csv")
phd = read_xlsx("/Users/mhkr/Downloads/dki-export-94266.xlsx")


# creating population distribution of surnames
pop_dist = bind_rows(census,
                     cemetery %>% filter(year>=1902 & year<=2001),
                     dst
)

sen_datter_plot = pop_dist %>% 
  filter(!is.na(year) & (year>=1700 & year<=2023)) %>% 
  group_by(year) %>% 
  summarize(n_sen_datter = sum(n[grepl("sen$|datter$", surname)]),
            n = sum(n)) %>% 
  ungroup() %>% 
  mutate(frac_sen_datter = n_sen_datter/n) %>% 
  ggplot(aes(x = year, y = frac_sen_datter, group = 1)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1901, linetype = 2) +
  geom_vline(xintercept = 2001, linetype = 2) +
  annotate("text", x = 1800, y = 0.75, label = "Census data", size = 5, color = "black") +
  annotate("text", x = 1950, y = 0.75, label = "Cemetery data", size = 5, color = "black") +
  annotate("text", x = 2020, y = 0.75, label = "DST", size = 5, color = "black") +
  theme_linedraw() + 
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "Year", y = 'Fraction of ..sen / ..datter surnames') 

ggsave(sen_datter_plot, file = "../DanishMobility/Project_dissemination/DanishMobility_slides/figures/sen_datter_plot.png")

# defining time periods to break variables into
breaks <- c(1700, 1730, 1760, 1790, 1820, 1850, 1880, 1910, 1940, 1970, 2000, 2023)
labels <- c("1700-1730", "1731-1760", "1761-1790", "1791-1820", "1821-1850", "1851-1880", "1881-1910", "1911-1940", "1941-1970", "1971-2000", "2001-2023")

pop_dist = pop_dist %>%
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE))

# examples of surnames in different pre-1820 HISCAM groupings
examples = cbind(
census_pre1820_ses %>% filter(period == "1787-1820" & group == "80-99") %>% arrange(desc(nSES)) %>% slice_head(n = 10) %>% select(surname), 
census_pre1820_ses %>% filter(period == "1787-1820" & group == "60-79") %>% arrange(desc(nSES)) %>% slice_head(n = 10) %>% select(surname),
census_pre1820_ses %>% filter(period == "1787-1820" & group == "40-59") %>% arrange(desc(nSES)) %>% slice_head(n = 10) %>% select(surname)
) %>% as.data.frame() %>% 
  setNames(c("80-99", "60-79", "40-59"))

## ==== summary of sources used for elite groups ====

manor = manor %>% pivot_longer(cols = starts_with("surname"),  # Selects all columns that start with 'surname'
                                       names_to = "surname_variable",  # New column for the names of the original columns
                                       values_to = "surname") %>% 
  filter(!is.na(surname)) %>% 
  mutate(year = ifelse(is.na(yearFrom), yearTo, yearFrom),
         period = cut(year, breaks = breaks, labels = labels, include.lowest = T),
         surname = tolower(surname))

summary_statistics_elite = rbind(
cbind("Source", "N", "Start", "End"),
cbind("DLD", nrow(dld), min(dld$bYear, na.rm = T), max(dld$bYear, na.rm = T)),
cbind("Owners of manor estates", nrow(manor), min(manor$yearFrom, na.rm = T), max(manor$yearTo, na.rm = T)),
cbind("PhD graduates", nrow(phd), min(phd$`Submission Year`, na.rm = T), max(phd$`Submission Year`, na.rm = T))
)

