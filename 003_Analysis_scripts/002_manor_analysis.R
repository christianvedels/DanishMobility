# Preliminary analysis using owners of manor estates as elite occupation
# Date updated:   2024-04-15
# Author:         MHK
#
# Purpose:        To produce preliminary results depicting how the relative representation of surnames have changed over time.

# ==== Libraries ====
library(tidyverse)
library(readxl)
library(ggplot2)
library(fixest)

# ==== Load data ====
census = read.csv("../Data/population/census_count.csv")
cemetery = read.csv("../Data/population/cemetery/cemetery_count.csv")
dst = read.csv("../Data/population/pop2002_2023/dst_count.csv")
nobility = read.csv("../Data/elite/nobility.csv")
manor = read.csv("../Data/elite/manor_owners_cleaned.csv")
census_pre1820_ses = read.csv("../Data/population/census_pre1820_count_ses.csv")

# creating a single data frame which contains distribution of surnames
pop_dist = bind_rows(census, # from 1787 to 1901: census dictates the distribution
                     cemetery %>% filter(year>=1902 & year<=2001),  # from 1902 to 2001: cemetery
                     dst # from 2002 to 2023: DST
)

pop_dist = pop_dist %>% filter(year != 1885) # remove data from 1885 census as it only covers CPH

# ==== Data cleaning ====
breaks <- c(1700, 1730, 1760, 1790, 1820, 1850, 1880, 1910, 1940, 1970, 2000, 2023)
labels <- c("1700-1730", "1731-1760", "1761-1790", "1791-1820", "1821-1850", "1851-1880", "1881-1910", "1911-1940", "1941-1970", "1971-2000", "2001-2023")

nobility$nobility = 1 # creating nobility indicator

# ==== Analysis by nobility status ====

# create population distribution of surnames by nobility status
pop_dist_nob = pop_dist %>%
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  group_by() %>% 
  left_join(nobility %>% distinct(noble_family, nobility), by = c("surname" = "noble_family")) %>% 
  group_by(nobility, period) %>%
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(nobility = ifelse(is.na(nobility), 0, 1))

# calculate fraction of observations with surname i in period t
pop_dist_nob = pop_dist_nob %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

# create data set of surnames among owners of manor estates
manor = manor %>% pivot_longer(cols = starts_with("surname"),  # Selects all columns that start with 'surname'
                               names_to = "surname_variable",  # New column for the names of the original columns
                               values_to = "surname") %>% 
  filter(!is.na(surname)) %>% 
  mutate(year = ifelse(is.na(yearFrom), yearTo, yearFrom),
         period = cut(year, breaks = breaks, labels = labels, include.lowest = T),
         surname = tolower(surname)) %>% 
  filter(!is.na(period))

# merge nobility status into manor owners data frame and count instances of surname i in period t among manor owners
manor = manor %>% 
  left_join(nobility %>% distinct(noble_family, nobility), by = c("surname" = "noble_family")) %>% 
  mutate(nobility = ifelse(is.na(nobility), 0, 1)) %>% 
  group_by(period, nobility) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(period, nobility, n) %>% 
  distinct()

# compute fraction of manor owners with surname i in period t
manor = manor %>% group_by(period) %>% mutate(frac = n/sum(n))

# merge manor data frame into population distribution of surnames, and calculate relative representation by nobility status
pop_dist_manor = pop_dist_nob %>% 
  left_join(manor, by = c("period" = "period",
                          "nobility" = "nobility"), 
            suffix = c("", "_elite")) %>% 
  mutate(rr = frac_elite/frac)

# plot relative representation by period
manor_rr_by_nobility = pop_dist_manor %>% 
  filter(!is.na(rr) & nobility == 1) %>% 
  ggplot(aes(x = period, y = rr, group = nobility, color = as.factor(nobility))) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = c("1" = "blue", "0" = "red"),
                     name = "Nobility",
                     labels = c("1" = "Yes", "0" = "No")) +
  labs(x = "Period",
       y = "Relative representation") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_log10()

ggsave(manor_rr_by_nobility, file = "../DanishMobility/Project_dissemination/DanishMobility_slides/figures/manor_rr_by_nobility.png")

# to estimate regression, use only noble surnames (correct?)
pop_dist_manor_nob = pop_dist_manor %>% 
  filter(nobility == 1) %>% 
  mutate(rr_lag = lag(rr))

lm(log(rr) ~ log(rr_lag), data = pop_dist_manor_nob) %>% summary()

lm(log(rr) ~ log(rr_lag), data = pop_dist_manor_nob, weights = n) %>% summary()


# ==== Analysis by nobility type ====
manor = read.csv("../Data/elite/manor_owners_cleaned.csv")

# create population distribution of surnames by nobility status
pop_dist_nob_type = pop_dist %>%
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  group_by() %>% 
  left_join(nobility, by = c("surname" = "noble_family")) %>% 
  group_by(type, period) %>%
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(type = ifelse(is.na(type), 0, type))

# calculate fraction of observations with surname i in period t
pop_dist_nob_type = pop_dist_nob_type %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

# create data set of surnames among owners of manor estates
manor = manor %>% pivot_longer(cols = starts_with("surname"),  # Selects all columns that start with 'surname'
                               names_to = "surname_variable",  # New column for the names of the original columns
                               values_to = "surname") %>% 
  filter(!is.na(surname)) %>% 
  mutate(year = ifelse(is.na(yearFrom), yearTo, yearFrom),
         period = cut(year, breaks = breaks, labels = labels, include.lowest = T),
         surname = tolower(surname)) %>% 
  filter(!is.na(period))

# merge nobility status into manor owners data frame and count instances of surname i in period t among manor owners
manor = manor %>% 
  left_join(nobility, by = c("surname" = "noble_family")) %>% 
  mutate(nobility = ifelse(is.na(nobility), 0, 1)) %>% 
  group_by(period, type) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(period, type, n) %>% 
  distinct() %>% 
  mutate(type = ifelse(is.na(type), 0, type))

# compute fraction of manor owners with surname i in period t
manor = manor %>% group_by(period) %>% mutate(frac = n/sum(n))

# merge manor data frame into population distribution of surnames, and calculate relative representation by nobility status
pop_dist_manor_type = pop_dist_nob_type %>% 
  left_join(manor, by = c("period" = "period",
                          "type" = "type"), 
            suffix = c("", "_elite")) %>% 
  mutate(rr = frac_elite/frac)

# plot relative representation by period
ggplot(pop_dist_manor_type, aes(x = period, y = rr, group = type, color = as.factor(type))) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank()) +
  labs(x = "Period",
       y = "Relative representation") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_log10()

# to estimate regression, use only noble surnames (correct?)
pop_dist_manor_nob_type = pop_dist_manor_type %>% 
  group_by(type)  %>% 
  mutate(rr_lag = lag(rr))

summary(lm(log(rr) ~ log(rr_lag), data = pop_dist_manor_nob_type))

## ==== Cross-sectional summary ====

pop_dist_manor_type %>% 
  mutate(
    type = case_when(type == "Brev- og højadel" ~ "Ennobled/higher nobility", 
                     type == "Brev- og lavadel" ~ "Ennobled/lower nobility", 
                     type == "Brevadel" ~ "Ennobled nobility",
                     type == "Ur- og højadel" ~ "Ancient/lower nobility",
                     TRUE ~ type)) %>% 
  filter(period == "2001-2023" & !is.na(rr) & type!=0) %>% 
  ggplot(aes(x = reorder(type, -rr), y = rr, fill = type)) + 
  geom_bar(stat = "identity") +
  labs(x = "Type", y = "Relative representation") +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Ennobled/higher nobility" = "red", 
                               "Ennobled/lower nobility" = "darkgreen", 
                               "Ennobled nobility" = "steelblue", 
                               "Ancient/lower nobility" = "pink"),
                    name = "Nobility") 

# ==== Analysis by avg. HISCAM score pre-1820 ====

pop_dist_ses = pop_dist %>% 
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  right_join(census_pre1820_ses %>% 
              filter(period == "1787-1820") %>% 
              select(surname, group), by = "surname") %>% 
  group_by(period, group) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

manor = read.csv("../Data/elite/manor_owners_cleaned.csv")

manor = manor %>% pivot_longer(cols = starts_with("surname"),  # Selects all columns that start with 'surname'
                               names_to = "surname_variable",  # New column for the names of the original columns
                               values_to = "surname") %>% 
  filter(!is.na(surname)) %>% 
  mutate(year = ifelse(is.na(yearFrom), yearTo, yearFrom),
         period = cut(year, breaks = breaks, labels = labels, include.lowest = T),
         surname = tolower(surname)) %>% 
  filter(!is.na(period))

# merge pre-1820 avg. HISCAM score into manor df
manor = manor %>% 
  right_join(census_pre1820_ses %>% filter(period == "1787-1820") %>% select(surname, group), by = "surname") %>% 
  group_by(period, group) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n))

# merge manor df into pop. dist
pop_dist_ses = pop_dist_ses %>% 
  left_join(manor, by = c("period", "group"), suffix = c("", "_elite")) %>% 
  mutate(rr = frac_elite / frac)

pop_dist_ses = expand.grid(
  group = unique(pop_dist_ses$group),
  period = sort(unique(pop_dist_ses$period))
) %>%
  left_join(pop_dist_ses, by = c("group", "period")) %>% 
  group_by(group) %>% 
  mutate(rr_lag = lag(rr)) %>% 
  ungroup()

# plot by group
manor_rr_hiscam = ggplot(pop_dist_ses %>% filter(!is.na(group) & !is.na(rr)), aes(x = period, y = rr, group = group, color = as.factor(group))) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank()) +
  labs(x = "Period",
       y = "Relative representation") +
  scale_color_brewer(palette = "Paired",
                     name = "Pre-1820 avg. HISCAM") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_log10()

ggsave(manor_rr_hiscam, file = "../DanishMobility/Project_dissemination/DanishMobility_slides/figures/manor_rr_by_hiscam.png")

summary(lm(log(rr) ~ log(rr_lag), data = pop_dist_ses))
summary(lm(log(rr) ~ log(rr_lag), data = pop_dist_ses, weights = n))

