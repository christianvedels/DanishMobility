# Preliminary analysis using members of parliament as elite occupation
# Date updated:   2024-04-15
# Author:         MHK
#
# Purpose:        To produce preliminary results depicting how the relative representation of surnames have changed over time

# ==== Libraries ====
library(tidyverse)
library(readxl)
library(ggplot2)
library(fixest)

# ==== Load data ====

# for creation of surname distribution in population
census = read.csv("../Data/population/census_count.csv")
cemetery = read.csv("../Data/population/cemetery/cemetery_count.csv")
dst = read.csv("../Data/population/pop2002_2023/dst_count.csv")

# for identifying elite surnames
nobility = read.csv("../Data/elite/nobility.csv")
census_pre1820_ses = read.csv("../Data/population/census_pre1820_count_ses.csv")

# for identifying distribution of surnames in elite occupation
dld = readRDS("../Data/elite/DLD/member1849-2022.rds")

# creating a single data frame which contains distribution of surnames
pop_dist = bind_rows(census, # from 1787 to 1901: census dictates the distribution
                     cemetery %>% filter(year>=1902 & year<=2001),  # from 1902 to 2001: cemetery
                     dst # from 2002 to 2023: DST
                     )

pop_dist = pop_dist %>% filter(year != 1885) # remove data from 1885 census as it only covers CPH

# ==== Data cleaning ====
dld = dld %>% mutate(lastName = tolower(lastName))

# periods (aim for 30-year intervals as these represent a generation):
breaks <- c(1787, 1820, 1850, 1880, 1910, 1940, 1970, 2000, 2023)
labels <- c("1787-1820", "1821-1850", "1851-1880", "1881-1910", "1911-1940", "1941-1970", "1971-2000", "2001-2023")

# Categorize data into periods
pop_dist_surname <- pop_dist %>%
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  group_by(surname, period) %>%
  summarize(n = sum(n))

# ==== Surname-level analysis ====

pop_dist_surname = pop_dist_surname %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% # calculate fraction of individuals with surname i in period t
  ungroup()

nobility$nobility = 1 # attaching an indicator for whether a surname is deemed as noble

# merging mobility indicator into population data frame
pop_dist_surname = pop_dist_surname %>% 
  left_join(nobility %>% distinct(noble_family, nobility), by = c("surname" = "noble_family"))

# create period indicator in Danish Legislators Database (DLD)
dld_surname = dld %>% 
  mutate(lastName = str_trim(lastName),
         period = cut(bYear, breaks = breaks, labels = labels, include.lowest = T))

# merge nobility indicator into DLD
dld_surname = dld_surname %>% 
  left_join(nobility %>% distinct(noble_family, nobility), by = c("lastName" = "noble_family")) %>% 
  group_by(period, lastName) %>% 
  mutate(n = n()) %>% 
  ungroup()

# compute fraction of politicians with surname i in period t
dld_surname = dld_surname %>% 
  group_by(period) %>% 
  select(period, lastName, n, nobility) %>% 
  distinct() %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

# merge DLD surname count into data frame with the population distribution of surnames
pop_dist_dld_s = pop_dist_surname %>% right_join(dld_surname, by = c("period" = "period",
                                                                     "surname" = "lastName"), 
                                                 suffix = c("", "_elite"))

# calculate relative representation of surname i in period t
pop_dist_dld_s = pop_dist_dld_s %>% mutate(rr = frac_elite/frac)

# keep only surnames we observe for more than 1 period
pop_dist_dld_s = pop_dist_dld_s %>% filter(!is.na(period)) %>% group_by(surname) %>% filter(n()>=2)

# create panel data set of surname
pop_dist_dld_s = expand.grid(
  surname = unique(pop_dist_dld_s$surname),
  period = sort(unique(pop_dist_dld_s$period))
) %>%
  left_join(pop_dist_dld_s, by = c("surname", "period"))

# create lag of relative representation
pop_dist_dld_s = pop_dist_dld_s %>% group_by(surname) %>% mutate(lag_rr = lag(rr))

# plot relative representation by surname
ggplot(pop_dist_dld_s, aes(x = period, y = rr, group = surname)) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  labs(x = "Period",
       y = "Relative representation")

summary(feols(log(rr) ~ log(lag_rr) | surname, data = pop_dist_dld_s))

# ==== Analysis by nobility status ====
pop_dist_nob = pop_dist %>%
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  group_by() %>% 
  left_join(nobility %>% distinct(noble_family, nobility), by = c("surname" = "noble_family")) %>% 
  group_by(nobility, period) %>%
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(nobility = ifelse(is.na(nobility), 0, 1))

# compute fraction of noble vs. unnobled surnames in the population
pop_dist_nob = pop_dist_nob %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

## categorize DLD into periods
dld_nob = dld %>% 
  mutate(lastName = str_trim(lastName),
         period = cut(bYear, breaks = breaks, labels = labels, include.lowest = T))

# merge nobility indicator into DLD and count by nobility status
dld_nob = dld_nob %>% 
  left_join(nobility %>% distinct(nobility, noble_family), by = c("lastName" = "noble_family")) %>% 
  mutate(nobility = ifelse(is.na(nobility), 0, 1)) %>% 
  group_by(period, nobility) %>% 
  mutate(n = n()) %>% 
  ungroup()

# count fraction of noble vs. unnoble surnames in DLD
dld_nob = dld_nob %>% 
  group_by(period) %>% 
  select(period, nobility, n) %>% 
  distinct() %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

# merge DLD into population distribution of surnames
pop_dist_dld_nob = pop_dist_nob %>% left_join(dld_nob, by = c("period" = "period",
                                                              "nobility" = "nobility"), 
                                              suffix = c("", "_elite"))

# compute relative representation
pop_dist_dld_nob = pop_dist_dld_nob %>% mutate(rr = frac_elite/frac)

# plot relative representation over time
ggplot(pop_dist_dld_nob, aes(x = period, y = rr, group = nobility, color = as.factor(nobility))) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("1" = "blue", "0" = "red"),
                     name = "Nobility",
                     labels = c("1" = "Yes", "0" = "No")) +
  scale_y_continuous(breaks = seq(1, 10, 3)) +
  labs(x = "Period",
       y = "Relative representation")

# create lag of relative representation
pop_dist_dld_nob = pop_dist_dld_nob %>% mutate(rr_lag = lag(rr))

# estimate regression of interest
summary(lm(log(rr) ~ log(rr_lag), data = pop_dist_dld_nob %>% filter(nobility==1)))


# ==== Analysis by avg. HISCAM score pre-1820 ====

pop_dist_ses = pop_dist %>% 
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  right_join(census_pre1820_ses %>% 
               filter(period == "1787-1820") %>% 
               select(surname, group), by = "surname") %>% 
  group_by(period, group) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

dld_ses = dld %>% 
  mutate(lastName = str_trim(lastName),
         period = cut(bYear, breaks = breaks, labels = labels, include.lowest = T))

# merge pre-1820 avg. HISCAM score into dld df
dld_ses = dld_ses %>% 
  right_join(census_pre1820_ses %>% filter(period == "1787-1820") %>% select(surname, group), by = c("lastName" = "surname")) %>% 
  group_by(period, group) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n))

# merge dld into pop. dist
pop_dist_ses = pop_dist_ses %>% 
  filter(period != "1787-1820") %>% 
  left_join(dld_ses, by = c("period", "group"), suffix = c("", "_elite")) %>% 
  mutate(rr = frac_elite / frac)

pop_dist_ses = expand.grid(
  group = unique(pop_dist_ses$group),
  period = sort(unique(pop_dist_ses$period))
) %>%
  left_join(pop_dist_ses, by = c("group", "period")) %>% 
  group_by(group) %>% 
  mutate(rr_lag = lag(rr)) %>% 
  ungroup() %>% 
  filter(period != "2001-2023") # only 1 observation in this period

ggplot(pop_dist_ses %>% filter(!is.na(group) & !is.na(rr)), aes(x = period, y = rr, group = group, color = as.factor(group))) +
  geom_line() +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  labs(x = "Period",
       y = "Relative representation") +
  scale_color_brewer(palette = "Paired",
                     name = "Pre-1820 avg. HISCAM")

summary(lm(log(rr) ~ log(rr_lag), data = pop_dist_ses))
