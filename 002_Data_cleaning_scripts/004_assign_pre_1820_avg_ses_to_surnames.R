# Assigning avg. SES to surnames
# Date updated:   2024-04-15
# Author:         MHK
#
# Purpose:        assign avg. SES to surnames and store file containing count by pre-1820 avg. level HISCAM

# ==== Libraries ====
library(tidyverse)
library(hisco)

# ==== Load data ====
census <- read.csv("../Data/population/census.csv")

# retaining variables that are relevant
census = census %>% select(name, name_cl, surname, event_year, occ1, 
                           hisco_1, prob_1, desc_1, hisco_2, prob_2, desc_2,
                           hisco_3, prob_3, desc_3, hisco_4, prob_4, desc_4,
                           hisco_5, prob_5, desc_5, ses)

# defining periods
breaks_t <- c(1787, 1820, 1850, 1880, 1910, 1940, 1970, 2000, 2023)
labels_t <- c("1787-1820", "1821-1850", "1851-1880", "1881-1910", "1911-1940", "1941-1970", "1971-2000", "2001-2023")

# summarizing census data by period and surname
census_count = census %>% 
  mutate(period = cut(event_year, breaks = breaks_t, labels = labels_t, include.lowest = TRUE)) %>% 
  group_by(period, surname) %>% 
  summarize(avgSES = mean(ses, na.rm = T), # calculating avg. HISCAM score
            nSES = sum(!is.na(ses)), # number of observations for whom ses (HISCAM) is non-NA
            n = n()) %>%
  ungroup() 

remove(census) # free space

# creating SES groups
breaks <- seq(0, 100, by = 20)

# breaking surnames into SES groups
census_count = census_count %>% mutate(group = cut(avgSES, breaks = breaks, include.lowest = TRUE, right = FALSE,
                                                   labels = paste(breaks[-length(breaks)], breaks[-1] - 1, sep = "-")))

# ==== Save data ====
write.csv(census_count, "../Data/population/census_pre1820_count_ses.csv", row.names = F)
