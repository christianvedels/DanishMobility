# Combining cemetery files
# Date updated:   2024-04-10
# Author:         MHK
#
# Purpose:        This script combines individual cemetery data files (which was necessary in order to complete data extraction from Dk-gravsten) into one

# ==== Libraries ====
library(tidyverse)

cemetery = read.csv("../Data/population/cemetery/cemetery.csv")
cemetery_1953 = read.csv("../Data/population/cemetery/cemetery_1953.csv")
cemetery_1977 = read.csv("../Data/population/cemetery/cemetery_1977.csv")
cemetery_1992 = read.csv("../Data/population/cemetery/cemetery_1992.csv")
cemetery_2004 = read.csv("../Data/population/cemetery/cemetery_2004.csv")
cemetery_special = read.csv("../Data/population/cemetery/cemetery_special.csv")
cemetery_special1 = read.csv("../Data/population/cemetery/cemetery_special1.csv")

cemetery = bind_rows(cemetery, cemetery_1953, cemetery_1977, cemetery_1992, cemetery_2004, cemetery_special, cemetery_special1) %>% 
  select(!Year) %>% 
  distinct()

remove(cemetery_1953, cemetery_1977, cemetery_1992, cemetery_2004, cemetery_special, cemetery_special1)

# ==== Data cleaning ====

# creating year indicator
start_position <- nchar(cemetery$born) - 3

# Extract the last four digits from birth and death year
cemetery = cemetery %>% mutate(Year_clean = substr(born, start_position, nchar(born)),
                               Year_clean = as.numeric(Year_clean),
                               year_of_death = substr(died, start_position, nchar(died)),
                               year_of_death = as.numeric(year_of_death))

# imputing year of birth for individuals for whom year of birth currently is unknown
avg_birth_years <- cemetery %>% 
  filter((!is.na(Year_clean) & Year_clean != 0) & (!is.na(year_of_death) & year_of_death != 0)) %>% 
  group_by(year_of_death) %>% 
  summarize(avg_year_of_birth = mean(Year_clean, na.rm = T)) %>% 
  mutate(avg_year_of_birth_round = round(avg_year_of_birth))

cemetery = cemetery %>% 
  left_join(avg_birth_years, by = "year_of_death") %>% 
  mutate(Year_clean = ifelse(is.na(Year_clean) | Year_clean == 0, avg_year_of_birth_round, Year_clean)) %>% 
  select(!c(avg_year_of_birth, avg_year_of_birth_round))

# ==== Save results ====

write.csv(cemetery, "../Data/population/cemetery/cemetery_combined.csv", row.names = F)
