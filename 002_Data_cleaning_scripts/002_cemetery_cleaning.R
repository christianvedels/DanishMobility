# Cleaning of surnames in cemetery data
# Date updated:   2024-04-10
# Author:         MHK
#
# Purpose:        This script cleans the cemetery data

# ==== Libraries ====
library(tidyverse)

# ==== Load data ====
cemetery = read.csv("../Data/population/cemetery/cemetery_combined.csv")

# ==== Data cleaning ====
cemetery[cemetery==""] <- NA # replacing whitespace with NAs

cemetery = cemetery %>% mutate(maiden_name_extracted = str_extract(name, "(?<= f\\.).*") %>% str_squish(),
                               name_clean = str_replace(name, " f\\..*", "") %>% str_squish(), # removes all text following "f."
                               name_clean = str_remove_all(name_clean, "\\[.*?\\]|\\(.*?\\)"), # removes all text within brackets
                               name_clean = str_remove_all(name_clean, "\\[|\\]|\\(|\\)"), # removes brackets
                               maiden_name_extracted = str_remove_all(maiden_name_extracted, "\\[|\\]|\\(|\\)"), 
                               name_clean = gsub("\\b\\S*hustru\\S*\\b", "", name_clean), # removing "hustru" from name variable
                               name_clean = str_replace_all(name_clean, "\\.", " "), # removing periods
                               name_clean = str_replace_all(name_clean, "\\,", " "), # removing commas
                               name_clean = str_replace_all(name_clean, "\\d", ""),
                               name_clean = str_replace_all(name_clean, "!", ""), # removes exclamation mark
                               name_clean = str_replace_all(name_clean, "\\?", "") %>% str_squish(), # removes question mark
                               maiden_name_extracted = gsub("\\b\\S*hustru\\S*\\b", "", maiden_name_extracted),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\.", " "),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\,", " "),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\d", ""),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "!", ""),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\?", "") %>% str_squish(),
                               surname = sub(".*\\s(.*)$", "\\1", name_clean),
                               surname_maiden = sub(".*\\s(.*)$", "\\1", maiden_name_extracted))

# removing single-name observations (approx. 10,000 observations) and writing surname to lower case
cemetery = cemetery %>%
  filter(str_detect(name_clean, "\\s")) %>% 
  mutate(surname = tolower(surname))

# creating count data
cemetery_count = cemetery %>% group_by(surname, Year_clean) %>% count()
colnames(cemetery_count) = c("surname", "year", "n")

# ==== Save results ====

write.csv(cemetery, "../Data/population/cemetery/cemetery_clean.csv", row.names = F)

write.csv(cemetery_count, "../Data/population/cemetery/cemetery_count.csv", row.names = F)
