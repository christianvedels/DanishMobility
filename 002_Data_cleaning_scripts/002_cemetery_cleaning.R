rm(list=ls())

library(tidyverse)

cemetery = read.csv("../Data/population/cemetery/cemetery_combined.csv")

# replacing whitespace with NAs
cemetery[cemetery==""] <- NA

cemetery = cemetery %>% mutate(maiden_name_extracted = str_extract(name, "(?<= f\\.).*") %>% str_squish(),
                               name_clean = str_replace(name, " f\\..*", "") %>% str_squish(), # removes all text following "f."
                               name_clean = str_remove_all(name_clean, "\\[.*?\\]|\\(.*?\\)"), # removes all text within brackets
                               name_clean = str_remove_all(name_clean, "\\[|\\]|\\(|\\)"), # removes brackets
                               maiden_name_extracted = str_remove_all(maiden_name_extracted, "\\[|\\]|\\(|\\)"),
                               name_clean = gsub("\\b\\S*hustru\\S*\\b", "", name_clean),
                               name_clean = str_replace_all(name_clean, "\\.", " "),
                               name_clean = str_replace_all(name_clean, "\\,", " "),
                               name_clean = str_replace_all(name_clean, "\\d", ""),
                               name_clean = str_replace_all(name_clean, "!", ""),
                               name_clean = str_replace_all(name_clean, "\\?", "") %>% str_squish(),
                               maiden_name_extracted = gsub("\\b\\S*hustru\\S*\\b", "", maiden_name_extracted),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\.", " "),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\,", " "),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\d", ""),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "!", ""),
                               maiden_name_extracted = str_replace_all(maiden_name_extracted, "\\?", "") %>% str_squish(),
                               surname = sub(".*\\s(.*)$", "\\1", name_clean),
                               surname_maiden = sub(".*\\s(.*)$", "\\1", maiden_name_extracted))

# removing single-name observations (approx. 10,000 observations)
cemetery = cemetery %>%
  filter(str_detect(name_clean, "\\s"))

cemetery <- cemetery %>%
  mutate(surname = ifelse(str_detect(surname, "_"), NA, surname),
         maiden_name_extracted = ifelse(str_detect(maiden_name_extracted, "_"), NA, maiden_name_extracted),
         surname = tolower(surname))

write.csv(cemetery, "../Data/population/cemetery/cemetery_clean.csv", row.names = F)