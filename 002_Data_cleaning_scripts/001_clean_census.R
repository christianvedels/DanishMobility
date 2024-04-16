
# Cleaning of surnames of census data
# Date updated:   2024-04-10
# Author:         MHK
#
# Purpose:        This script cleans the census data, as well as creates a data frame counting surnames by year

# ==== Libraries ====
library(purrr)
library(readr)
library(tidyverse)
library(data.table)
library(hisco)

# ==== Load data ====
base_path_census <- "../Data/Link_lives/standardized_sources"
hisco_codes = read.csv("../Data/Link_lives/Census_HISCO_codes_clean.csv")

csv_files <- list.files(path = base_path_census, pattern = "\\.csv$", full.names = T, recursive = T)

list_of_data_frames <- map(csv_files, fread) # load all files listed in csv_files object

census <- bind_rows(list_of_data_frames)

remove(list_of_data_frames, base_path_census, csv_files) # remove temporary objects

# ==== Data cleaning ====
census[census==""] <- NA # replace white space with NAs

census = census %>%
  filter(str_count(name_cl, pattern = "\\S+") > 1) %>% 
  mutate(surname = str_extract(name_cl, "\\S+$"),
         surname = ifelse(name==first_names, NA, surname),
         surname = ifelse(surname == "datter", str_extract(name, "\\S+$"), surname), # if surname is "datter", grab the string preceding " datter" in name variable
         surname = ifelse(str_detect(name_cl, " dtr\\.?$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dtr\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " dt\\.?$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dt\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " datt\\.?$"), paste0(str_extract(name_cl, "\\w+(?=\\s+datt\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " dt\\.$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dt\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " d\\.$"), paste0(str_extract(name_cl, "\\w+(?=\\s+d\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " dr\\.$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dr\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " dr$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dr$)"), "datter"), surname),
         surname = str_replace(surname, "dtr.", "datter"), # rewrite dtr. and other ways to abbreviate "datter" to "datter"
         surname = str_replace(surname, "dt\\.$", "datter"),
         surname = str_replace(surname, "datt\\.$", "datter"),
         surname = str_replace(surname, "d\\.$", "datter"),
         surname = ifelse(sex == "f", str_replace(surname, "dr\\.$", "datter"), surname),
         surname = ifelse(sex == "f", str_replace(surname, "dr$", "datter"), surname),
         surname = ifelse(sex == "f" & surname == "d", paste0(str_extract(name_cl, "\\w+(?=\\s+d$)"), "datter"), surname)) %>% 
  filter(surname != "do." & surname != "do")

census$surname <- sapply(census$surname, function(x) {
  if(grepl("\\.$", x)) {
    # If the string ends with a period, grab the string preceding the last period mark
    sub("(.*)\\.[^.]*\\.$", "\\1", x)
  } else {
    # If the string does not end with a period, grab the string after the last period mark
    sub(".*\\.\\s*", "", x)
  }
})

census = census %>% mutate(surname = str_replace_all(surname, "\\.", "")) # removes all periods from the surname variable

# cleaning HISCO codes df
hisco_codes = hisco_codes %>% 
  mutate(year = substr(Kilde, nchar(Kilde) - 3, nchar(Kilde)),
         year = as.numeric(year))

# merging HISCO codes into census data
census = census %>% left_join(hisco_codes, by = c("event_year" = "year",
                                                  "pa_id" = "pa_id"))

remove(hisco_codes)

# computing HISCAM score and attaching it to census df
census$ses = hisco_to_ses(census$hisco_1, ses = "hiscam_u1")

# creating count
surnames_census = census %>% 
  select(name_cl, surname, event_year) %>% 
  group_by(event_year, surname) %>% 
  summarize(n = n()) %>% 
  ungroup()

colnames(surnames_census) = c("year", "surname", "n")

# ==== Save results ====
write.csv(census, "../Data/population/census.csv", row.names = F)
write.csv(surnames_census, "../Data/population/census_count.csv", row.names = F)
