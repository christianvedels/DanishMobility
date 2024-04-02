rm(list=ls())

library(purrr)
library(readr)
library(tidyverse)
library(data.table)

base_path_census <- "/Users/mhkr/Library/CloudStorage/Dropbox/Data/Link_lives/standardized_sources"

csv_files <- list.files(path = base_path_census, pattern = "\\.csv$", full.names = T, recursive = T)

list_of_data_frames <- map(csv_files, fread)

census <- bind_rows(list_of_data_frames)

remove(list_of_data_frames, base_path_census, csv_files)

census[census==""] <- NA

census = census %>%
  filter(str_count(name_cl, pattern = "\\S+") > 1) %>% 
  mutate(surname = str_extract(name_cl, "\\S+$"),
         surname = ifelse(name==first_names, NA, surname),
         surname = ifelse(surname == "datter", str_extract(name, "\\S+$"), surname),
         surname = ifelse(str_detect(name_cl, " dtr\\.?$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dtr\\.?$)"), "datter"), surname),
         surname = ifelse(str_detect(name_cl, " dt$"), paste0(str_extract(name_cl, "\\w+(?=\\s+dtr\\.?$)"), "datter"), surname),
         surname = str_replace(surname, "dtr.", "datter")) %>% 
  filter(surname != "do." & surname != "do")

surnames_census = census %>% 
  select(name_cl, surname, event_year) %>% 
  group_by(event_year, surname) %>% 
  summarize(n = n()) %>% 
  ungroup()

ttt = surnames_census %>% filter(event_year == 1840)

tt = census %>% filter(surname == "dt")
tt1 = census %>% filter(str_detect(name_cl, " dt$"))
tt2 = census %>% filter(str_detect(name_cl, "datt\\.$"))
tt3 = census %>% filter(str_detect(name_cl, "dt\\.$"))

View(head(census, n = 1000))

## DST

base_path_DST <- "/Users/mhkr/Library/CloudStorage/Dropbox/Data/population/pop2002_2023"

csv_files <- list.files(path = base_path_DST, pattern = "\\.txt$", full.names = T, recursive = T)

read_file_and_add_year <- function(file_path) {
  # Extract year from the file name (assuming it's right before ".txt")
  year_extracted <- gsub(".+ (\\d+)\\.txt$", "\\1", basename(file_path))
  
  # Read the file into a data frame
  data_frame <- fread(file_path)
  
  # Add the extracted year as a new column
  data_frame$Year <- as.integer(year_extracted)
  
  colnames(data_frame) <- c("surname", "n", "year")
  
  return(data_frame)
}

list_of_data_frames <- map(csv_files, read_file_and_add_year)

dst = bind_rows(list_of_data_frames)

Encoding(dst$surname) <- "latin1"

dst[dst==""] <- NA

dst = dst %>% 
  filter(!is.na(surname)) %>% 
  mutate(surname = tolower(surname))
