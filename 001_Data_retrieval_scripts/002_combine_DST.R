
# Combining surname distributions provided by Statistics Denmark
# Date updated:   2024-04-10
# Author:         MHK
#
# Purpose:        This script combines surname distributions provided by Statistics Denmark into one file

# ==== Libraries ====
library(purrr)
library(readr)
library(tidyverse)
library(data.table)

# ==== Load data ====
base_path_DST <- "../Data/population/pop2002_2023"

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

# ==== Data cleaning ====
dst = bind_rows(list_of_data_frames)

Encoding(dst$surname) <- "latin1"

dst[dst==""] <- NA

dst = dst %>% 
  filter(!is.na(surname)) %>% 
  mutate(surname = tolower(surname))


# ==== Save results ====
write.csv(dst, "../Data/population/pop2002_2023/dst_count.csv", row.names = F)

