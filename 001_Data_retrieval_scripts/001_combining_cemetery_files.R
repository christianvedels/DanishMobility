# Combining cemetry files
# Date updated:   2024-04-08
# Auhtor:         MHK
#
# Purpose:        WHAT IS THE PURPOSE?


# ==== Libraries ====
library(tidyverse)

# ==== Load data ====
cemetery = read.csv("../Data/population/cemetery/cemetery.csv")
cemetery_1953 = read.csv("../Data/population/cemetery/cemetery_1953.csv")
cemetery_1977 = read.csv("../Data/population/cemetery/cemetery_1977.csv")
cemetery_1992 = read.csv("../Data/population/cemetery/cemetery_1992.csv")
cemetery_2004 = read.csv("../Data/population/cemetery/cemetery_2004.csv")
cemetery_special = read.csv("../Data/population/cemetery/cemetery_special.csv")
cemetery_special1 = read.csv("../Data/population/cemetery/cemetery_special1.csv")

# ==== Data cleaning ====
cemetery = bind_rows(cemetery, cemetery_1953, cemetery_1977, cemetery_1992, cemetery_2004, cemetery_special, cemetery_special1) %>% 
  select(!Year) %>% 
  distinct()

remove(cemetery_1953, cemetery_1977, cemetery_1992, cemetery_2004, cemetery_special, cemetery_special1)

# creating year indicator
start_position <- nchar(cemetery$died) - 3

# Extract the last four digits from death year
cemetery$Year_clean <- substr(cemetery$died, start_position, nchar(cemetery$died))

cemetery$Year_clean = as.numeric(cemetery$Year_clean)

# ==== Save results ====
write.csv(cemetery, "../Data/population/cemetery/cemetery_combined.csv", row.names = F)
