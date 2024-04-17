# Analysis using PhD dissertations (2011-2024)
# Date updated:   2024-04-16
# Author:         MHK
#
# Purpose:        Calculate relative representation of specific surname types among PhD students who have handed in their dissertation between 2011 and 2024 (Apr)

# ==== Libraries ====
library(tidyverse)
library(readxl)

# ==== Load data ====
phd = read_xlsx("/Users/mhkr/Downloads/dki-export-94266.xlsx")
census_pre1820_ses = read.csv("../Data/population/census_pre1820_count_ses.csv")
census = read.csv("../Data/population/census_count.csv")
cemetery = read.csv("../Data/population/cemetery/cemetery_count.csv")
dst = read.csv("../Data/population/pop2002_2023/dst_count.csv")


# creating a single data frame which contains distribution of surnames
pop_dist = bind_rows(census,
                     cemetery %>% filter(year>=1902 & year<=2001),
                     dst
)

# ==== Data cleaning ====
phd = phd %>% select(Authors, `Submission Year`, `Publication Year`, Affiliations, `DK Main Research Area`) # retain only the relevant columns (name, year, affiliation, research area)

# assign proper column names
colnames(phd) <- c("name", "submission_year", "publication_year", "affiliations", "area")

phd = phd %>% mutate(name = tolower(name),
                     surname = str_extract(name, "\\b[\\w-]+\\b(?=, [^,]+$)"))

# ==== Analysis ====
breaks <- c(2010, 2023)
labels <- c("2011-2023")

pop_dist_ses = pop_dist %>% 
  mutate(period = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE)) %>% 
  left_join(census_pre1820_ses %>%  
               filter(period == "1787-1820") %>% 
               select(surname, group), by = "surname") %>% 
  filter(!is.na(group) & !is.na(period)) %>% 
  group_by(period, group) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(frac = n/sum(n)) %>% 
  ungroup()

# Group by pre-1820 avg. HISCAM in phd df and count fraction with that surname
phd_count_group = phd %>% 
  left_join(census_pre1820_ses %>% filter(period == "1787-1820") %>% select(surname, group), by = "surname") %>% 
  filter(!is.na(group)) %>% 
  group_by(group, area) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(area) %>% 
  mutate(frac = n/sum(n))

phd_count = phd %>% 
  left_join(census_pre1820_ses %>% filter(period == "1787-1820") %>% select(surname, group), by = "surname") %>% 
  filter(!is.na(group)) %>% 
  group_by(group) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(frac = n/sum(n))

# merge PhD df into pop_dist and plot
phd_rr = pop_dist_ses %>% 
  filter(group!="20-39") %>% 
  left_join(phd_count, by = c("group"), suffix = c("", "_elite")) %>% 
  mutate(rr = frac_elite/frac) %>% 
  ggplot(aes(x = group, y = rr, fill = group)) + 
  labs(y = "Relative representation", x = "") +
  geom_bar(stat = "identity") +
  theme_linedraw() +
  scale_fill_brewer(palette = "Paired",
                    name = "Pre-1820 avg. HISCAM") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  geom_hline(yintercept = 1, linetype = "dotted")

# merge PhD df into pop_dist and plot by research area
phd_rr_by_area = pop_dist_ses %>% 
  left_join(phd_count_group, by = c("group"), suffix = c("", "_elite")) %>% 
  filter(area != "Humanities|Medical Science" & area != "Humanities|Social Science") %>% 
  mutate(rr = frac_elite/frac) %>% 
  ggplot(aes(x = group, y = rr, fill = group)) + 
  labs(y = "Relative representation", x = "") +
  geom_bar(stat = "identity") +
  theme_linedraw() +
  scale_fill_brewer(palette = "Paired",
                     name = "Pre-1820 avg. HISCAM") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black", size = 12)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  facet_grid(~area)

ggsave(phd_rr, file = "../DanishMobility/Project_dissemination/DanishMobility_slides/figures/phd_rr_by_hiscam.png")
ggsave(phd_rr_by_area, file = "../DanishMobility/Project_dissemination/DanishMobility_slides/figures/phd_rr_by_area.png")
