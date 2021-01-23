# exporting datasets from Easy Statistics in Food Science with R

# Workflow:
# 1) Have your pdf file ready
# 2) Use tabulizer package to extract tables
# 3) Make data tidy

# 1. Load packages ######
library(rJava)
library(tabulizer)
library(tidyverse)

# 2. Extract table using (extract_tables()) #####

# Banana ######
scraped_banana <- extract_tables(
  file = "dataset-banana.pdf", # character string specifying path to PDF
  method = "stream", # lattice or stream or decide, may need to try
  output = "data.frame"
)

banana_a <- scraped_banana[[1]]
banana_b <- scraped_banana[[2]]

data_banana <- bind_rows(banana_a, banana_b)
glimpse(data_banana)

# export dataset
write.csv(data_banana, "data_scraped_banana.csv")

# Cockles #####

scraped_cockles <- extract_tables(
  file = "data - cockles.pdf", # character string specifying path to PDF
  method = "stream", # lattice or stream or decide, may need to try
  output = "data.frame"
)

# Clean up

# i. Set column names of dataset
col_names <- c("Location", "Cr", "As", "Cd", "Zn", "Cu", "Pb", "Hg")


# ii. Set for first list

cockles_a <- scraped_cockles[[1]] %>% 
  slice(-(1)) %>% # slice: to select, remove and duplicate rows
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  separate(`Location Cr As Cd Zn Cu Pb Hg`,
           c("Location", "Cr", "As", "Cd", "Zn", "Cu", "Pb", "Hg"),
           sep = " ") %>%  # use separate to turn single chr col into multiple col
  slice(-36)

# iii: Table 2: need to split location and Cr
cockles_b <- scraped_cockles[[2]] %>% 
              as_tibble() %>% 
              janitor::row_to_names(1) %>% 
  separate(`Location Cr`,
           c("Location", "Cr"),
           sep = " ")

# iv: merge
data_cockles <- bind_rows(cockles_a, cockles_b)
glimpse(data_cockles)

# export dataset
write.csv(data_cockles, "data_scraped_cockles.csv")

### Tapioca chips #####

scraped_tapioca <- extract_tables(
  file = "data - tapioca.pdf", # character string specifying path to PDF
  method = "stream", # lattice or stream or decide, may need to try
  output = "data.frame")

data_tapioca <- scraped_tapioca[[1]] %>% as_tibble()

# export dataset
write.csv(data_tapioca, "data_scraped_tapioca.csv")

### Date Palm Fruits #####

scraped_datepalm <- extract_tables(
  file = "data - date palm fruits.pdf", # character string specifying path to PDF
  method = "stream", # lattice or stream or decide, may need to try
  output = "data.frame")

data_datepalm <- scraped_datepalm[[1]] %>% 
  janitor::row_to_names(1) %>% 
  separate(`Type TFC TPC AA (frap)`,
           c("Type", "TFC", "TPC", "AA_frap"),
           sep = " ")

glimpse(data_datepalm)

# export dataset
write.csv(data_datepalm, "data_scraped_datepalm.csv")

save.image("scrapped datasets from easy stats book.RData")

# References
# https://www.r-bloggers.com/2019/09/pdf-scraping-in-r-with-tabulizer/