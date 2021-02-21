# Wine dataset ML

# Created on 21 Feb 2020

# Objectives:

# 1. To revise wine supervised machine learning: predicting quality of white wine
# 2. To revise wine supervised machine learning: predicting quality of red wine
# 3. To try to predict type of wine

# Packages #####
library(pacman)

p_load(tidyverse)

# Import data #####

red_rawdata <-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                         strip.white = T, sep = ";", header = T) %>%
  as_tibble()

white_rawdata <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
                            strip.white = T, sep = ";", header = T) %>% 
  as_tibble()



# References ######
# https://online.stat.psu.edu/stat508/lesson/1a

# capstone HarvardX
# http://rstudio-pubs-static.s3.amazonaws.com/565136_b4395e2500ec4c129ab776b9e8dd24de.html#results
