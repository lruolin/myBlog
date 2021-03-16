# Applied Predictive Modelling
# p137

# Infrared spectroscopy technology is used to determine the chemical makeup of a substance.
# The theory of IR spectroscopy holds that unique molecualr structures absorb IR
# frequencies differently. In practice, a spectrometer fires a series of IR frequencies
# into a sample material, and the device measures the absorbance of the sample at each
# individual frequency. This series of measurements creates a spectrum profile which can
# then be used to determine the chemical makeup of the sample material. 

# A Tecator Infratec Food and Feed Analyzer instrument was used to analyse 215 samples 
# of meat across 100 frequencies. In addition to IR profile, analytical chemistry
# determined the percent content of water, fat and protein for each sample.

# If we can establish a predictive relationship between IR spectrum and fat content,
# then food scientists could predict a sample's fat content instead of using
# analytical chemistry. This would provide cost savings, since analytical chemistry
# is more expensive, and is time-consuming.

# load data, packages ------

library(caret)
library(pacman)
p_load(tidyverse, janitor, skimr, psych, tidymodels, learntidymodels, tidytext,
       ggthemes)

data(tecator) # Fat, Water and Protein Content of Meat Samples

# "These data are recorded on a Tecator Infratec Food and Feed Analyzer 
# working in the wavelength range 850 - 1050 nm by the Near Infrared 
# Transmission (NIT) principle. Each sample contains finely chopped pure meat 
# with different moisture, fat and protein contents.

# absorp : absorbance data for 215 samples. The first 129 were originally used
# as a training set.

absorp
str(absorp) # matrix of 100 values, from 850-1050 nm, at 2 nm intervals

# endpoints: percentages of water, fat and protein.
endpoints 
str(endpoints) # matrix

# PCA -----
# carry out PCA to determine the effective dimension of these data

absorp_tibble <- as_tibble(absorp, .name_repair = "unique") %>% 
  clean_names()

absorp_tibble

glimpse(absorp_tibble)

endpoints_tibble <- as_tibble(endpoints, .name_report = "unique") %>% 
  rename(water = V1,
         fat = V2,
         protein = V3)

glimpse(endpoints_tibble)

# combine both datasets
data_meat_x <- absorp_tibble
data_meat_y <- endpoints_tibble

data_meat <- cbind(endpoints_tibble, absorp_tibble) %>% 
  mutate(id = seq(1:215))

glimpse(data_meat)

# EDA ----

# all numeric
# 215 rows, 100x , 3 y
# highly correlated since is IR data
# no missing values
# y is quite skewed

skim(data_meat)

# Checking assumptions ----

data_meat_x %>% 
  cor() %>% 
  KMO() # Overal MSA = 0.97, greater than 0.70

data_meat_x %>% 
  cor() %>% 
  cortest.bartlett(., n = 215) # p < 0.05

# all assumptions met, ok to do PCA

# tidymodels PCA -----

glimpse(data_meat)

# recipe
meat_recipe <- recipe(~ ., data = data_meat) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(water, fat, protein, new_role = "outcome") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca")

meat_recipe

# prep

meat_prep <- prep(meat_recipe)

# loadings
tidy_pca_loadings <- meat_prep %>% 
  tidy(id = "pca")

tidy_pca_loadings

# bake
meat_bake <- bake(meat_prep, data_meat)
meat_bake # PCA LOADING VECTORS

# Check number of PC: ------

# Check number of PCs by eigenvalues

meat_prep$steps[[2]]$res$sdev %>% as_tibble() %>% 
  filter(value>1) # only 1 PC

# check using scree plots

proportion_scree_plot <- meat_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(aes(component, value, label = value)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_text(aes(label = round(value, 2)), vjust = -0.2, size = 4) +
  labs(title = "% Variance explained",
       y = "% total variance",
       x = "PC",
       subtitle = "98.63 % of variance is explained in PC 1",
       caption = "Source: Tecator Dataset {caret}") +
  theme_few() +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))

proportion_scree_plot

# Split the data into a training and a test set, pre-process the data, 
# and build each variety of models described in this chapter. 
# For those mod- els with tuning parameters, what are the optimal 
# values of the tuning parameter(s)?














