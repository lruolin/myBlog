# https://cmdlinetips.com/2020/06/pca-with-tidymodels-in-r/

library(tidymodels)
library(tidyverse)
library(gapminder)


# ! GAPMINDER ---------------------------------------------------------------


# IMPORT #####
glimpse(gapminder) # 1704 x 6
head(gapminder)

# Use life expectancy over the years from all countries to do 
# PCA

# TRANSFORM #####

# convert to wide form, years as col
life_df <- gapminder %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(values_from = lifeExp,
              names_from = year)

glimpse(life_df)

# RECIPES #####
#
pca_recipe <- recipe(~., data = life_df)
pca_recipe
#
pca_trans <- pca_recipe %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_pca(all_numeric())

#
pca_estimates <- prep(pca_trans)
pca_estimates
names(pca_estimates)

pca_estimates[[3]]


sdev <- pca_estimates$steps[[3]]$res$sdev

percent_variation <- sdev^2/sum(sdev^2)
var_df <- data.frame(PC = paste0("PC", 1:length(sdev)),
                     var_explained = percent_variation,
                     stringsAsFactors = F)

# scree plot
var_df %>% 
  mutate(PC = fct_inorder(PC)) %>% 
  ggplot(aes(x = PC, y = var_explained, group = 1)) +
 geom_path(aes(x = PC, y = var_explained))+
  geom_point(aes(x = PC, y = var_explained, size = var_explained),
             show.legend = F) +
  theme_bw()

# See PCA
juice(pca_estimates)

juice(pca_estimates) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = continent), alpha = 0.7, size = 2)+
  labs(title="PCA from tidymodels")

juice(pca_estimates) %>%
  #separate(country_continent,c("country","continent"), sep="_") %>%
  ggplot(aes(PC1, PC2, label = continent)) +
  geom_point(aes(color = continent), alpha = 0.7, size = 2,show.legend = FALSE) +
  geom_text(check_overlap = TRUE) + 
  labs(title="PCA with Tidymodels")


# ! ZOO ---------------------------------------------------------------------

# https://rdrr.io/github/tidymodels/learntidymodels/f/inst/tutorials/pca_recipes/pca_recipes.Rmd

library(pacman)
p_load(learnr, tidyverse, tidymodels, embed, corrr, tidytext,
       gradethis, sortable, learntidymodels, psych, gridExtra,
       highcharter)

# install.packages("devtools")
# devtools::install_github("tidymodels/learntidymodels")

# IMPORT #####

zoo_names <- c("animal_name", "hair", "feathers", "eggs", "milk", "airborne", "aquatic", "predator", "toothed", "backbone", "breathes", "venomous", "fins", "legs", "tail", "domestic", "catsize", "class")
anim_types <- tribble(~class, ~type,
                      1, "mammal",
                      2, "bird",
                      3, "reptile",
                      4, "fish",
                      5, "amphibian",
                      6, "insect",
                      7, "other_arthropods")


zoo <- 
  read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data", 
           col_names = zoo_names) %>%
  left_join(anim_types) %>%
  select(-class) %>%
  rename(animal_type=type)

glimpse(zoo) # 101 rows x 18 col
# animal name and animal type

# Exploratory: animals #####
# how many types of animals?
zoo %>% 
  count(animal_type) # 7 types; count the unique values variable

# visualize
zoo %>% 
  ggplot(aes(animal_type)) +
  geom_bar(fill = "deepskyblue3") +
  theme_bw()

# alternatively
library(GGally)
zoo %>% 
  select(animal_type) %>% 
  ggpairs()

zoo %>% 
  mutate(eggs = recode(eggs, 
                       "0" = "doesn't lay eggs",
                       "1" = "lays eggs")) %>% 
  ggplot(aes(animal_type, fill = eggs)) +
  geom_bar() + # by default is count
  theme_minimal()

zoo %>% 
  count(animal_type, eggs)

# which mammal lays eggs?

zoo %>% 
  filter(animal_type == "mammal", eggs == "1") %>% 
  select(animal_name, animal_type, eggs)

# EDA : Correlation #####
zoo_corr <- zoo %>%
  select(-animal_name, -animal_type) %>% # remove non-numeric
  corrr::correlate() %>%
  rearrange() %>% 
  shave()

library(GGally)  # this is better personally
zoo %>% 
  select(-animal_name, -animal_type) %>%
  ggcorr(label = T, label_alpha = T,label_round = 2)

# PCA #####

# 0. Assumption checks #####

# Assp #1: 
# You should have multiple variables that are measured
# at the continuous level

# Assp #2:
# There needs to be a linear relationship between all
# variables

# Assp #3:
# There should be large enough sample sizes
# 5 - 10 cases per variable, min 150 cases

# Assp 4:
# Data should be suitable for reduction. 
# Bartlett's test of sphericity - residual corr are 0
# KMO sampling adequacy

zoo %>% 
  select(-animal_name, -animal_type) %>% 
  cor() %>% 
  psych::KMO()  # Cutoff is 0.70, here is 0.64

zoo %>% 
  select(-animal_name, -animal_type) %>% 
  cor() %>% 
  psych::cortest.bartlett(., n = 101) # p < 0.05 cutoff

# 1. Create recipe #####
          
                  # do not define Y since PCA is unsupervised
pca_rec <- recipe(~., data = zoo) %>% 
    # change animal name into id
    # alters existing variable without dropping them
  update_role(animal_name, new_role = "id") %>%  # alters existing role

   # step_scale(all_predictors()) %>%  # all x, normalize to std dev = 1
#  step_center(all_predictors()) %>%  # all x, normalise to mean = 0
 
  step_normalize(all_numeric()) %>% # combines scale and center
   step_pca(all_numeric(), id = "pca") # to provide id

pca_rec

# 2. prep #####

pca_prep <- prep(pca_rec)
pca_prep # trained # list of 9

tidy(pca_prep)  # step pca: step 2

# extract the intermediate values computed in each step by providing
# its number as an argument to tidy

tidy(pca_prep, 2)

pca_zoo_results <- tidy(pca_prep, 2)
pca_zoo_results 

# or you can specify the type of underlying value to extract

pca_loading <- tidy(pca_prep, id = "pca", type = "coef")
pca_loading

pca_variances <- tidy(pca_prep, id = "pca", type = "variance")

# Variance explained plot #####
sdev <- pca_prep$steps[[2]]$res$sdev

percent_variation <- sdev^2/sum(sdev^2) # 
var_df <- data.frame(PC = paste0("PC", 1:length(sdev)),
                     var_explained = percent_variation,
                     stringsAsFactors = F)

# variance explained
var_df %>% mutate(PC = fct_inorder(PC)) %>% 
  ggplot(aes(x = PC, y = var_explained, group = 1)) +
  geom_point() +
  geom_path() +
  labs(title = "Variance explained") +
  theme_minimal() # looks like 4

# scree plot #####
# alternatively, from recipes:
# step_PCA is step 2, therefore [[2]]
eigen_values_alt<- pca_prep$steps[[2]]$res$sdev^2
eigen_values_alt

# visualizing scree plot
eigen_values_alt %>% 
  as_tibble() %>% 
  ggplot(aes(x = 1:16, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:16) +
  labs(title = "Scree plot for PCA",
       x = "PC", y = "Eigenvalues") +
  theme_classic()

# parallel analysis #####
zoo_pca <- zoo %>% 
  select(-animal_name, - animal_type)

psych::fa.parallel(cor(zoo_pca),
                   n.obs = 101, 
                   cor = "cor",
                   plot = T)  # suggests 3


# 3. bake #####
pca_bake <- bake(pca_prep, zoo)
pca_bake # has the PC readings

pca_bake %>%
  ggplot(aes(PC1, PC2, label=animal_name)) +
  geom_point(aes(color = animal_type), alpha = 0.7, size = 2)+
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = "Animal Type") +
  theme_minimal()

# 4. visualize #####
# plot the first four components and top 10 variables to get largest abs loadings
plot_top_loadings(pca_prep, component_number <= 4, n = 10) + 
  scale_fill_manual(values = c("#372F60", "#CA225E")) +
  theme_minimal()

# plot the first two principal components, 
# while labeling our points with animal name
# and coloring by animal_type

pc1pc2 <- pca_bake %>%
  ggplot(aes(PC1, PC2, label=animal_name)) +
  geom_point(aes(color = animal_type), alpha = 0.7, size = 2)+
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

pc1pc2
# mammals, reptiles + birds, fishes

# how much variablity in data is captured by principal compoennts

pca_variances <- tidy(pca_prep, id = "pca", type = "variance")
pca_variances

pca_variances %>% 
  count(terms)

# variance explained plot
pca_variances %>%
  filter(terms == "percent variance") %>%
  ggplot(aes(component, value)) +
  geom_col(fill="#372F60") +
  labs(x = "Principal Components", y = "Variance explained (%)") +
  theme_minimal()

# cumulative variance
pca_variances %>%
  filter(terms == "cumulative percent variance") %>%
  ggplot(aes(component, value)) +
  geom_col(fill="#372F60") +
  labs(x = "Principal Components", y = "Cumulative variance explained (%)") +
  theme_minimal()

# We can see that 50% of the variance is explained by the 
# first two components. If we were to use more components, 
# we can capture even more variance in the data. 
# That is why it is also common to plot multiple components 
# to get a better idea of the data.

# Try plotting PC1 and PC3, do you see other clusters in 
# the data that wasn't as obvious with PC1 and PC2?

pc1pc3 <- pca_bake %>%
  ggplot(aes(PC1, PC3, label=animal_name)) +
  geom_point(aes(color = animal_type), alpha = 0.7, size = 2)+
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

pc1pc3

# PC2, PC3
pc2pc3 <- pca_bake %>%
  ggplot(aes(PC2, PC3, label=animal_name)) +
  geom_point(aes(color = animal_type), alpha = 0.7, size = 2)+
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

pc2pc3

grid.arrange(pc1pc2, pc1pc3, pc2pc3, ncol = 2)

# highcharter #####

hchart(princomp(zoo_pca, cor = T))  # loadings plot


#! PENGUINS --------------------------------------------------------------
# https://allisonhorst.github.io/palmerpenguins/articles/articles/pca.html


library(pacman)
p_load(palmerpenguins, corrr, GGally, recipes, tidytext,
       tidyverse, skimr)

theme_set(theme_minimal())

# IMPORT ####
penguins # 344 x 8
glimpse(penguins)



# EXPLORATORY #####

# to see types of species of penguins
penguins %>% 
  select(species) %>% 
  unique() 


# pairwise matrix #####
penguins %>% 
  select(bill_length_mm, bill_depth_mm, flipper_length_mm,
         body_mass_g, species) %>% # species = Y
  ggpairs(aes(col = species),
          columns = c("bill_length_mm",
                      "bill_depth_mm",
                      "flipper_length_mm",
                      "body_mass_g")) 

# PCA can only take continuous variables
penguins_pca <- penguins %>% 
  select(where(is.numeric)) %>% 
  select(-year) %>% 
  drop_na()
  

penguins_pca

skim(penguins_pca) # there are missing values initially




# to see correlation

penguins_pca %>% 
  ggcorr(label = T, label_alpha = T, label_round = 2)

# body mass and flipper length are correlated

# check dataset

# PCA #####
# need to impute missing values
# need to normalize
# need to PCA

# 0. assumption checks #####
# need to make sure no missing values

penguins_pca %>%  
  cor() %>% 
  psych::KMO()  # Cutoff is 0.70, here is 0.64

penguins_pca%>% 
  cor() %>% 
  psych::cortest.bartlett(., n = 344) # p < 0.05 cutoff

# 1. Create recipe #####
glimpse(penguins)

penguins <- penguins %>% 
  mutate(year = factor(year)) %>% 
  drop_na()

penguin_recipe <-  recipe(~., data = penguins) %>% 
  update_role(species, island, sex, year, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca")

# 2. Prep #####
penguin_prep <- penguin_recipe %>% 
  prep()

# to see tidy version of PC loadings
penguin_pca_loading <- penguin_prep%>% 
  tidy(id = "pca")

# to see how much variance each component explains
penguin_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Variance explained",
       y = "% of total variance",
       caption = "Source: Palmer Penguins Dataset") +
  theme_classic()

# scree plot #####
# alternatively, from recipes:
# step_PCA is step 2, therefore [[3]]
eigen_values_alt<- penguin_prep$steps[[3]]$res$sdev^2
eigen_values_alt # 1

# visualizing scree plot, eigenvalues #####
eigen_values_alt %>% 
  as_tibble() %>% 
  ggplot(aes(x = 1:4, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:4) +
  labs(title = "Scree plot for PCA",
       x = "PC", y = "Eigenvalues") +
  theme_classic()

# parallel analysis #####
skim(penguins)

penguins_pca

psych::fa.parallel(cor(penguins_pca),
                   n.obs = 342, 
                   cor = "cor",
                   plot = T)  # suggests 1

# 3. bake #####
penguin_bake <- bake(penguin_prep, penguins)
penguin_bake # has the PC readings

# 4. visualize #####
# plot the first four components and top 10 variables to get largest abs loadings
plot_top_loadings(penguin_prep, component_number <= 4, n = 4) + 
  scale_fill_manual(values = c("deepskyblue3", "grey")) +
  theme_minimal()


# define arrow style
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")
# 1
# get PCA loadings in a wider format
pca_wider <- penguin_pca_loading %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)

pca_plot <- penguin_bake %>%
  ggplot(aes(PC1, PC2, label=species)) +
  geom_point(aes(color = species), alpha = 0.7, size = 2)+
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = "species") +
  theme_minimal() 

loadings_plot <- pca_wider %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_segment(aes(xend = PC1, yend = PC2),
                    x = 0, y = 0,
                    arrow = arrow_style) +
  ggrepel::geom_text_repel(aes(x = PC1, y = PC2,
                               label = terms),
                           hjust = 0, vjust = 1,
                           size = 5, col = "darkred") +
  labs(title = "Loadings on PC 1 and PC 2") +
  theme_classic()

loadings_plot
