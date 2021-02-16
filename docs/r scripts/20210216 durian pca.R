# PCA for durian

# https://www.sciencedirect.com/science/article/abs/pii/S0889157506001013

# PACKAGES ####
library(pacman)
p_load(tidyverse, janitor, skimr, psych, tidymodels, learntidymodels)

# IMPORT ####

durian <- read_csv("Durian.csv") %>% 
  clean_names() %>% 
  mutate(peak_no_2 = paste( "peak", peak_no, sep = "_")) %>% 
  select(-peak_no) %>% 
  rename(peak_no = peak_no_2) %>% 
  select(peak_no, everything())

glimpse(durian)

# so that can pivot longer later
# durian$d101 <- as.character(durian$d101)
# durian$d2 <- as.character(durian$d2)
durian$d24 <- as.numeric(durian$d24)

# TRANSFORM #####

durian_reshape <- durian %>% 
  
  # remove unnecessary columns
  select(-category, -odor_description, -compound) %>% 
  # pivot longer for variety
  pivot_longer(cols = starts_with("d"),
               names_to = "variety",
               values_to = "concentration") %>% 
  
  pivot_wider(names_from = peak_no,
              values_from = concentration) %>% 
  
  clean_names() %>% 

  # pivot wider for compound names as (X)/Features
  dplyr::group_by(variety) %>% 
  dplyr::summarize_all(sum, na.rm = T)

  
glimpse(durian_reshape)  # 40 variables: 1Y and 39 X

durian_reshape$variety <- factor(durian_reshape$variety)

# EDA
skim(durian_reshape)

# no missing values
# should do auto-scale and means centering later

# Check assumptions for EDA

durian_no_y <- durian_reshape %>% 
  dplyr::select(-variety)

# KMO test
durian_no_y %>% 
  cor() %>% 
  KMO() # overall MSA = 0.5

# Bartlett 

durian_no_y %>% 
  cor() %>% 
  cortest.bartlett(., n = 3) # p = 1, by right not suitable for PCA

# 3 observations - not really ok for PCA

# PCA ####
glimpse(durian_reshape)

# recipe
durian_recipe <- recipe(~ ., data = durian_reshape) %>% 
  update_role(variety, new_role = "id") %>%  
  # step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca", num_comp = 3)

durian_recipe

# need to specify num_comp = 3 if not will have error
# Error in comps[, 1:object$num_comp, drop = FALSE] : 
# subscript out of bounds


# prep: estimate the required parameters from a training set
# that can be later applied to other data sets
# returns an updated recipe with its estimates

durian_prep <- prep(durian_recipe)

tidy_pca_loadings <- durian_prep %>% 
  tidy(id = "pca")

tidy_pca_loadings

# bake

durian_bake <- bake(durian_prep, durian_reshape)

durian_bake

# cumulative variance plot

durian_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(component, value)) +
  geom_col(fill = "forestgreen") +
  labs(x = "Principal Components",
       y = "Cumulative percent variance explained") +
  geom_text(aes(label = round(value, 2)), vjust = -0.2, size = 4) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10))


# plot loadings for top 8

loadings_top_8 <- tidy_pca_loadings %>% 
  group_by(component) %>% 
  top_n(8, abs(value)) %>% 
  ungroup() %>% 
  mutate(terms = tidytext::reorder_within(terms, abs(value), component)) %>% 
  ggplot(aes(abs(value), terms, fill = value>0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  ggthemes::scale_fill_few() +
  theme_minimal()

loadings_top_8 

juice(durian_prep) %>% 
  ggplot(aes(PC1, PC2, label = variety)) +
  geom_point(aes(col = variety), show.legend = F) +
  geom_text() +
  labs(x = "PC1 (59.28%)",
       y = "PC2 (41.72%)") +
  theme_classic()
                   

# loadings only

# define arrow style
arrow_style <- arrow(angle = 30,
                     length = unit(0.02, "inches"),
                     type = "closed")

# get pca loadings into wider format
pca_loadings_wider <- tidy_pca_loadings%>% 
  pivot_wider(names_from = component, id_cols = terms)


pca_loadings_only <- pca_loadings_wider %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_segment(aes(xend = PC1, yend = PC2),
               x = 0, 
               y = 0,
               arrow = arrow_style) +
  ggrepel::geom_text_repel(aes(x = PC1, y = PC2, label = terms),
                           hjust = 0, 
                           vjust = 1,
                           size = 4,
                           color = "deepskyblue4") +
  labs(title = "Loadings on PCs 1 and 2 for normalized data") +
  theme_classic()

pca_loadings_only

loadings_top_8 


# check raw data
glimpse(durian)

# PC 1
pc1_raw <- durian %>% 
  filter(peak_no %in% c("peak_19",
                        "peak_12",
                        "peak_6",
                        "peak_34",
                        "peak_2",
                        "peak_10",
                        "peak_14",
                        "peak_26"))



# PC 2
pc2_raw <- durian %>% 
  filter(peak_no %in% c("peak_11",
                        "peak_15",
                        "peak_8",
                        "peak_37",
                        "peak_36",
                        "peak_1",
                        "peak_32",
                        "peak_24"))

# PC 3
pc3_raw <- durian %>% 
  filter(peak_no %in% c("peak_3"))

pc1_raw %>% arrange(peak_no)
pc2_raw %>%  arrange(peak_no)
pc3_raw %>%  arrange(peak_no)

# plot by viz

glimpse(durian)

d101 <- durian %>% 
  select(-peak_no, -odor_description, - category) %>% 
  pivot_longer(cols = starts_with("d"),
               names_to = "variety",
               values_to = "concentration") %>% 
  filter(variety == "d101") %>% 
  top_n(10, concentration) %>% 
  ggplot(aes(fct_reorder(compound, concentration), concentration)) +
  geom_col(fill = "goldenrod") +
  labs(x = NULL,
       title = "D101",
       caption = "Chin et al, 2007") +
  coord_flip() +
  theme_classic() +
  theme(title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14))
  
d2 <- durian %>% 
  select(-peak_no, -odor_description, - category) %>% 
  pivot_longer(cols = starts_with("d"),
               names_to = "variety",
               values_to = "concentration") %>% 
  filter(variety == "d2") %>% 
  top_n(10, concentration) %>% 
  ggplot(aes(fct_reorder(compound, concentration), concentration)) +
  geom_col(fill = "forestgreen") +
  labs(x = NULL,
       title = "D2",
       caption = "Chin et al, 2007") +
  coord_flip() +
  theme_classic() +
  theme(title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14))

d24 <- durian %>% 
  select(-peak_no, -odor_description, - category) %>% 
  pivot_longer(cols = starts_with("d"),
               names_to = "variety",
               values_to = "concentration") %>% 
  filter(variety == "d24") %>% 
  top_n(10, concentration) %>% 
  ggplot(aes(fct_reorder(compound, concentration), concentration)) +
  geom_col(fill = "darkorange2") +
  labs(x = NULL,
       title = "D24",
       caption = "Chin et al, 2007") +
  coord_flip() +
  theme_classic() +
  theme(title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14))

gridExtra::grid.arrange(d101, d2, d24, ncol = 3)

# 
glimpse(durian)

durian %>% 
  select(-peak_no, -odor_description) %>% 
  pivot_longer(cols = starts_with("d"),
               names_to = "variety",
               values_to = "concentration") %>% 
  group_by(category, variety) %>% 
  summarize(sum_conc = sum(concentration)) %>% 
  arrange(desc(sum_conc))
