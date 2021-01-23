# practice PCA #

# Notes: ######
# PCA is an unsupervised technique for grouping variables.
# It can be used for exploratory work, feature extraction. 
# New predictors that are uncorrelated may be created for 
# machine learning purposes. 

# Workflow: #####
# 1. Check for missing values, impute if necessary
# 2. Exploratory data analysis: skimr
# 3. Check for correlation of variables
# 4. Centering and scaling (not influenced by original measurement scales)
# 5. Apply PCA - Determine number of components using 4 techniques
#     a. Kaiser's rule
#     b. Scree Plot
#     c. Parallel analysis
#     d. Domain knowledge
# 6. Apply PCA
# 7. Report results
# 8. Visualize 

# WINE dataset #####

# Load packages ######
library(pacman)
p_load(tidyverse, GGally, skimr, ggthemes, psych, highcharter)

# Import ####

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep = ",", strip.white = T) %>% 
  as_tibble()

glimpse(wine)

# need to set the col names
colnames(wine) <- c("class", "alcohol", "malic_acid", "ash",
                    "alkalinity_of_ash", "magnesium", "total_phenols",
                    "flavanoids", "nonflavanoid_phenols", "proanthocyanins",
                    "color_intensity", "hue", "OD280_OD315", "proline")

glimpse(wine)

# WINE Workflow: #####
# 1. Check for missing values, impute if necessary #####
skim(wine) # no missing values

# 2. Exploratory data analysis: GGally #####
ggpairs(wine) # can see that some variables are skewed

# 3. Check for correlation of variables #####
ggcorr(wine, label = T, label_alpha = T)

# 4. Centering and scaling (not influenced by original measurement scales) #####
# remove first col with groupings

wine_pca <- wine %>% 
  select(-1)

glimpse(wine_pca)

# 5. Apply PCA - Determine number of components using 4 techniques #####
#     a. Kaiser's rule
eigen_values <- wine_pca %>% prcomp(scale = T)

eigen_values <- eigen_values$sdev^2 # 14 features seen, 3 with values greater than 1

#     b. Scree Plot

eigen_values %>% 
  as_tibble() %>% 
  ggplot(aes(x = 1:13, y = value)) +
  geom_point(aes(size = value), col = "deepskyblue4", show.legend = F) +
  geom_line(col = "deepskyblue4") +
  scale_x_continuous(breaks = 1:13) +
  labs(y ="Eigen-values",
       x = "Principal Components (PCs)",
       title = "Scree plot for PCA",
       caption = "Source: UCI Machine Learning Repository") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 14))
  

#     c. Parallel analysis

fa.parallel(cor(wine_pca),
            n.obs = 178,
            cor = "cor",
            plot = T) # suggests that number of factors = 3

#     d. Domain knowledge
# YES there are 3 groups

# 6. Apply PCA #####
pca_step_1 <- wine_pca %>% 
  principal(.,
            nfactors = 3, 
            rotate = "promax")

pca_step_1

# remove malic_acid and magnesium

pca_step_2 <- wine_pca %>% 
  select(-malic_acid, -magnesium) %>% 
  principal(.,
            nfactors = 3, 
            rotate = "promax")


pca_step_2 # this is better

# 7. Report results #####


# 8. Visualize #####
hchart(princomp(wine_pca), cor = T)

# References: ####
# https://www.r-bloggers.com/2017/01/principal-component-analysis-in-r/

# tidymodels
# https://cmdlinetips.com/2020/06/pca-with-tidymodels-in-r/