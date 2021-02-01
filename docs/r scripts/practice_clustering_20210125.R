# Cluster analysis

# Although PCA may reveal groups of like objects, it is not always
# successful in doing do. 

# Cluster analysis: a method for dividing a group of objects into 
# classes so that similar objects are in the same class.
# Cluster analysis searches for objects which are close
# together in the variable space (Euclidian distance)

# Unsupervised method

# Data must be standardised

# Hierarchical: once an object has been assigned to a group, the 
# process cannot be reversed. 

# k-means method: non-hierarchical method, points may be divided
# into k number of clusters. THe final grouping reflects the initial
# choice of clusters. Another disadvantage is that the value of k
# has to be chosen in advance. 

library(mokken) # conducts mokken scale analysis
library(haven) # read spss/stata/sas data
library(tidyverse)


gss_panel <- read_dta("https://talktoroh.squarespace.com/s/gss_panel06.dta")
gss_panel

glimpse(gss_panel)

gss_cleaned <- gss_panel %>% 
  dplyr::select(starts_with("suicide")) %>% 
  dplyr::select(ends_with("_1")) %>% 
  mutate_all(funs(ifelse (. == 1, 1, 0))) %>%
  drop_na() # 1235 obs of 4 var


glimpse(gss_cleaned) # suicide

# To calculate scalability coefficients H
mokken::coefH(as.data.frame(gss_cleaned))$H  # 0.927 --> Strongly scalable


## k-means clustering ######
# must be continuous
# must not have missing values
# must be standardised

library(pacman)
p_load(tidyverse, ggthemes, haven, factoextra, cluster, skimr)

# IMPORT ######
education_seoul <- read_csv("https://talktoroh.squarespace.com/s/Education_Seoul_2004_2016.csv")

glimpse(education_seoul)

as_tibble(education_seoul)

# TRANSFORM #####

education_seoul_2016 <- education_seoul %>% 
  filter(district != "total",
         year == 2016) %>% 
  dplyr::select(district,starts_with("average_class_size"))

glimpse(education_seoul_2016)

names(education_seoul_2016) <- c("district",
                                 "kindergarten",
                                 "elementary",
                                 "middle",
                                 "high")

# Try PCA #####
skim(education_seoul_2016) # no missing values, need to normalise

# correlation

ggcorr(education_seoul_2016, label = T, label_alpha = T,
       label_round = 2) # quite correlated

education_no_district <- education_seoul_2016 %>% 
  dplyr::select(-district)

# KMO
education_no_district %>% 
  cor() %>% 
  psych::KMO() # 0.68

# Bartlett

education_no_district %>% 
  cor() %>% 
  psych::cortest.bartlett(., n = 25) # p<0.05

# Tidymodels PCA ######

# 1. recipe #####
edu_recipe <- recipe( ~., data = education_seoul_2016) %>% 
  update_role(district, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") # step 3

# 2. preparation #####

edu_prep <- prep(edu_recipe)

edu_prep

# 3. bake #####

edu_bake <- bake(edu_prep, education_seoul_2016)
edu_bake

# check number of PC ######

# (a) Eigenvalues
edu_prep$steps[[3]]$res$sdev # 1

# (b) Scree plot

edu_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:4) +
  labs(title = "Scree plot: % Variance explained",
       y = "% total variance",
       x = "PC") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")) # 2

# (c) Parallel analysis
psych::fa.parallel(cor(education_no_district),
                   n.obs = 25,
                   cor  = "cor",
                   plot = T) # 1


# visualize loadings #####

learntidymodels::plot_top_loadings(edu_prep,
                                   component_number<=3, n = 5) +
  scale_fill_manual(values = c("deepskyblue4", "darkorange")) +
  theme_minimal() # doesn't look like can separate cleanly

# visualize scores #####

pc1pc2_scores_plot <- edu_bake %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(col = district), alpha = 0.8, size = 2) +
  theme_classic() +
  theme(legend.position = "none")

pc1pc2_scores_plot 

# visualize loadings #####
tidy_pca_loadings <- edu_prep %>% 
  tidy(id = "pca")

tidy_pca_loadings # values here are the loading


# define arrow style
arrow_style <- arrow(angle = 30,
                     length = unit(0.2, "inches"),
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
                           size = 5,
                           color = "red") +
  labs(title = "Loadings on PCs 1 and 2 for normalized data") +
  theme_classic()  # strange


# k-means

# check for missing values
sapply(education_no_district, function(x) sum(is.na(x)))

# older way
library(factoextra)
fviz_nbclust(education_no_district)


education_no_district %>% fviz_nbclust(.,
                                     kmeans,
                                     method = "wss") # within sum square
# elbow = 3, 4

# a novel way #####
library(cluster)

# clusGap() calculates a goodness of clustering measure, the “gap” statistic. 
gap_statistic <- cluster::clusGap(education_no_district,
                                  FUN = kmeans,
                                  nstart = 50,
                                  K.max = 10,
                                  B = 1000)

fviz_gap_stat(gap_statistic)
# choose the point that drops then increase
# ie cluster = 4

# visualization #####

kcluster4 <- education_no_district %>% 
  kmeans(4, nstart = 50)

glimpse(education_no_district)

# add in row names
rownames(education_no_district) <- education_seoul_2016$district

kcluster4 %>% 
  fviz_cluster(data = education_no_district,
               geom = "text") +
  theme_bw() +
  theme(legend.position = "none") # can see the 4 clusters

#  TIDYMODELS kmeans ######
# https://www.tidymodels.org/learn/statistics/k-means/

kclust <- kmeans(education_no_district, centers = 4)
kclust 

# add the classifications to existing dataset
augment(kclust, education_no_district) 

# summarise on a per-cluster level
tidy(kclust)

# extract a single row summary
glance(kclust)

# exploring different k numbers #####
kclusts_explore <- tibble(k = 1:10) %>% 
  mutate(kclust = purrr::map(k, ~kmeans(education_no_district, .x)),
         tidied = purrr::map(kclust, tidy),
         glanced = purrr::map(kclust, glance),
         augmented = purrr::map(kclust, augment, education_no_district))

# turn this into 3 separate datasets, each representing a
# different type of data

#
clusters <- kclusts_explore %>% 
  unnest(cols = c(tidied))

clusters

#
assignments <- kclusts_explore %>% 
  unnest(cols = c(augmented))

assignments  # can be used to plot, with each point colored according to predicted cluster

#
clusterings <- kclusts_explore %>% 
  unnest(cols = c(glanced))
clusterings

#  visualize

# number of clusters
clusterings %>% 
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Plot of Total Within Sum of Squares for different number of clusters",
       subtitle = "Additional clusters beyond k = 4 have little value") +
  theme_classic()


# how datapoints are separated
assignments %>% 
  ggplot(aes(x = middle, y = high)) +
  geom_point(aes(color = .cluster), size = 2, alpha = 0.8) +
  facet_wrap( ~ k) +
  # to see the center of the clusters
  geom_point(data = clusters, size = 6, shape  = "x") +
  theme_bw()


  