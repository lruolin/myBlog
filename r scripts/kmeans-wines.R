# Practice for kmeans clustering on wine dataset

# PACKAGES ####
library(kohonen)
library(cluster)
library(factoextra)

# IMPORT #####
data(wines)
wines <- wines %>% 
  as_tibble() %>% 
  janitor::clean_names()


glimpse(wines)

# Wines PCA #### 

# PCA #####

# 1. recipe ######
wines_recipe <- recipe(~ ., data = wines) %>% 
  # step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca")

wines_recipe # 13 predictors

# 2. prep #####

wines_prep <- prep(wines_recipe)

wines_prep # trained

tidy_pca_loadings <- wines_prep %>% 
  tidy(id = "pca")

tidy_pca_loadings # values here are the loadings

# b. Scree plot/Variance plot

wines_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms ==  "percent variance") %>% 
  ggplot(aes(x = component, y = value, label = round(value, 1))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:13) +
  labs(title = "% Variance explained",
       y = "% total variance",
       x = "PC",
       caption = "Source: ChemometricswithR book") +
  ggrepel::geom_text_repel(aes(label = round(value, 1))) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))  # 3


# alternate plot loadings

learntidymodels::plot_top_loadings(wines_prep,
                                   component_number <= 3, n = 5) +
  scale_fill_manual(values = c("deepskyblue4", "darkorange")) +
  theme_minimal()

# 3. bake ####
wines_bake <- bake(wines_prep, wines)
wines_bake  # has the PCA SCORES in normal form

# k-means ######

# factoextra::fviz_nbclust(): Determines and visualize the optimal number 
# of clusters using different methods: within cluster sums of squares, 
# average silhouette and gap statistics.

# how many clusters? - using gap statistic method (newer method)
gap_statistic <- cluster::clusGap(wines_bake,
                                  FUN = kmeans,
                                  nstart = 50, 
                                  K.max = 10, # max number of clusters
                                  B = 1000) # bootstrap

factoextra::fviz_gap_stat(gap_statistic) # theoretically should have only 3 clusters?


# how many clusters - using the within sum of square method

fviz_nbclust(wines_bake,
             kmeans,
             method = "wss") # this suggests 3 clusters, in line with theory

# how many clusters? - silhouette method

fviz_nbclust(wines_bake,
             FUN = hcut,
             method = "silhouette") # this suggests 3 clusters

# visualization ########

kcluster3 <- wines_bake %>% 
  kmeans(centers = 3, # number of clusters, from textbook
         nstart = 50 ) # random sets to be chosen

kcluster3 %>% 
  factoextra::fviz_cluster(data = wines_bake) +
  theme_bw()


table(vintages,kcluster3$cluster) # looks ok


# tidymodels method #####

#  TIDYMODELS kmeans ######
# https://www.tidymodels.org/learn/statistics/k-means/
glimpse(wines)

wines_cluster <- cbind(wines, vintages) %>% 
  as_tibble

glimpse(wines_cluster)

# use original dataset
kclust <- kmeans(wines_bake, centers = 3)
kclust 

# add the classifications to existing dataset
test_augment <- augment(kclust, wines) 
glimpse(test_augment) # has additional .cluster column to show groupings


# summarise on a per-cluster level
test_tidy <- tidy(kclust)

glimpse(test_tidy)

# extract a single row summary
glance(kclust)

# exploring different k numbers #####
kclusts_explore <- tibble(k = 1:10) %>% 
  mutate(kclust = purrr::map(k, ~kmeans(wines_bake, .x)),
         tidied = purrr::map(kclust, tidy),
         glanced = purrr::map(kclust, glance),
         augmented = purrr::map(kclust, augment, wines_bake))

kclusts_explore

# turn this into 3 separate datasets, each representing a
# different type of data

#
clusters <- kclusts_explore %>% 
  unnest(cols = c(tidied))

clusters
glimpse(clusters)

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
clusterings %>% # from glance
  ggplot(aes(k, tot.withinss)) + # total within cluster sum of squares, keep low
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Plot of Total Within Sum of Squares for different number of clusters",
       subtitle = "Additional clusters beyond k = 3 have little value") +
  theme_classic()

# how datapoints are separated
glimpse(assignments)

assignments %>% # from augment
  ggplot(aes(x = PC1, y = PC2)) + # use PCA data
  geom_point(aes(color = .cluster), size = 2, alpha = 0.8) +
  facet_wrap( ~ k) +
  # to see the center of the clusters
  geom_point(data = clusters, size = 6, shape  = "x") +
  labs(x = "PC1 (36% variance)",
       y = "PC2 (19.2% variance",
       title = "Visualization of k-means clustering",
       subtitle = "Optimal k = 3",
       caption = "Source: Wines dataset from kohonen package")
  theme_minimal()

# hierarchical clustering #####
wines_HC <- wines_bake %>% 
    dist(.,method = "euclidean") %>% 
    hclust(., method = "ward.D2")

fviz_dend(wines_HC)  
  

# 3 clusters:
fviz_dend(wines_HC,
          k = 3,
          rect = T,
          rect_fill = T)

# references
# https://www.r-bloggers.com/2019/07/use-the-k-means-clustering-luke/
# # https://www.tidymodels.org/learn/statistics/k-means/
# https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/
# https://agroninfotech.blogspot.com/2020/06/visualizing-clusters-in-r-hierarchical.html