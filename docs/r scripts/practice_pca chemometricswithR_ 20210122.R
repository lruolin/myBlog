# PCA Chemometrics with R - wines dataset

# install_github("rwehrens/ChemometricsWithR″ )

library(ChemometricsWithR)

library(pacman)
p_load(corrr, palmerpenguins, GGally, tidymodels, tidytext, tidyverse, psych,
       skimr, gridExtra, kohonen, janitor, learntidymodels)


# IMPORT #####
library(kohonen)
data(wines)

wines <- as.data.frame(wines) %>% 
  janitor::clean_names() %>%  # require data.frame
  as_tibble() %>% 
  cbind(vintages)  # vintages = Y outcome = category
 
glimpse(wines)

skim(wines) # 177 x 13, all numeric + Y outcome



# types of wines
wines %>% 
  select(vintages) %>% 
  unique()  # Barolo, Grignolino, Barbera

# EXPLORATORY #####

wines %>% 
  select(-vintages) %>% 
  ggcorr(label = T, label_alpha = T, label_round = 2)

wines %>% 
  ggpairs(aes(col = vintages))


# CHECK: 
# Continuous Y
# No missing data


# TRANSFORM #####
# Check assumptions for PCA #####
wines_no_y <- wines %>% 
  select(-vintages)

glimpse(wines_no_y)

wines_no_y %>% 
  cor() %>% 
  KMO() # .70 above : YES


# Bartlett's test of sphericity ----
wines_no_y %>% 
  cor() %>% 
  cortest.bartlett(., n = 177) # p<0.05

# PCA #####

# 1. recipe ######
wines_recipe <- recipe(~ ., data = wines) %>% 
  update_role(vintages, new_role = "id") %>% 
  # step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca")

wines_recipe # 13 predictors


# 2. prep #####

wines_prep <- prep(wines_recipe)

wines_prep # trained

tidy_pca_loadings <- wines_prep %>% 
  tidy(id = "pca")

tidy_pca_loadings # values here are the loading


# 3. bake ####
wines_bake <- bake(wines_prep, wines)
wines_bake  # has the PCA SCORES in normal form

# CHECK how many PC #####

# a. Eigenvalues
# data is stored in penguins_prep, step 3

wines_prep$steps[[2]]$res$sdev # 3

# b. Scree plot/Variance plot

wines_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms ==  "percent variance") %>% 
  ggplot(aes(x = component, y = value, label = round(value, 1))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:4) +
  labs(title = "% Variance explained",
       y = "% total variance",
       x = "PC",
       caption = "Source: ChemometricswithR book") +
  ggrepel::geom_text_repel(aes(label = round(value, 1))) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))  # 2 or 3

# bii: Cumulative variance plot

wines_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>%
  ggplot(aes(component, value)) +
  geom_col(fill= "forestgreen") +
  labs(x = "Principal Components", 
       y = "Cumulative variance explained (%)",
       title = "Cumulative Variance explained") +
  geom_text(aes(label = round(value, 2)), vjust = -0.2, size = 4) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")) 

# c. Parallel analysis

fa.parallel(cor(wines_no_y),
            n.obs = 333,
            cor = "cor",
            plot = T)  # 3


plot_loadings <- tidy_pca_loadings  %>% 
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>% 
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>% 
  ggplot(aes(abs(value), terms, fill = value>0)) +
  geom_col() +
  facet_wrap( ~component, scales = "free_y") +
  scale_y_reordered() + # appends ___ and then the facet at the end of each string
  scale_fill_manual(values = c("deepskyblue4", "darkorange")) +
  labs( x = "absolute value of contribution",
        y = NULL,
        fill = "Positive?",
        title = "PCA Loadings Plot",
        subtitle = "Number of PC should be 3, compare the pos and the neg",
        caption = "Source: ChemometricswithR") +
  theme_minimal()


plot_loadings

# alternate plot loadings

learntidymodels::plot_top_loadings(wines_prep,
                  component_number <= 4, n = 5) +
  scale_fill_manual(values = c("deepskyblue4", "darkorange")) +
  theme_minimal()


# PC1: flavonoids, tot_phenols, od_ratio, proanthocyanidins, col_hue, 36%
# PC2: col_int, alcohol, proline, ash, magnesium; 19.2%
# PC3: ash, ash_alkalinity, non_flav phenols; 11.2%
# PC4: malic acid?

# get pca loadings into wider format
pca_loadings_wider <- tidy_pca_loadings %>% 
  pivot_wider(names_from = component, id_cols = terms)


# Scores plot #####
# PCA SCORES are in bake

pc1pc2_scores_plot <- wines_bake %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = vintages, shape = vintages), 
             alpha = 0.8, size = 2) +
  scale_color_manual(values = c("deepskyblue4", "darkorange", "purple")) +
  labs(title = "Scores on PCs 1 and 2 for normalized data",
       x = "PC1 (36%)",
       y = "PC2 (19.2%)") +
  theme_classic() +
  theme(legend.position = "bottom") 

pc1pc2_scores_plot 


pc1pc2_scores_plot

# Loadings + scores plot #####
# does not work cos scale is different

# define arrow style 
arrow_style <- arrow(angle = 30,
                     length = unit(0.2, "inches"),
                     type = "closed")

# combined plot (does not work here) ######
pc1pc2_scores_and_loadings <- pc1pc2_scores_plot + 
  geom_segment(data = pca_loadings_wider,
               aes(xend = PC1, yend = PC2),
               x = 0, 
               y = 0,
               arrow = arrow_style) +
  geom_text(data = pca_loadings_wider,
            aes(x = PC1, y = PC2, label = terms),
            hjust = 0, 
            vjust = 1,
            size = 5,
            color = "forestgreen") +
  labs(title = "PC1 PC2 Scores + Loadings Plot") +
  theme(legend.position = "bottom")


pc1pc2_scores_and_loadings

# loadings only #####

# define arrow style 
arrow_style <- arrow(angle = 30,
                     length = unit(0.2, "inches"),
                     type = "closed")

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
  theme_classic()
  

pca_loadings_only

# final plots ######

pc1pc2_scores_plot
pca_loadings_only

grid.arrange(pc1pc2_scores_plot, pca_loadings_only, ncol = 2)

# check against data

wines %>% 
  group_by(vintages) %>% 
  summarise(across(c(flavonoids, col_int, ash, malic_acid),
                   mean,
                   na.rm = T))
                   


