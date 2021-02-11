
# Tidy tuesdays coffee rating

# Load packages
library(pacman)
p_load(tidyverse,skimr,tidytuesdayR, ggthemes, GGally, broom)


#coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
coffee_ratings <- tuesdata$coffee_ratings

glimpse(coffee_ratings)
skim(coffee_ratings)

# how many species of coffee?
coffee_ratings %>% 
  select(species) %>% 
  unique()

# what is the distribution?

coffee_ratings %>% 
  select(species) %>% 
  count(species) %>% # equivalent to df %>% group_by(a, b) %>% summarise(n = n()).
  mutate(percentage = n/sum(n)*100) %>%  # need not group by first
  ggplot(aes(species,percentage)) +
  geom_col(aes(fill = species)) +
  scale_fill_few() + # ggthemes: Color scales from Few's "Practical Rules for Using Color in Charts"
  labs(title = "Distribution of Arabica and Robusta coffe",
       subtitle = "Most of the coffee graded are Arabica coffee",
       x = "Species",
       y = "Percentage of samples",
       caption = "Source: Coffee Quality Institute") +
  theme_clean()

# Maybe I should just look at Arabica coffee ratings

glimpse(coffee_ratings)

# Arabica beans has higher ratings than Robusta
coffee_ratings %>% 
  select(species,total_cup_points) %>% 
  group_by(species) %>% 
  summarise(mean = mean(total_cup_points)) %>% 
  ggplot(aes(x = species, y = mean, label = round(mean,1))) +
  geom_col(aes(fill = species)) +
  geom_text(aes(label = round(mean,1)), vjust = -0.5) +
  scale_fill_few() +
  labs(title = "Mean Total Cup Points for Arabica and Robusta",
       subtitle = "Arabica has higher mean score than Robusta",
       caption  = "Source: Coffee Quality Institute") +
  theme_clean()
  
glimpse(coffee_ratings)

# Let's just focus on Arabica beans
# What are the countries of origin and if there is any diff in mean cup score?

arabica <- coffee_ratings %>% 
  filter(species == "Arabica")

glimpse(arabica)

# check missing values for coffee_ratings
sapply(arabica, function(x) sum(is.na(x))) # 1 missing value for country

# what is the mean score for coffee rating?
mean(arabica$total_cup_points) #82.11

# what is the median score?
median(arabica$total_cup_points) # 82.5

# what is the distribution for ratings like?
arabica %>% 
  select(total_cup_points) %>% 
  ggplot(aes(total_cup_points)) +
  geom_density(fill = "coral") +
  labs(title = "Distribution of Ratings for Arabica Coffee",
       subtitle = "Ratings are right skewed",
       caption = "Source: Coffee Quality Institute") +
  theme_few()

# in this case, it's ok to use either mean or median, and I will use mean

# plot to see which countries are above/below mean rating

arabica_dotplot <- arabica %>% 
  filter(!is.na(country_of_origin)) %>% # 1 missing value
  group_by(country_of_origin) %>% 
  summarise(mean_rating = mean(total_cup_points)) %>% 
  mutate(above_below_mean = as.factor(ifelse(mean_rating > mean(arabica$total_cup_points),
                                             "above_mean", "below_mean"))) %>% 
  ggplot(aes(x = reorder(country_of_origin, mean_rating), 
             y = mean_rating, 
             col = above_below_mean,
             label = round(mean_rating,1))) +
  geom_point(aes(col = above_below_mean), stat = "identity", size = 9) +
  scale_color_few() +
  geom_text(col = "black", size = 4) +
  geom_hline(aes(yintercept = mean(arabica$total_cup_points)), size = 2,
             col = "grey")+
  labs(title = "Dot plot for Arabica Coffee Ratings",
       subtitle = "Countries with ratings above mean values are coloured blue, and countries below mean values are colored orange.",
       x =  "Country of Origin",
       y = "Mean Rating",
       caption = "Source: Coffee Quality Institute") +
  coord_flip() +
  theme_clean() +
  theme(legend.position = "none",
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        title = element_text(size = 20, face = "bold"))

arabica_dotplot

# how does processing method affect coffee ratings?

arabica %>% 
  select(processing_method) %>% 
  unique()

# from website:
# there are three types of processing methods:
# a. washed: focus solely on the bean, most specialty coffees are washed.
#            needs to have enough natural sugars inherently. Fruit flesh
#              is removed from the bean before beans are dried. 
#            leads to bright and acidic flavors in the cup. 
# b. natural: fruit remains on the bean and dries undisturbed, 
#             considered a lower quality method that can lead to inconsistent flavors
#             due to unripe fruit drying and turning brown alongside ripe fruits
# c. honey: often has a rounded acidity than washed coffees, with intense
# sweetness and complex mouthfeel. Strongly associated with Costa Rica
# d. Other processing methods:
# anaerobic, carbonic maceration, giling basah

# breakdown by processing methods
arabica %>% 
  filter(!is.na(processing_method)) %>% 
  count(processing_method) %>% 
  mutate(percentage = n/sum(n)*100) %>% 
  arrange(desc(percentage)) %>% 
  ggplot(aes(reorder(processing_method, percentage),percentage),
         label = round(percentage,1)) +
  geom_col(aes(fill = processing_method), width = 0.75) +
  scale_color_few() +
  geom_text(aes(label = round(percentage,1), hjust = -0.15)) +
  labs(title = "Distribution by Processing Method",
       subtitle = "Most of the Arabica Coffee were either Wet or Dry Processed",
       caption = "Source: Coffee Quality Institute",
       x = NULL) +
  coord_flip() +
  theme_clean() +
  theme(legend.position = "none")

# how are the ratings like when you account for country of origin, 
# and look at either wet or dry processed

arabica %>% 
  select(country_of_origin, processing_method, total_cup_points) %>% 
  group_by(country_of_origin) %>% 
  filter(processing_method %in% c("Washed / Wet", "Natural / Dry")) %>% 
  group_by(country_of_origin, processing_method) %>% 
  mutate(mean_ratings = mean(total_cup_points)) %>% 
  ggplot(aes(x = reorder(country_of_origin, mean_ratings), y = mean_ratings,
             group = processing_method)) +
  geom_point(aes(col = processing_method)) +
  scale_color_few() +
  facet_grid(processing_method ~.) +
  labs(title = "Comparing Mean Ratings for Arabica Coffee for Dry Processing and Wet Processing",
       subtitle = "Mean ratings were higher for Wet Processing",
       x = NULL,
       caption = "Source: Coffee Quality Institute") +
  coord_flip() +
  theme_clean() +
  theme(legend.position = "none",
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        title = element_text(size = 20, face = "bold"))


# Check the scores

glimpse(coffee_ratings)

sensory <- coffee_ratings %>% 
  select(total_cup_points, species, country_of_origin,
         processing_method:category_two_defects)

glimpse(sensory)

sensory %>% ggcorr(label = T, label_alpha = T, label_round = 2)

summary(sensory)

# category for total_cup_points
# 95 - 100: Super Premium Specialty
# 90 - 94: Premium Specialty
# 85 - 89: Specialty
# 80 - 84: Premium
# 75 - 79: Usual Good Quality
# 70 - 74: Average Quality
# 60 - 70: Exchange grade
# 50 - 60: Commercial grade

sensory %>% 
  ggplot(aes(total_cup_points)) +
  geom_histogram()

min(sensory$total_cup_points)  # 0 : means got missing values

table(sensory$total_cup_points) # 1 missing value, lowest is 59.83


sensory_with_category <- sensory %>% 
  filter(total_cup_points != 0) %>% # remove zero score
  mutate(classification = ifelse(total_cup_points > 95, "Super Premium Specialty",
                                 ifelse(total_cup_points >90, "Premium Specialty",
                                        ifelse(total_cup_points >85, "Specialty",
                                               ifelse(total_cup_points >80, "Premium",
                                                      ifelse(total_cup_points >75, "Usual Good Quality",
                                                             ifelse(total_cup_points >70, "Average Quality",
                                                                    ifelse(total_cup_points >60, "Exchange grade",
                                                                           "Commercial grade"))))))))
# just to check
sensory_with_category %>% 
  select(total_cup_points, classification) %>% 
  arrange(desc(total_cup_points))

min(coffee_ratings$total_cup_points)

# which coffee had the highest score?
coffee_ratings %>% 
  filter(total_cup_points == 90.58) %>% 
  t() # transpose

max(coffee_ratings$total_cup_points)

# which coffee had the lowest score?
coffee_ratings %>% 
  filter(total_cup_points == 59.83) %>% 
  t() # transpose

#
sensory_with_category %>% 
  select(total_cup_points, classification) %>% 
  arrange(total_cup_points)

# distribution of types of coffee
sensory_with_category %>% 
  filter(species == "Arabica",
         processing_method %in% c("Natural / Dry", "Washed / Wet")) %>% 
  count(classification, processing_method) %>% 
  ggplot(aes(fct_reorder(classification, n), n, label = n)) + 
  geom_col(aes(fill = classification)) +
  scale_color_few() +
  labs(title = "Distribution of types of Arabica coffees, by processing method",
       subtitle = "Most of the premium coffee (with cup scores 80 - 84) are processed by Washed/Wet method.",
       caption = "Source: Coffee Quality Institute") +
  facet_grid(processing_method ~. ) +
  theme_clean() +
  coord_flip() +
  theme(legend.position = "none")


# established that most of the coffee is arabica
# types of processing dominated by natural/dry and washed/wet
# should just focus on arabica, wet vs dry: sensory scores


glimpse(sensory_with_category)

plot_sensory <- sensory_with_category %>% 
  filter(classification == "Premium",
         species == "Arabica",
         processing_method %in% c("Natural / Dry", "Washed / Wet")) %>% 
  mutate(processing_mtd_fct = ifelse(processing_method == c("Natural / Dry"), "Dry",
                                                            "Wet")) %>% 
  select(-quakers, -color, - category_one_defects, 
         - category_two_defects, - processing_method) %>% 
  pivot_longer(cols = aroma:cupper_points,
               names_to = "parameters",
               values_to = "score") %>% 
  mutate(parameters_fct = factor(parameters,
                                 levels = c("acidity", "aroma", "clean_cup",
                                            "sweetness", "uniformity", "aftertaste",
                                            "balance", "body", "cupper_points", "flavor"
                                            ))) %>% 
  group_by(processing_mtd_fct, parameters_fct) %>% 
  summarise(mean_score = mean(score)) %>% 
  ggplot(aes(x = processing_mtd_fct, y = mean_score)) +
  geom_point(aes(col = processing_mtd_fct), size = 2) +
  scale_color_manual(values = c("Dry" = "chocolate4",
                                "Wet" = "cadetblue4")) +
  facet_wrap(vars(parameters_fct), scales = "free", ncol= 5) +
  labs(x = NULL,
       title = "Comparison of mean score for Arabica coffee: Dry vs Wet Processing",
       subtitle = "Wet processed coffee has higher acidity, aroma, clean_cup, sweetness, uniformity.",
       caption = "Source: Coffee Quality Institute") +
  theme_few() +
  theme(legend.position = "none")

plot_sensory
glimpse(plot_sensory)


# mean total cup 

plot_sensory_total <- sensory_with_category %>% 
  filter(classification == "Premium",
         species == "Arabica",
         processing_method %in% c("Natural / Dry", "Washed / Wet")) %>% 
  mutate(processing_mtd_fct = ifelse(processing_method == c("Natural / Dry"), "Dry",
                                     "Wet")) %>% 
  select(total_cup_points, processing_mtd_fct) %>% 
  group_by(processing_mtd_fct) %>% 
  summarise(mean_score = mean(total_cup_points)) %>% 
  ggplot(aes(x = processing_mtd_fct, y = mean_score)) +
  geom_point(aes(col = processing_mtd_fct), size = 2) +
  scale_color_manual(values = c("Dry" = "chocolate4",
                                "Wet" = "cadetblue4")) +
  labs(x = NULL,
       title = "Comparison of average total cup points for Arabica coffee: Dry vs Wet Processing",
       subtitle = "Both processing methods had similar average total cup points.",
       caption = "Source: Coffee Quality Institute") +
  theme_few() +
  theme(legend.position = "none")

plot_sensory_total
glimpse(plot_sensory_total)

# should show as boxplot

plot_sensory_total_boxplot <- sensory_with_category %>% 
  filter(classification == "Premium",
         species == "Arabica",
         processing_method %in% c("Natural / Dry", "Washed / Wet")) %>% 
  mutate(processing_mtd_fct = ifelse(processing_method == c("Natural / Dry"), "Dry",
                                     "Wet")) %>% 
  select(total_cup_points, processing_mtd_fct) %>% 
  ggplot(aes(x = processing_mtd_fct, y = total_cup_points)) +
  geom_boxplot(aes(col = processing_mtd_fct),notch = T) +
  stat_summary(fun.data = "mean_cl_normal",
           geom = "errorbar",
           fun.args = (conf.int = 0.95),
           color = "forestgreen") +
  geom_jitter(aes(col = processing_mtd_fct), alpha = 0.3) +
  scale_color_manual(values = c("Dry" = "chocolate4",
                                "Wet" = "cadetblue4")) +
  labs(title = "Comparison of Mean Total Cup Points for Dry vs Wet Processing in Arabica Coffee",
       subtitle = "The Mean Total Cup Points are very similar for both processing methods",
       caption = "Source: Coffee Quality Institute",
       x = "Processing Method",
       y = "Total Cup Points") +
  theme_few() +
  theme(legend.position = "none")

glimpse(plot_sensory_total_boxplot)
plot_sensory_total_boxplot 


# individual
plot_sensory_boxplot <- sensory_with_category %>% 
  filter(classification == "Premium",
         species == "Arabica",
         processing_method %in% c("Natural / Dry", "Washed / Wet")) %>% 
  mutate(processing_mtd_fct = ifelse(processing_method == c("Natural / Dry"), "Dry",
                                     "Wet")) %>% 
  select(-quakers, -color, - category_one_defects, 
         - category_two_defects, - processing_method) %>% 
  pivot_longer(cols = aroma:cupper_points,
               names_to = "parameters",
               values_to = "score") %>% 
  mutate(parameters_fct = factor(parameters,
                                 levels = c("acidity", "aroma", "clean_cup",
                                            "sweetness", "uniformity", "aftertaste",
                                            "balance", "body", "cupper_points", "flavor"
                                 ))) %>% 
  ggplot(aes(x = processing_mtd_fct, y = score)) +
  geom_boxplot(aes(col = processing_mtd_fct), notch = T, size = 1) +
  geom_jitter(aes(col = processing_mtd_fct), alpha = 0.1) +
  scale_color_manual(values = c("Dry" = "chocolate4",
                                "Wet" = "cadetblue4")) +
  facet_wrap(vars(parameters_fct), scales = "free", ncol= 5) +
  labs(x = NULL,
       title = "Comparison of mean score for Arabica coffee: Dry vs Wet Processing",
       subtitle = "Wet processed coffee has higher average scores for acidity, aroma, clean_cup, sweetness, uniformity.",
       caption = "Source: Coffee Quality Institute") +
  theme_few() +
  theme(legend.position = "none")

plot_sensory_boxplot
glimpse(plot_sensory_boxplot)

# sensory scores for arabica coffee, top scorers for sensory

sensory_by_country <- coffee_ratings %>% 
  filter(species == "Arabica",
         !total_cup_points %in% 0,
         !is.na(country_of_origin),
         !is.na(owner)) %>% 
  select(country_of_origin, owner, 
         total_cup_points, aroma:cupper_points)

skim(sensory_by_country)
  
# there is one datapoint in which clean_cup score = 0
coffee_ratings %>% 
  filter(clean_cup == 0) %>% 
  t() # transpose

# one is missing value, already filtered out for total_cup_points = 0
# the remaining one looks like it really has 0 for clean cup score

7.08 + 6.83 + 6.25 + 7.42 + 7.25 + 6.75 + 10 + 10  + 6.75 # 68.33


# check for other scores

coffee_ratings %>% 
  slice(2) %>% 
  t()

8.67 + 8.83 + 8.67 + 8.75 + 8.5 + 8.42 + 10 + 10 + 10  + 8.75 # 90.58

# 89.92
8.75 + 8.67 + 8.5 + 8.58 + 8.42 + 8.42 + 10 + 10 + 10 + 8.58

glimpse(sensory_by_country)

country_mean_score <- sensory_by_country %>% 
  group_by(country_of_origin, owner) %>% 
  summarise(mean_score = mean(total_cup_points)) %>% 
  arrange(desc(mean_score)) 

country_mean_score

min(country_mean_score$mean_score) # 68.33
max(country_mean_score$mean_score) # 89.7767

# plot profile for top 10 owners


sensory_by_country

glimpse(sensory_by_country) 

# dot plot
top_owners_data <- sensory_by_country%>% 
  group_by(country_of_origin, owner) %>% 
  summarise_at(.vars = vars(total_cup_points:cupper_points),
               .funs = c(mean = "mean")) %>% 
  ungroup() %>% 
  mutate(country_owner = str_c(country_of_origin, owner, sep = ","),
         country_owner_fct = factor(country_owner, 
                                    levels =c("Ethiopia,metad plc", 
                                             "Guatemala,grounds for health admin", 
                                             "Ethiopia,yidnekachew dabessa",
                                             "Brazil,ji-ae ahn",
                                             "Peru,hugo valdivia",
                                             "Ethiopia,diamond enterprise plc",
                                             "Ethiopia,mohammed lalo",
                                             "Indonesia,grounds for health admin"))) %>% 
  group_by(country_owner_fct) %>% 
  arrange(desc(total_cup_points_mean)) %>% 
  ungroup() %>% 
  slice_max(total_cup_points_mean, n = 8) %>% 
  pivot_longer(cols = c(aroma_mean:cupper_points_mean),
               names_to = "parameters",
               values_to = "score") %>% 

  ggplot(aes(x = fct_rev(factor(parameters)), y = score, label = round(score, 1))) +
  geom_point(stat = "identity", aes(col = factor(parameters)), size = 8) +
  geom_text(col = "black", size = 4) +
  facet_wrap(country_owner_fct~., scales = "free_y", ncol = 4) +
  coord_flip() +
  theme_few() +
  theme(legend.position = "none")
  
  
top_owners_data  
glimpse(top_owners_data)

#
top_owners_data %>% 
  select(owner) %>% 
  unique()

country_ownder_order <- sensory_by_country%>% 
  group_by(country_of_origin, owner) %>% 
  summarise_at(.vars = vars(total_cup_points:cupper_points),
               .funs = c(mean = "mean")) %>% 
  ungroup() %>% 
  mutate(country_owner = str_c(country_of_origin, owner, sep = ","),
         country_owner_fct = factor(country_owner)) %>% 
  group_by(country_owner_fct) %>% 
  arrange(desc(total_cup_points_mean)) %>% 
  ungroup() %>% 
  slice_max(total_cup_points_mean, n = 8) %>% 
  select(country_owner) %>% 
  as.character()

country_ownder_order

# types of coffee (variety)

glimpse(coffee_ratings)

variety_count <- coffee_ratings %>% 
  count(variety) %>% 
  arrange(desc(n)) # 30 observations

head(variety_count, 8) # NA: 226, Other: 226
tail(variety_count)

data_variety <- coffee_ratings %>% 
  select(total_cup_points, species, owner, country_of_origin, processing_method,
         variety, aroma:cupper_points, color) %>% 
  filter(variety %in% c("Caturra", "Bourbon", "Typica", "Catuai", 
                        "Hawaiian Kona", "Yellow Bourbon")) %>% 
  group_by(variety)

glimpse(data_variety)


data_variety %>% 
  count(species) # all arabica

data_variety %>% 
  count(processing_method) # quite varied

data_variety %>% 
  ungroup() %>% 
  count(country_of_origin) %>% 
  arrange(desc(n))

# see sensory ratings across variety
glimpse(data_variety)


data_variety %>% 
  ungroup() %>% 
  group_by(variety) %>% 
  skim()



# boxplot
data_variety %>% 
  select(variety, total_cup_points) %>% 
  filter(total_cup_points != 0) %>% 
  ggplot(aes(fct_reorder(variety, total_cup_points), total_cup_points)) +
  geom_boxplot(aes(col = variety), show.legend = F) +
  labs(title = "Comparison of Total Cup Points across top 6 varieties (by count)",
       subtitle = "Caturra has the higest mean Total Cup Score",
       x = "Variety",
       y = "Total Cup Points",
       caption = "Source: Coffee Quality Institute") +
  geom_jitter(aes(col = variety), alpha = 0.2, show.legend = F) +
  scale_color_few() +
  coord_flip()  +
  theme_few()

# dot plot by variety
glimpse(data_variety)

dot_plot_variety <- data_variety %>% 
  filter(total_cup_points != 0) %>% 
  select(variety, aroma:cupper_points) %>% 
  group_by(variety) %>% 
  summarise(across(c(aroma:cupper_points), mean)) %>% 
  pivot_longer(cols = c(aroma:cupper_points),
               names_to = "parameters",
               values_to = "score") %>% 
  ggplot(aes(x = fct_reorder(factor(variety), score), y = score, label = round(score, 1))) +
  geom_point(stat = "identity", aes(col = factor(variety)), size = 8) +
  geom_text(col = "black", size = 4) +
  facet_wrap(parameters~., scales = "free", ncol = 4) +
  labs(title = "Breakdown of scoring criteria for top 5 coffee (by count)",
       subtitle = "Scores were quite close for all categories, within +/- 0.2. 
Main areas of differences were in balance, clean_cup, cupper_points, sweetness, uniformity",
       caption = "Source: Coffee Quality Institute",
       x = "Variety",
       y = "Score") +
  coord_flip() +
  theme_few() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))

dot_plot_variety

glimpse(dot_plot_variety)

# References
# https://perfectdailygrind.com/2016/07/washed-natural-honey-coffee-processing-101/
# https://www.baristainstitute.com/blog/jori-korhonen/january-2020/coffee-processing-methods-drying-washing-or-honey
# https://www.coffeechemistry.com/cupping-fundamentals
# https://www.data-to-viz.com/caveat/spider.html

# write.csv(coffee_ratings, "data-coffee_ratings.csv")

