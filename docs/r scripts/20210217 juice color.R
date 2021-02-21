# Colorimeter data analysis

# Overview of color analysis

# Objectives: to automate color data analysis

# Packages #####
library(pacman)
p_load(tidyverse, spacesXYZ, ggthemes, gridExtra)

# Import #####


data_l <- tribble(
  ~Juices, ~L_d0, ~L_d5, ~L_d10, ~L_d15, ~L_d30,
  #-------/------/-----/--------/------/-------
  "BJ",     22.45, 22.87, 22.31, 22.37, 24.16 , 
  "PBJ",    22.37, 22.71, 22.34, 22.23, 23.72,
  "POJ",    33.61, 38.18, 36.73, 37.04, 42.42,
  "BOMJ_1", 23.21, 23.76, 23.15, 23.14, 24.78,
  "BOMJ_2", 23.77, 24.33, 23.81, 24.15, 26.18
)


data_a <- tribble(
  ~Juices, ~a_d0, ~a_d5, ~a_d10, ~a_d15, ~a_d30,
  #-------/------/-----/--------/------/-------
  "BJ",     0.78, 0.68, 0.74, 0.81, 1.07,
  "PBJ",    0.95, 0.82, 0.87, 0.91, 1.19,
  "POJ",    -2.49, -2.87, -2.51, -2.63, -3.64,
  "BOMJ_1", 4.80, 5.18, 4.78, 4.59, 6.13,
  "BOMJ_2", 6.65, 7.07, 6.65, 6.68, 8.76
)


data_b <- tribble(
  ~Juices, ~b_d0, ~b_d5, ~b_d10, ~b_d15, ~b_d30,
  #-------/------/-----/--------/------/-------
  "BJ",    1.56, 1.52, 1.56, 1.57, 0.97, 
  "PBJ",   1.67, 1.61, 1.63, 1.64, 1.21,  
  "POJ",  16.34, 17.03, 16.46, 15.95, 18.34,
  "BOMJ_1", 2.39, 2.38, 2.35, 2.19, 2.23,
  "BOMJ_2", 2.75, 2.82, 2.68, 2.26, 2.47
)

# Transform #####
data <- bind_cols(data_l, data_a, data_b, .name_repair = "unique") %>% 
  select(-Juices...7, -Juices...13) %>% 
  rename(juices = Juices...1)

glimpse(data)

# Reshape data, so that data is in long shape

data_reshape_L <- data %>% 
  pivot_longer(cols = starts_with("L"),
               names_to = "days_L",
               values_to = "L_av") %>% 
  select(juices, days_L, L_av)



data_reshape_a <- data %>% 
  pivot_longer(cols = starts_with("a"),
               names_to = "days_a",
               values_to = "a_av") %>% 
  select(juices, days_a, a_av)
  
data_reshape_b <- data %>%   
  pivot_longer(cols = starts_with("b"),
               names_to = "days_b",
               values_to = "b_av") %>% 
  select(juices, days_b, b_av)


data_reshaped <- bind_cols(data_reshape_L, data_reshape_a, data_reshape_b) %>% 
  mutate(days = parse_number(days_L)) %>% 
  select(juices...1, days, L_av, a_av, b_av) %>% 
  rename(juices = juices...1)
  
glimpse(data_reshaped) # 25 x 5 obs

# Create function to calculate chroma and hue #####

cal_chroma <- function (a_av, b_av) {
  
  a_sq = a_av^2
  b_sq = b_av^2
  chroma = sqrt(a_sq + b_sq)
  
}

cal_hue <- function (a_av, b_av) {
  
  if(a_av > 0 & b_av > 0) {  # a pos, b pos
    hue = 180*(atan(b_av/a_av)/pi)
    
    
  }   else if (a_av<0 & b_av > 0) {  # a neg, b pos
    hue = 180 + 180*(atan(b_av/a_av)/pi)
    
    
  } else if (a_av<0 & b_av<0) {   # a neg, b neg
    hue = 180 + 180*(atan(b_av/a_av)/pi)
    
    
  } else {    # a pos, b neg
    hue = 360 + 180*(atan(b_av/a_av)/pi)
    
  }
  
}

# Calculate chroma and hue 

glimpse(data_reshaped)

data_transformed <- data_reshaped %>% 
  mutate(chroma = map2_dbl(.x = a_av,
                           .y = b_av,
                           .f = cal_chroma),
         hue = map2_dbl(.x = a_av,
                        .y = b_av,
                        .f = cal_hue))

glimpse(data_transformed) # compares well with table

# Create initial values tibble dataframe #####

initial <-  data_transformed %>% 
                      filter(days == 0) %>% 
                      select(L_av, a_av, b_av, chroma, hue) %>% 
                      rename(ini_L = L_av,
                             ini_a = a_av,
                             ini_b = b_av,
                             ini_chroma = chroma,
                             ini_hue = hue)

initial


data_transformed_b<- data_transformed %>% 
  group_by(juices) %>% 
  nest() %>% 
  bind_cols(initial) %>% 
  unnest(cols = c(data))

data_transformed_b

glimpse(data_transformed_b)

# calculate de2000 using spacesXYZ package, input must be as matrix

lab_meas <- as.matrix(data_transformed_b[, c("L_av", "a_av", "b_av")])
lab_ini <- as.matrix(data_transformed_b[, c("ini_L", "ini_a", "ini_b")])

data_de <- spacesXYZ::DeltaE(lab_ini, lab_meas, metric = 2000)
  


data_transformed_c <- data_transformed_b %>% 
  bind_cols(data_de) %>% 
  rename(de2000 = ...13) %>% 
  ungroup() # remove group by juices

# round off to 2 digits
data_transformed_c$de2000 <- round(data_transformed_c$de2000, digits = 2)

glimpse(data_transformed_c)

# threshold is de2000>2

above_threshold <- data_transformed_c %>% 
  filter(de2000>2) %>% 
  select(juices, days, de2000)

above_threshold  # POJ


# Visualization

glimpse(data_transformed_c)

data_viz_long <- data_transformed_c %>% 
  mutate(delta_L = L_av - ini_L,
         delta_a = a_av - ini_a,
         delta_b = b_av - ini_b,
         delta_chroma = chroma - ini_chroma,
         delta_hue = hue - ini_hue) %>% 
  select(juices, days, L_av:delta_hue) %>% 
  pivot_longer(cols = c(L_av:delta_hue),
               names_to = "parameters",
               values_to = "readings")
  

data_viz_long


# de2000

data_viz_long %>% 
  filter(parameters == "de2000") %>% 
  ggplot(aes(days, readings)) +
  geom_point(aes(col = juices), size = 2) +
  geom_line(aes(col = juices), size = 1) +
  scale_color_lancet() +
  geom_hline(yintercept = 2, col = "grey77", lty = 2) +
  labs(title = "Comparison of Total Color Difference (dE2000) when stored at 4degC for 30 days",
       x = "Days",
       y = "Calc. dE2000",
       subtitle = "Pure Orange Juice (POJ) had the greatest change in color. Addition of beet juice decreases change in color difference.",
       caption = "Source: Porto et al, 2017") +
  facet_wrap(~juices, ncol = 3) +
  theme_few() +
  theme(title = element_text(face = "bold", size = 16),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 14))
  
# Type of change in color?

viz_absolute <- data_viz_long %>% 
  filter(parameters %in% c("L_av", "a_av", "b_av", "chroma", "hue")) %>%
  mutate(parameters_fct = factor(parameters,
                                 levels = c("L_av", "a_av", "b_av", "chroma", "hue"))) %>% 
  ggplot(aes(days, readings, group = juices)) +
  geom_point(aes(col = juices), size = 2) +
  geom_line(aes(col = juices), size = 1) +
  scale_color_lancet() +
  labs(title = "Change in color over shelf life period",
       caption = "Source: Porto et al, 2017",
       col = "Juices") +
  facet_wrap( ~ parameters_fct, ncol = 5, scales = "free") +
  theme_few()+
  theme(title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold", size = 14),
        legend.position = "top")


# plot change in color parameters
glimpse(data_viz_long)

data_viz_long$parameters %>% unique()

viz_change <- data_viz_long %>% 
  filter(parameters %in% c("delta_L", "delta_a", "delta_b", "delta_chroma", "delta_hue")) %>% 
  mutate(parameters_fct = factor(parameters, 
                                 levels = c("delta_L", "delta_a", "delta_b", "delta_chroma", "delta_hue"))) %>% 
  ggplot(aes(days, readings, group = juices)) +
  geom_point(aes(col = juices), size = 2) +
  geom_line(aes(col = juices), size = 1) +
  scale_color_lancet() +
  labs(title = "Change in color over shelf life period",
       caption = "Source: Porto et al, 2017",
       col = "Juices") +
  facet_wrap( ~ parameters_fct, ncol = 5) +
  theme_few()+
  theme(title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold", size = 14),
        legend.position = "top")

grid.arrange(viz_absolute, viz_change, nrow = 2)


# References:

# https://www.mdpi.com/2306-5710/3/3/36
# https://www.xrite.com/blog/lab-color-space
# https://sensing.konicaminolta.us/us/blog/identifying-color-differences-using-l-a-b-or-l-c-h-coordinates/