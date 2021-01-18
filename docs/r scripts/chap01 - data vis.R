# CHAPTER 01 - DATA VISUALIZATION
setwd("~/Desktop/r-distill/myBlog/r scripts")

library(pacman)
p_load(tidyverse)

# MPG #####
mpg # to see dataset

?mpg # to query a dataset
 
glimpse(mpg)

# Create ggplot

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  labs( x = "Engine Size (displ)", y = "Fuel Efficiency (hwy)")

# The plot shows a negative relationship between engine size and
# fuel efficiency. In other words, cars with big engines use
# more fuel. 

# Scatterplot of hwl vs cyl

mpg %>% 
  ggplot(aes(x = cyl, y = hwy)) +
  geom_point()

# Aesthetic mappings #####
mpg %>% 
  ggplot(aes(x = displ, y = hwy, group = class)) +
  geom_point(aes(x = displ, y = hwy, col = class))

# can also map shape, alpha, size

mpg %>% 
  ggplot(aes(x = displ, y = hwy, group = class)) +
  geom_point(aes(x = displ, y = hwy), col = "blue")


# most of the unusual points are two seater cars

# Facets #####
mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap( ~ class, nrow = 2) # ~ must be discrete

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)


mpg %>% 
  ggplot(aes( x = displ, y = hwy)) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Faceting allows you to encode with more distinct categories
# however, comparing on different plots may be bad because
# you cannot see the datapoints on the same scale; even if
# you fix the scales, the difference is not as obvious
# as when they overlap together. 

# With facet_grid, you usually put the variable with more
# unique levels of the variables in the columns, so that there
# will be more space for columns if the plot is laid out
# horizontally (landscape)

# Geometric objects

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes( x = displ, y = hwy))

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_smooth(aes( x = displ, y = hwy))

mpg %>% 
  ggplot(aes(x = displ, y = hwy, group = drv)) +
  geom_point(aes(x = displ, y = hwy, col = drv)) +
  geom_smooth(aes(x = displ, y = hwy, linetype = drv,
                  col = drv))

mpg %>% 
  ggplot(aes(x = displ, y = hwy, col = drv)) +
  geom_point() +
  geom_smooth(se = F)

# 6a
mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se=F)

# 6b
mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = FALSE) 
  # note: se should be outside aes()

# Statistical Transformation #####

diamonds
glimpse(diamonds)
?diamonds

diamonds %>% 
  ggplot(aes(x = cut)) +
  geom_bar() +
  labs(title = "Count is not a variable in diamonds dataset")

 ?geom_bar

#
demo <- tribble( ~a ,   ~b,
                 "bar_1",  20,
                 "bar_2",  30,
                 "bar_3",  40)

demo

# y variable value
demo %>% 
  ggplot(aes(x = a, y = b)) +
  geom_bar(aes(x = a, y = b), stat = "identity")
# note: stat must be outside aes()

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, y = ..prop.., group = 1))

# geom_col is more straight forward
demo %>% 
  ggplot(aes(x = a, y = b)) +
  geom_col()

