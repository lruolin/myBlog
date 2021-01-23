# Chapter 2: Workflow

# Making sequences #####

seq(1,10)

x <- "hello world"
x

(y <- "print out")

# Exercise

library(tidyverse)

glimpse(mpg)
mpg %>% 
ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(x = displ, y = hwy))


filter(mpg, cyl == 8)
filter(diamonds, carat >3)
