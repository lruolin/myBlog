# Tidy tuesdays coffee rating

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

glimpse(coffee_ratings)

write.csv(coffee_ratings, "data-coffee_ratings.csv")
