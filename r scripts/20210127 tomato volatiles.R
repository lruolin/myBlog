# Import tomato volatiles 
# Use ggplot to show types of chemicals

library(rJava)
library(tabulizer)
library(tidyverse)
library(stringr)
library(stringi)
library(textclean)

# download paper: 

# IMPORT #####
# using the tabulizer package

file <- "1986-tomato volatiles.pdf"
locate_areas(file)

p18 <- extract_text(file, pages = 18, area = list(c(115.5, 33.05, 641.923, 443.94)))

p19 <- extract_text(file, pages = 19, 
                    area = list(c(117.15636,  53.20364, 651.60000, 465.22545 )))

p20 <- extract_text(file, pages = 20, 
                    area = list(c(114.52364,  38.72364, 650.28364, 449.42909 )))

p21 <- extract_text(file, pages = 21, 
                    area = list(c(119.29273 , 48.89727, 656.76545, 457.90091 )))

p22 <- extract_text(file, pages = 22, 
                    area = list(c(113.20727,  30.32545, 643.70182, 466.04182)))

p23 <- extract_text(file, pages = 23, 
                    area = list(c(122.25273,  49.84636, 652.01455, 473.13000 )))

p24 <- extract_text(file, pages = 24, 
                    area = list(c(113.,  34, 645.01818, 430.50000 )))

p25 <- extract_text(file, pages = 25, 
                    area = list(c(119,  58 ,642.81273, 478.38818)))

p26 <- extract_text(file, pages = 26, 
                    area = list(c(114,  35, 668.17818, 443.44000)))

p27 <- extract_text(file, pages = 27, 
                    area = list(c(114.20727,  41.24545, 656.36364, 442.94000)))

p28 <- extract_text(file, pages = 28, 
                    area = list(c(115.52000,  33.86909, 404.32000, 469.69455 )))

# TRANSFORM #####
# use textclean package

p18 %>% as_tibble()

combined <- tribble(~page, ~text,
                    "p18", p18,
                    "p19", p19,
                    "p20", p20,
                    "p20", p20,
                    "p21", p21,
                    "p22", p22,
                    "p23", p23,
                    "p24", p24,
                    "p25", p25,
                    "p26", p26,
                    "p27", p27,
                    "p28", p28) %>% 
  # replace \n with ;
  mutate(text_2 = gsub("\\n", "; ", text),
         text_3 = str_split(text_2, "; ")) %>% # split by ; into new columns
  unnest() 

glimpse(combined)

# 
cleaned_text <- combined %>% 
  dplyr::select(text_3) # 521

glimpse(cleaned_text)  

cleaned_text_b <- cleaned_text %>% 
  filter(!text_3 %in% c("(Continued)", "")) %>%  # 503
  filter(!is.na(text_3)) %>% 
  mutate(chem_category = str_extract_all(text_3, "^[:upper:]+$")) %>% 
  mutate(text_4 = str_replace_all(text_3, "[0-9]{2,3}", ""),
         text_4 = str_replace_all(text_4, "\\ , ", ""),
         text_4 = str_replace_all(text_4, "\\,,+", ""),
         text_4 = str_replace_all(text_4, "\\(unknown structure\\)", ""),
         text_4 = str_replace_all(text_4, "\\[[^\\]\\[]*]", ""),
         text_4 = str_replace_all(text_4, "\\[[^\\[]*", ""),
         text_4 = str_replace_all(text_4, " \\,", ""),
         text_4 = str_replace_all(text_4, " \\/", ""),
         # replace last commar, dash
         text_4 = stri_replace_last(text_4, fixed = ",", ""),
         text_4 = str_replace_all(text_4, "PJ-dimethyl-ö-octen-l-ol]", ""),
         text_4 = str_replace_all(text_4, "/", ""),
         
         # correct for commar replacement
         text_4 = str_replace_all(text_4, "45", "4,5"),
         text_4 = str_replace_all(text_4, "26", "2,6"),
         text_4 = str_replace_all(text_4, "25", "2,5"),
         text_4 = str_replace_all(text_4, "33", "3,3"),
         text_4 = str_replace_all(text_4, "23", "2,3"),
         text_4 = str_replace_all(text_4, "14", "1,4"),
         text_4 = str_replace_all(text_4, "12", "1,2"),
         text_4 = str_replace_all(text_4, "24", "2,4"),
         text_4 = str_replace_all(text_4, "34", "3,4"),
         text_4 = str_replace_all(text_4, "11", "1,1"),
         text_4 = str_replace_all(text_4, "2E 4Z", "2E,4Z"),
         text_4 = str_replace_all(text_4, "2E4E", "2E,4E"),
         text_4 = str_replace_all(text_4, "2E4Z", "2E,4Z"),
         text_4 = str_replace_all(text_4, "hy droxy", "hydroxy"),
         text_4 = str_replace_all(text_4, "66", "6,6"),
         text_4 = str_replace_all(text_4, "3E5E", "3E,5E"),
         
         # further clean up
         text_4 = str_replace_all(text_4, "\\(unknownstructure\\)", ""),
         text_4 = str_replace_all(text_4, "\\(unknown", ""),
         text_4 = str_replace_all(text_4, "2-formylpyiTole ", "2-formylpyrrole"),
         text_4 = str_replace_all(text_4, "neraUcis-SJ-dimethyl\\^.o-octadienal]", 
                                  "neral"),
         text_4 = str_replace_all(text_4, "2-methyl-l-propanol -", 
                                  "2-methyl-l-propanol"),
         text_4 = str_replace_all(text_4, "propanal 9 09 39 49 5",
                                  "propanal"),
         text_4 = str_replace_all(text_4, "\\(methylthioH-propanol",
                                  "\\(methylthio)-propanol"),
         text_4 = str_replace_all(text_4, "\\(2,2,6-trimethyl-7-oxabicyclo",
                                  ""),
         text_4 = str_replace_all(text_4, "._.", ""),
         
         # remove white space
         text_4 = str_replace_all(text_4, " \\s", ""),
         
         # remove quotation marks
         text_4 = str_remove_all(text_4, "\"")) 



glimpse(cleaned_text_b)

str_view_all(cleaned_text_b$text_3, "[0-9]{2,3}\\, ") # 2 or 3 digits with commar
str_view_all(cleaned_text_b$text_4, "[0-9]{2,3}\\, ") # 2 or 3 digits with commar

str_view_all(cleaned_text_b$text_4, "\\ , ")
str_view_all(cleaned_text_b$text_4, "\\,,+")
str_view_all(cleaned_text_b$text_4, "\\[[^\\]\\[]*]") # remove all square brackets
str_which(cleaned_text_b$text_4, "\\(unknown structure\\)")
str_view_all(cleaned_text_b$text_4, "\\[[^\\[]*")
str_view_all(cleaned_text_b$text_4, " \\,")
str_view_all(cleaned_text_b$text_4, "PJ-dimethyl-ö-octen-l-ol]")
str_view_all(cleaned_text_b$text_4, "/")
str_which(cleaned_text_b$text_4, " \\s")
str_which(cleaned_text_b$text_4, "")

# str_which to find indexes of strings that contain a pattern match
str_which(cleaned_text_b$text_4, "heptane") # 2, 293, 471
str_which(cleaned_text_b$text_4, "neraUcis-SJ-dimethyl\\^.o-octadienal]")
str_which(cleaned_text_b$text_4, "propanal 9 09 39 49 5")
str_which(cleaned_text_b$text_4, "._.")

cleaned_text_b$text_4[163]

cleaned_text_b$text_4[483]

cleaned_text_b$text_4[43]


# final extract

tomato_cleaned <- cleaned_text_b$text_4 %>% 
  as.data.frame() %>% 
  unique()

names(tomato_cleaned) <- c("compounds")

glimpse(tomato_cleaned)

tomato_cleaned <- print(tomato_cleaned, quote = FALSE) %>% 
  filter(!compounds == " ",
         !compounds == "")# remove quotation marks 


tomato_cleaned$compounds <- str_remove_all(tomato_cleaned$compounds, 
                                           "\\s")


write.csv(tomato_cleaned, "tomato_cleaned.csv")

# Split #####


hydrocarbons <- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(2:39) %>% 
  mutate(category = "hydrocarbons")


alcohols <- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(41:98) %>% 
  mutate(category = "alcohols")

phenols <- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(99:108) %>% 
  mutate(category = "phenols")

ethers<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(111:116) %>% 
  mutate(category = "ethers")

aldehydes<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(118:176) %>% 
  mutate(category = "aldehydes")

ketones<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(178:219) %>% 
  mutate(category = "ketones")

dicarbonyl<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(221:226) %>% 
  mutate(category = "dicarbonyl")

acids<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(228:254) %>% 
  mutate(category = "acids")

esters<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(256:314) %>% 
  mutate(category = "esters")

lactones<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(316:328) %>% 
  mutate(category = "lactones")

sulfur<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(330:341) %>% 
  mutate(category = "sulfur")

nitrogen<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(343:362) %>% 
  mutate(category = "nitrogen")

halogen<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(364:366) %>% 
  mutate(category = "halogen")

oxygen<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(368:398) %>% 
  mutate(category = "oxygen")

sulfur_heterocyclic<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(400:404) %>% 
  mutate(category = "sulfur_heterocyclic")

nitrogen_heterocyclic<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(406:417) %>% 
  mutate(category = "nitrogen_heterocyclic")

nitrogen_sulfur_heterocyclic<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(419:423) %>% 
  mutate(category = "nitrogen_sulfur_heterocyclic")

nitrogen_oxygen_heterocyclic<- tomato_cleaned %>% 
  as_tibble() %>% 
  slice(425:427) %>% 
  mutate(category = "nitrogen_oxygen_heterocyclic")


tomatoes_compounds <- bind_rows(hydrocarbons,
                                alcohols,
                                phenols,
                                ethers,
                                aldehydes,
                                ketones,
                                dicarbonyl,
                                acids,
                                esters,
                                lactones,
                                sulfur,
                                halogen,
                                oxygen,
                                sulfur_heterocyclic,
                                nitrogen_heterocyclic,
                                nitrogen_sulfur_heterocyclic,
                                nitrogen_oxygen_heterocyclic
                                )
# types of categories
categories <- tomatoes_compounds %>% 
  dplyr::select(category) %>% 
  unique()

categories

# ggplot

tomatoes_compounds %>% 
  group_by(category) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(category, count), y = count, label = count)) +
  geom_col(fill = "tomato2") +
  geom_text(aes(label = count), hjust = -0.5, size = 4) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
  labs(x = "No. of compounds",
       y = "Category",
       title = "Number of volatile compounds identified in tomatoes, sorted by chemical category",
       subtitle = "Esters, aldehydes and alcohols dominate the types of compounds identified",
       source = "Petro-Turza(1989): Flavor of tomato and tomato products ") +
  coord_flip() +
  theme_classic() +
  theme(title = element_text(size = 16),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12))


# references:

# https://www.tandfonline.com/doi/abs/10.1080/87559128609540802
# https://www.r-bloggers.com/2019/09/pdf-scraping-in-r-with-tabulizer/
