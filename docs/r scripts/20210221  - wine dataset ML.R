# Wine dataset ML

# Created on 21 Feb 2020

# Objectives:

# 1. To revise wine supervised machine learning: predicting quality of white wine
# 2. To revise wine supervised machine learning: predicting quality of red wine
# 3. To try to predict type of wine

# Packages #####
library(pacman)

p_load(tidyverse, janitor, GGally, skimr, ggthemes, ggsci,
       broom, gvlma, ggfortify, jtools, car, huxtable, sandwich,
       tidymodels, parsnip, vip)

# Import data #####

red_rawdata <-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                         strip.white = T, sep = ";", header = T) %>%
  as_tibble() %>% 
  mutate(type = "red")

white_rawdata <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
                            strip.white = T, sep = ";", header = T) %>% 
  as_tibble() %>% 
  mutate(type = "white")

data_raw <- rbind(red_rawdata, white_rawdata) %>% 
  clean_names()

glimpse(data_raw)

# EDA #####
# https://jhudatascience.org/tidyversecourse/model.html
# What do you look out for during EDA?  < --- find notes
# what are the features? categorical or numerical?
# how many rows, how many columns? is n>>p or n<<p? Wha is the shape of the data?

# are there missing values? -- missing values check
# are the distributions of variables normal or skewed? - need to do transfomrations?
# are there outliers? either 0 or very extreme end?

# check the summary statistics: mean, median, variance (sd)

# evaluating relationships: 
# is there collinearity? check correlation

# how to do feature extraction? pca?
# need few variables, but model to be accurate (trade-off)

# need to transform categorical variables into dummy variables?
# need to bin predictors? -- may result in loss of performance in the model


data <- data_raw %>% 
  mutate(type = factor(type)) # change into factor

glimpse(data)

skim(data)

# all numeric variables, no missing columns

# understanding the correlation 
data %>% 
  select(-type) %>% 
  ggcorr(label = T,label_round = 2)

glimpse(data)

theme_set(theme_few())

# create a ggplot for first variable
data %>% 
  ggplot(aes(fixed_acidity, group = type)) +
  geom_freqpoly(aes(col = type), size = 2)

# create a function to create ggplots
# cannot use geom_freqpoly because white >> red
# use geom_density instead

plot_geom_density = function (.x = var_name, .z = type) {
  ggplot(data, aes(x = .x, group = .z)) +
           geom_density(aes(col = .z), size = 2) +
           scale_color_manual(values = c("darkred", "burlywood3")) +
    labs(title = paste("Plot:",  .x),
         x = NULL,
         col = "Type")
   
                                        
}

# create plots
# is there a way to use purrr to plot?

plot_1 <- plot_geom_density(data$fixed_acidity, data$type) +
  labs(title = "fixed acidity")
plot_2 <- plot_geom_density(data$volatile_acidity, data$type) +
  labs(title = "volatile acidity")
plot_3 <- plot_geom_density(data$citric_acid, data$type) +
  labs(title = "citric acid")
plot_4 <- plot_geom_density(data$residual_sugar, data$type) +
  labs(title = "residual sugar")
plot_5 <- plot_geom_density(data$chlorides, data$type) +
  labs(title = "chlorides")
plot_6 <- plot_geom_density(data$free_sulfur_dioxide, data$type) +
  labs(title = "free SO2")
plot_7 <- plot_geom_density(data$total_sulfur_dioxide, data$type) +
  labs(title = "total SO2")
plot_8 <- plot_geom_density(data$density, data$type) +
  labs(title = "density")
plot_9 <- plot_geom_density(data$p_h, data$type) +
  labs(title = "pH")
plot_10 <- plot_geom_density(data$sulphates, data$type) +
  labs(title = "sulphates")
plot_11 <- plot_geom_density(data$alcohol, data$type) +
  labs(title = "alcohol")

plot_12 <- plot_geom_density(data$quality, data$type) +
  labs(title = "quality")

# Change quality plot from geom_polygon to geom_boxplot
glimpse(data)
data %>% 
  ggplot(aes(x = type, y = quality)) +
  geom_boxplot(aes(fill = type)) +
  scale_fill_manual(values = c("darkred", "burlywood3")) +
  labs(title = "quality") +
  coord_flip()

gridExtra::grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6,
                        plot_7, plot_8, plot_9, plot_10, plot_11, plot_12,
                        ncol = 3)

# EDA findings
# a lot of right skewed
# quality, Y variable, is not unimodal, has three peaks

glimpse(data)

# to compare mean values for each parameter
mean_median_summary <- data %>% 
  pivot_longer(cols = c(fixed_acidity:quality),
               names_to = "parameters",
               values_to = "readings") %>% 
  group_by(type, parameters) %>% 
  summarize(mean = mean(readings),
            median = median(readings)) %>% 
  ungroup() %>% 
  arrange(parameters)

mean_median_summary

# number of obs for white wine: 4898
# number of obs for red wine: 1599

# (A) REGRESSION FOR RED WINE - TO PREDICT SCORE ------

# multivariate analysis?
# need to preprocess the data since the scale is different


glimpse(data)

data_white <- data %>% 
  filter(type == "white") %>% 
  select(-type)

glimpse(data_white) # 12 variables

summary(data_white)

ggpairs(data_white)

# Fit a linear model -----
# Y = quality (score), X = the 11 features
glimpse(data_white)

# Fit linear model, the traditional way
fit_lm_whitewine <- data_white %>% 
  lm(quality ~ ., .)

summary(fit_lm_whitewine) 

# call out significant variables
tidy(fit_lm_whitewine) %>% 
  filter(p.value<0.05) # only 8 variables remain

tidy(fit_lm_whitewine) %>% 
  filter(p.value>0.05) # citric acid, chlorides, total sulfur dioxide

# refit second linear model

fit_lm_whitewine_b <- data_white %>% 
  lm(quality ~ fixed_acidity + volatile_acidity +residual_sugar +
     free_sulfur_dioxide + density + p_h + sulphates + alcohol,
     data = .)

glance(fit_lm_whitewine_b)

glance(fit_lm_whitewine)

# slight differences only between the two models

# check assumptions for linear model:
gvlma::gvlma(fit_lm_whitewine) # failed 
# the relationships between X and Y are not really linear
# skewness: the distribution is skewed and some transformation should be done
# kurtosis: distribution is not normal, needs some transformation
# link function: is your dependent variable continous, or categorical?
#   can see that the quality score is not unimodal and has 3 peaks
# heterodasticity: the variance across the range of X is not constant. need to scale/means center?
# homodasticity: the variance (distance from the line) must be constant throughout the variable.

autoplot(gvlma(fit_lm_whitewine))
# maybe this explains why the r-sq is so poor

# if really want to fit linear regression,
# use robust methods

summ(fit_lm_whitewine, robust = T)
vif(fit_lm_whitewine) # failed, collinearity issues for residual sugar, density

data_white %>% 
  ggcorr(label = T, label_alpha = T, label_round = 2)

# summary of issues:
# failed gvlma
# multicollinearity issues
# not all X are important/significant

# In modelling for prediction, need to increase the r-sq
# so as to reduce the residual standard error

# Quality of white wine is 29.2% dependent on all the predictors as a whole


# Tidymodels -----

# Workflow for regression:
# 1. Split data into training, validation and testing data
# 2. Exploratory data
# 3. Preprocessing
# 4. Fit the different algorithms (models)
#     Check variable (feature) level relationships and significance
#     Which varialbes are of interest to you and play significant roles in 
#     explaining Y variables

#     Visualize model level information (error and accuracy)

#     Cross validate the models

# 5. Assess your different algorithms based on error and accuracy

# 6. Choose your models based on which predicts the outcome the best

# 7. Regularize your model (perform parameter tuning)

# lets begin ----

# 1. Split data into training and testing data -----
# how about validation?

# remember to set.seed since there is a split , so that results are reproducible

set.seed(202102251)

data_white_regression_split <- data_white %>% 
  initial_split(prop = 0.8)

data_white_regression_split 

regression_white_train <- training(data_white_regression_split)

regression_white_test <- testing(data_white_regression_split)

# split training dataset for cross-validation
set.seed(20210228)
white_cv <- vfold_cv(whitewine_train) # split training dataset for tuning mtry later

# splitting into training and testing datasets so that will not
# overfit, ie the model fits the sample data very well but is not as
# accurate when new datapoints are added into the model
# model is built based on training dataset, but 
# assessing the accuracy on the test dataset to see how well the model
# will work on new data

# 2. Exploratory data-----

# already did this before hand, but can do again for the training dateset

glimpse(regression_white_train) # 3919 out of 4898 

skim(regression_white_train)
# all numeric variables

# check for missing values
sapply(regression_white_train, function(x) sum(is.na(x)))

ggpairs(regression_white_train) +
  theme(axis.ticks = element_blank()) # right skewed except for ph

# Y is not unimodal (quality score)

regression_white_train %>% 
  ggcorr(label = T, label_round = 2) # density and residual sugar collinear


# 3. Preprocessing-----

# a. recipe #####
# defines the preprocessing steps to be taken, Note: the order MATTERS
# depending on the EDA insights, the steps taken include:
# - impute missing values (step_knn_impite, random forest hot deck imputation)
# - individual transformations for skewness and other issues
#   can be log transfomration, square root, inverse to remove skewness
#   step_log(all_numeric, offset = 1,  - pH) (unselect variables that are not skewed)
#   do transformations before normalizations!

# - create binning of variables only if necessary
# - create dummy X variables if there are categorical X features
# - create interactions
# - remove predictors if they are collinear, by PCA 
# - normalization steps (center, scale, range etc). 
#   means centering is only for numerical variables
#   step_normalise(all_numeric)
# - multivariate transformation (PCA, spatial sign to remove outliers etc)

glimpse(regression_white_train)
summary(regression_white_train)


# the order matters. need to do step_corr first, then step_log then step_norm

regression_wine_train_reciped # 1 outcome, 11 predictors

# if you need to transform the outcome, https://github.com/tidymodels/workflows/issues/31



# 4. Fit the different algorithms (models)------

#     Check variable (feature) level relationships and significance
#     Which variables are of interest to you and play significant roles in 
#     explaining Y variables

# 4a. Random forest model -----




# tune mtry
# mtry: An integer for the number of predictors that will be randomly sampled 
# at each split when creating the tree models.





# try out on diamonds dataset ####

data("diamonds")

set.seed(20210221)

diamonds_split <- initial_split(diamonds, prop = 0.8)

diamonds_train <- training(diamonds_split)
diamonds_test <- testing(diamonds_split)

diamonds_reciped <- recipe(price ~., data = diamonds_train) %>% 
  step_log(all_outcomes(), skip = T) %>% 
  step_normalize(all_predictors(), -all_nominal()) %>% 
  step_dummy(all_nominal()) %>% 
  step_poly(carat, degree = 2)

diamonds_preprocessed <- diamonds_reciped %>% prep()

lr_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lr_model

lr_workflow <- workflow() %>% 
  add_recipe(diamonds_reciped) %>% 
  add_model(lr_model)

final_model <- fit(lr_workflow, diamonds_train)
predict(final_model, new_data = diamonds_test)


# repeat for white wine data ------

glimpse(data_white)

set.seed(202102212)
whitewine_split <- initial_split(data_white, prop = 0.8)

whitewine_train <- training(whitewine_split)
whitewine_test <- testing(whitewine_split)

glimpse(whitewine_train)
whitewine_reciped <- whitewine_train %>% 
  recipe(quality ~., .) %>% 
  step_corr(all_predictors(), threshold = 0.5) %>% 
  step_log(all_predictors(), -pH, offset = 1) %>% # do not use all numeric since will affect Y (outcome)
  step_normalize(all_predictors())

whitewine_reciped 
  
whitewine_preprocessed <- prep(whitewine_reciped, verbose = T)

ww_transformed <- whitewine_preprocessed %>% bake(new_data = NULL) # see preprocessed data
glimpse(ww_transformed)
# sapply(ww_transformed, function(x)sum(is.na(x)))  - baseR

# alternatively, to check for missing values: - tidyverse purrr
ww_transformed %>% 
  map(is.na) %>%  
  map_df(sum) %>% 
  tidy() %>% 
  select(column, mean)
  
ww_lr_model <- linear_reg() %>% 
  set_engine("lm") %>% # there are other options available, eg glmnet
  set_mode("regression") # could also be classification for certain models, so just specify as best practice to be clear

ww_lr_workflow <- workflow() %>% 
  add_recipe(whitewine_reciped) %>% 
  add_model(ww_lr_model)

final_ww_lm_model_fit <- fit(ww_lr_workflow, whitewine_train)

# to understand actual vs predicted values:
predict(final_ww_lm_model_fit, new_data = whitewine_test) %>% # ok it works now ---
  bind_cols(whitewine_test$quality)

# try random forest for white wine #####

rf_model <- rand_forest() %>% 
  set_args(mtry = tune()) %>% 
  set_mode(mode = "regression") %>% 
  set_engine(engine = "ranger", importance = "impurity")

rf_model

rf_workflow <- workflow() %>% 
  add_recipe(whitewine_reciped) %>% 
  add_model(rf_model)

rf_grid <- expand.grid(mtry = 3:7)


tuned_rf_results <- rf_workflow %>% 
  tune_grid(resamples = white_cv, # using cv dataset from training dataset
            grid = rf_grid,
            metrics = metric_set(rmse, rsq, mae))

model_results <- tuned_rf_results %>% 
  collect_metrics()

finalized_rf_param <- tuned_rf_results %>% 
  select_best(metric = "rmse")
finalized_rf_param #M TRY = 3

rf_model_b <- rand_forest() %>% 
  set_args(mtry = 3) %>% 
  set_engine(engine = "ranger", importance = "impurity") %>% 
  set_mode(mode = "regression")

rf_workflow_b <- workflow() %>% 
  add_recipe(whitewine_reciped) %>% 
  add_model(rf_model_b)

final_ww_rf_model_fit <- fit(rf_workflow_b, whitewine_train)


# to understand actual vs predicted values:
predict(final_ww_rf_model_fit, new_data = whitewine_test) %>% # ok it works now ---
  bind_cols(whitewine_test$quality)


# end trial ------

# understanding the lm model

lm_fit_output <- final_ww_lm_model_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

lm_fit_output

# understanding variable importance, using vip library

vip_lm <- final_ww_lm_model_fit %>% 
  pull_workflow_fit() %>% # extracts the model information
  vip(num_features = 10) + # most important factor is alcohol +
  labs(title = "Variable Importance: Linear Regression") +
  theme_few() +
  theme(axis.text = element_text(face = "bold", size = 14))

# for random forest, need to set importance = impurity in set_engine() to extract this
vip_rf <- final_ww_rf_model_fit %>% 
  pull_workflow_fit() %>% # extracts the model information
  vip(num_features = 10) + # most important factor is alcohol
  labs(title = "Variable Importance: Random Forest") +
  theme_few() +
  theme(axis.text = element_text(face = "bold", size = 14))

gridExtra::grid.arrange(vip_lm, vip_rf, nrow = 2)

# start assessing #####
final_ww_lm_model_fit
final_ww_rf_model_fit

results_train <- final_ww_lm_model_fit %>% 
  predict(new_data = whitewine_train) %>%  # use actual train data, not preprocessed data
  mutate(truth = whitewine_train$quality,
         model = "lm") %>% 
  bind_rows(final_ww_rf_model_fit %>% 
              predict(new_data = whitewine_train) %>% 
              mutate(truth = whitewine_train$quality,
                     model = "rf")) 
  
results_train %>% 
  group_by(model) %>% 
  metrics(truth = truth, estimate = .pred)


results_test <- final_ww_lm_model_fit %>% 
  predict(new_data = whitewine_test) %>% 
  mutate(truth = whitewine_test$quality,
         model = "lm") %>% 
  bind_rows(final_ww_rf_model_fit %>% 
              predict(new_data = whitewine_test) %>% 
              mutate(truth = whitewine_test$quality,
                     model = "rf")) 

results_test %>% 
  group_by(model) %>% 
  metrics(truth = truth, estimate = .pred)

# when comparing rmse, rf has lower rmse in training dataset but the rmse value
# increased in the test dataset --> overfitting and cannot predict as well

# same for other indicators rsq and mean absolute error

# need to improve the results using bootstrapping
# bootstrapping consists of randomly sampling a data set with replacement,
# and then performing the analysis individually on each bootstrapped replicate. 
# the variation in the resulting estimate is then a reasonable approximation of
# the variance in our estimate. 



# --- theory ------
#     Visualize model level information (error and accuracy) ######

rf_fit <- final_ww_rf_model_fit %>% 
  pull_workflow_fit()


# visualizing model fit

lm_fit <- final_ww_lm_model_fit %>% 
  pull_workflow_fit()

lm_fit

lm_fitted_values <- lm_fit$fit$fitted.values

# another way, from workflow
lm_wf_fitted_values <- 
  broom::augment(lm_fit$fit, data = whitewine_train) %>% 
  select(quality, .fitted: .std.resid)

glimpse(lm_wf_fitted_values)

head(wf_fitted_values)

results_train %>% 
  ggplot(aes(x =  truth, y = .pred)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(col = "darkorange") +
  labs(x = "actual values (truth)",
       y = "predicted values",
       title = "Training dataset") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap( model ~ ., ncol = 2) +
  theme_fivethirtyeight()

results_test %>% 
  ggplot(aes(x =  truth, y = .pred)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(col = "darkorange") +
  labs(x = "actual values (truth)",
       y = "predicted values",
       title = "Testing dataset") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap( model ~ ., ncol = 2) +
  theme_fivethirtyeight()

# same as plot above
lm_wf_fitted_values%>% 
  ggplot(aes(x =  quality, y = .fitted)) + 
  geom_point() + 
  labs(x = "actual values (truth)",
       y = "predicted values") 

# using cross validation resamples for linear regression #######
# previous: 
# white_cv <- vfold_cv(whitewine_train)

white_cv

#fit_resamples() computes a set of performance metrics across one or more resamples. 
rsample_fit_lm <- fit_resamples(ww_lr_workflow,
                                white_cv)
rsample_fit_lm 

collect_metrics(rsample_fit_lm)






# compare with old


#     Cross validate the models

# 5. Assess your different algorithms based on error and accuracy------



# 6. Choose your models based on which predicts the outcome the best------

# 7. Regularize your model (perform parameter tuning)


save.image(file = "20210221.Rdata")
load("20210221.Rdata")

# Learning points #####
 # random forest doesnt need all numeric input
 # mtry: use square root of the number of predictors and plug that value for "mtry".
 # in this case there are 10 predictors, sqrt 10 = 3.16, similar to the value
 # after tuning for mtry

# pre-processing: the order is important, all_numeric() vs all_predictor() 
# for step_log --> may have errors after that
# qtn: when does step_corr come in? at the start or at the end?


# models should be trained using training dataset only, not the whole dataset
# so as to assess model in unbiased manner that will not take into acct 
# test dataset


# metrics for assessing regression models: rsq(accuracy), rmse (root mean square error),
 # mae: mean absolute error, to account for errors without considering direction


# References ######
# https://www.tmwr.org/
# https://online.stat.psu.edu/stat508/lesson/1a
# https://semba-blog.netlify.app/05/11/2020/regression-with-tidymodels/
# https://stackoverflow.com/questions/63239600/how-to-make-predictions-in-tidymodels-r-when-the-data-has-been-preprocessed
# https://stackoverflow.com/questions/13956435/setting-values-for-ntree-and-mtry-for-random-forest-regression-model
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# https://jhudatascience.org/tidyversecourse/model.html
# https://koalaverse.github.io/vip/articles/vip.html
# http://rstudio-pubs-static.s3.amazonaws.com/565136_b4395e2500ec4c129ab776b9e8dd24de.html#results
