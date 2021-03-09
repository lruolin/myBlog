# practice pls model

# reference
# https://www.tidymodels.org/learn/models/pls/

#install.packages("modeldata")
# install.packages("pls")

# packages ------
library(pacman)
p_load(modeldata, pls, tidyverse, tidymodels, learntidymodels, 
       AppliedPredictiveModeling, corrplot, skimr, plsmod, ggthemes)

# install BiocManager if not installed if (!requireNamespace("BiocManager", quietly = TRUE))     install.packages("BiocManager") 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install(version = "3.12")


## install mixOmics 
# BiocManager::install('mixOmics')

# "These data are recorded on a Tecator Infratec Food and Feed Analyzer 
# working in the wavelength range 850 - 1050 nm by the Near Infrared Transmission 
# (NIT) principle. Each sample contains finely chopped pure meat with 
# different moisture, fat and protein contents.

# "For each meat sample the data consists of a 100 channel spectrum of absorbances 
# and the contents of moisture (water), fat and protein. 
# The absorbance is -log10 of the transmittance measured by the spectrometer. 
# The three contents, measured in percent, are determined by analytic chemistry."

 # meats ------
data(meats) # not tibble, is matrices
glimpse(meats) # 215 observations, 103 variables. 100 wavelengths

# the three outcomes are highly correlated
meats %>% 
  select(water, fat, protein) %>% 
  ggpairs()

# Objective:
# to predict the proportion of water, fat and protein using the spectral data
# multivariate analysis, regression model using partial least squares method

# pre-processing data

norm_rec <- recipe(water + fat + protein ~., data = meats) %>% 
  step_normalize(everything()) %>% 
  prep()

norm_rec # 3 outcomes (Y), 100 predictors (X)

# doing for without resampling

y_matrix <- bake(norm_rec, new_data = NULL, 
              composition =  "matrix", # matrix for y
              all_outcomes())
glimpse(y_matrix)

x_matrix <- bake(norm_rec, new_data = NULL,
                 composition = "matrix", # matrix for x
                 all_predictors())

glimpse(x_matrix)

# put into pls format: Y and X are to be in matrices.
# I(): change the class of an object to indicate that is dhould be treated "as is"
# In function data.frame. Protecting an object by enclosing it in I() in a call 
# to data.frame inhibits the conversion of character vectors to factors and the 
# dropping of names, and ensures that matrices are inserted as single columns. 
# 
# I can also be used to protect objects which are to be added to a data frame, 
# or converted to a data frame via as.data.frame.

pls_format <- data.frame(
  endpoints = I(y_matrix), # inhibit data.frame, such that pls_format has 2 columns
  measurements = I(x_matrix)
)

pls_format

# fit the model, pls: partial least square regression
# plsr (Y ~ X)
# other options: cppls: canonical powered partial least squares
# principal component regression (pcr)
pls_model <- plsr(endpoints ~ measurements, data = pls_format)

# can specify preprocessing steps, cross-validation, but in the context of
# tidymodels, these steps are done in recipes step, before modelling is done

# get the proportion of the predictor variance that is explained by the model

x_variance <-  explvar(pls_model)/100

x_variance %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(components = rowid) %>% 
  mutate(cumsum_variance = cumsum(value)) %>% 
  ggplot(aes(components, cumsum_variance)) +
  geom_point() +
  geom_line() +
  coord_cartesian(xlim = c(1,10)) +
  theme_classic()  # 4 components are enough to explain var for x, but if you 
# want to predict Y?


summary(pls_model)

# variance

drop(pls::R2(pls_model, estimate = "train", intercept = FALSE)$val) %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(predictors = cumsum(x_variance) %>%  as.vector(),
         components = seq_along(x_variance)) %>% 
  pivot_longer(cols = c(-components),
               names_to = "source",
               values_to = "proportion") %>% 
  ggplot(aes(components, proportion, col = source)) +
  geom_point() +
  geom_line() +
  labs(title = "Cumulative % Variance Explained, for Outcomes (fat, protein and moisture) and Predictors, in TRAINING dataset",
       x = "Components",
       y = "% variance",
       col = "Outcomes (Y variables)") +
  coord_cartesian((xlim = c(1, 20))) +
  theme_classic() +
  theme(legend.position = "top")


# root mean square error

drop(pls::RMSEP(pls_model, estimate = "train", intercept = FALSE)$val) %>% 
  t() %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(components = rowid) %>% 
  pivot_longer(cols = c(-components),
               names_to = "source",
               values_to = "RMSEP") %>% 
  ggplot(aes(components, RMSEP, col = source)) +
  geom_point() +
  geom_line() +
  labs(title = "RMSEP, for Outcomes (fat, protein and moisture) in TRAINING dataset",
       x = "Components",
       y = "RMSEP",
       col = "Outcomes (Y variables)") +
  scale_x_continuous(n.breaks = 20) +
  coord_cartesian((xlim = c(1, 20))) +
  theme_classic() +
  theme(legend.position = "top")



# resampling ######
# only 215 observations, considered to be a small dataset. 
# use resampling to build the PLS model, using 90% of data (cos small dataset?)
# for each of the 100 models, we extract and save the proportions

set.seed(57343)

folds <- vfold_cv(meats, repeats = 10)

folds <- 
  folds %>%
  mutate(recipes = map(splits, prepper, recipe = norm_rec))


# V-fold cross-validation randomly splits the data into V groups of 
# roughly equal size (called "folds"). A resample of the analysis data 
# consisted of V-1 of the folds while the assessment set contains the final fold. 

# for the pls package, the standardized outcomes (Y) and predictors (X) needs 
# to be formatted into two separate matrices.
# need to get the preprocessed data out using bake()
# and then save the preprocessed data as matrix, using composition = "matrix"

# need to use I(): each side of the formula is a matrix.

# calculate proportion of variance explained using pls::explvar()
# use get_var_explained() to get a dataframe with components, source, and proportion

get_var_explained <- function(recipe, ...) {
  
  # Extract the predictors and outcomes into their own matrices
  y_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_outcomes())
  x_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_predictors())
  
  # The pls package prefers the data in a data frame where the outcome
  # and predictors are in _matrices_. To make sure this is formatted
  # properly, use the `I()` function to inhibit `data.frame()` from making
  # all the individual columns. `pls_format` should have two columns.
  pls_format <- data.frame(
    endpoints = I(y_mat),
    measurements = I(x_mat)
  )
  # Fit the model
  mod <- plsr(endpoints ~ measurements, data = pls_format)
  
  # Get the proportion of the predictor variance that is explained
  # by the model for different number of components. 
  xve <- explvar(mod)/100 
  
  # To do the same for the outcome, it is more complex. This code 
  # was extracted from pls:::summary.mvr. 
  explained <- 
    drop(pls::R2(mod, estimate = "train", intercept = FALSE)$val) %>% 
    # transpose so that components are in rows
    t() %>% 
    as_tibble() %>%
    # Add the predictor proportions
    mutate(predictors = cumsum(xve) %>% as.vector(),
           components = seq_along(xve)) %>%
    # Put into a tidy format that is tall
    pivot_longer(
      cols = c(-components),
      names_to = "source",
      values_to = "proportion"
    )
}

# previously,
norm_rec <- recipe(water + fat + protein ~., data = meats) %>% 
  step_normalize(everything()) %>% 
  prep()



folds <- 
  folds %>%
  mutate(var = map(recipes, get_var_explained),
         var = unname(var))

variance_data <- 
  bind_rows(folds[["var"]]) %>%
  filter(components <= 15) %>%
  group_by(components, source) %>%
  summarize(proportion = mean(proportion))

ggplot(variance_data, aes(x = components, y = proportion, col = source)) + 
  geom_line() + 
  geom_point() +
  theme_classic()


# pls package documentation ------
library(pls) # partial least squares and principal component regression

# load data

# 28 NIR of PET yarns, measured at 268 wavelengths as predictors, 
# and density as response
data(yarn) 

# 5 quality measurment (chemical) and 6 panel sensory variables
# made on 16 olive oil samples
data(oliveoil) # ?oliveoil for details

# octane number and NIR spectra of 60 gasoline samples
# each NIR consists of 401 diffuse reflectance measurements from 900-1700nm
data(gasoline)

# look at data
gasoline # 60 obs of 2 variables
glimpse(gasoline)
?gasoline

yarn # 28 obs of 3 variables
glimpse(yarn)

oliveoil
glimpse(oliveoil)

# set digits
options(digits = 4)

# gasoline data -----

# plot spectra out
wavelengths <- seq(900,1700, by = 2)
matplot(wavelengths, 
        t(gasoline$NIR),
        type = "l",
        lty = 1,
        xlab = "Wavelength (nm)",
        ylab = "1/R"
        ) # t means transpose

gasoline_b <- gasoline$NIR %>%
  t()

gasoline$NIR[[1]]

# convert matrix to dataframe, for plotting
# unclass NIR and bind as col to existing dataframe
# remove NIR col

ds <- cbind(gasoline, as.data.frame((unclass(gasoline$NIR)))) %>% 
  select(-NIR) %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = starts_with("x"),
               names_to = "wavelength",
               values_to = "reflectance") %>% 
  mutate(wavelength_number = parse_number(wavelength))

ds %>% 
  ggplot(aes(x = wavelength_number, y = reflectance, col = factor(rowid))) +
  geom_line(show.legend = F) +
  labs(y = "log(1/R)",
       x = "nm",
       title = "Plot of NIR spectra for 60 gasoline samples") +
  coord_cartesian(xlim = c(900, 1800)) +
  theme_classic()



# tidymodels ------

gasoline_tidy <-  cbind(gasoline, as.data.frame((unclass(gasoline$NIR)))) %>% 
  dplyr::select(-NIR) %>% 
  as_tibble() %>% 
  janitor::clean_names() 

glimpse(gasoline_tidy) # 60 rows, 402 columns (60 datasets)

# EDA

# check for missing values
gasoline_tidy %>% 
  purrr::map(is.na) %>% 
  map_df(sum) %>% 
  tidy() %>% 
  dplyr::select(column, mean) %>% 
  as_tibble() %>% 
  filter(mean>0) # no missing values

#?

# initial split
set.seed(20210308)
gasoline_split <- initial_split(gasoline_tidy, prop = 0.8)

gasoline_training <- gasoline_split %>% 
  training()

gasoline_testing <- gasoline_split %>% 
  testing()

gasoline_cv <- vfold_cv(gasoline_training) # to tune number of components later


glimpse(gasoline_training) # 49 x 402 
head(gasoline_training)

# recipe

gasoline_reciped <- recipe(octane ~ ., data = gasoline_training) %>% 
  update_role(octane, new_role = "outcome") %>% 
  step_normalize(all_predictors())

gasoline_reciped # 1 id,  1 outcome, 401 predictors

gasoline_prep <- gasoline_reciped %>% 
  prep()

gasoline_prep # on training dataset

# fit model

pls_model <- plsmod::pls(num_comp = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("mixOmics")

# put into workflow

pls_workflow <- workflow() %>% 
  add_recipe(gasoline_reciped) %>% 
  add_model(pls_model)

# create grid
pls_grid <- expand.grid(num_comp = seq (from = 1, to = 20, by = 1))

tuned_pls_results <- pls_workflow %>% 
  tune_grid(resamples = gasoline_cv,
            grid = pls_grid,
            metrics = metric_set(mae, rmse, rsq))

(model_results <- tuned_pls_results %>% 
  collect_metrics())

# plot

tuned_pls_results %>% 
  collect_metrics() %>% 
  ggplot(aes(num_comp, mean, col = .metric)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 20) +
  labs(x = "Number of components",
       y = "Indicator",
       title = "Plot of MAE, RMSE and R-SQ vs number of components for TRAINING dataset, with 10-fold repeated cross validation",
       subtitle = "Optimal number of components is 3") +
 facet_grid(.metric ~.) +
  theme_few() +
  theme(legend.position = "none")

# check against numerical values
tuned_best <- tuned_pls_results %>% 
  select_best("rsq") # num_comp = 7

# check with other indicators
tuned_pls_results %>% 
  select_best("mae") # num_comp = 4

tuned_pls_results %>% 
  select_best("rmse") # num_comp = 6

updated_pls_model <-  plsmod::pls(num_comp = 4) %>% 
  set_mode("regression") %>% 
  set_engine("mixOmics")

updated_workflow <- pls_workflow %>% 
  update_model(updated_pls_model)
  

pls_fit <- updated_workflow %>% 
  fit(data = gasoline_training)

# check the most important predictors

tidy_pls <- pls_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

tidy_pls

tidy_pls$term %>% unique() # Y variable is "Y"

# variable importance
tidy_pls %>% 
  filter(term != "Y",
         component == c(1:3)) %>% 
  group_by(component) %>% 
  slice_max(abs(value), n = 20) %>% 
  ungroup() %>% 
  ggplot(aes(value, fct_reorder(term, value), fill = factor(component))) +
  geom_col(show.legend = F) +
  facet_wrap( ~ component, scales = "free_y") +
  labs( y = NULL) +
  theme_few()
  
# assess

# results_train
pls_fit %>% 
  predict(new_data = gasoline_training) %>% 
  mutate(truth = gasoline_training$octane) %>% 
  ggplot(aes(truth, .pred)) +
  geom_point() +
  geom_abline() +
  labs(title = "Actual vs Predicted for TRAINING dataset",
       x = "Actual Octane Number",
       y = "Predicted Octane Number") +
  theme_few()

pls_fit %>% 
  predict(new_data = gasoline_training) %>% 
  mutate(truth = gasoline_training$octane) %>% 
  metrics(truth = truth, estimate = .pred) %>% 
  as_tibble() # rsq is 0.981 for training dataset

# results_test
pls_fit %>% 
  predict(new_data = gasoline_testing) %>% 
  mutate(truth = gasoline_testing$octane) %>% 
  ggplot(aes(truth, .pred)) +
  geom_point(col = "deepskyblue3") +
  geom_abline(col = "deepskyblue3") +
  labs(title = "Actual vs Predicted for TESTING dataset",
       x = "Actual Octane Number",
       y = "Predicted Octane Number") +
  theme_few()

pls_fit %>% 
  predict(new_data = gasoline_testing) %>% 
  mutate(truth = gasoline_testing$octane) %>% 
  metrics(truth = truth, estimate = .pred) %>% 
  as_tibble() # r-sq is 0.989 for testing dataset

# predict future data
glimpse(gasoline_tidy)

trial_data <- gasoline_tidy %>% 
  head((1)) %>% 
  dplyr::select(-octane) # octane = 85.3
 
trial_data

pls_fit %>% 
  predict(trial_data) # 85.3, but needs rowid

# References ------
# https://stackoverflow.com/questions/64254295/predictor-importance-for-pls-model-trained-with-tidymodels
# https://rpubs.com/RandallThompson15/HW10_624 
