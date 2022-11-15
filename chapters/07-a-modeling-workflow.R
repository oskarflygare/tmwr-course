
# 07 - A Model Workflow ---------------------------------------------------

library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <-
  workflow() %>% 
  add_model(lm_model)

lm_wflow <-
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)

lm_wflow <-
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_wflow

fit(lm_wflow, ames_train)


# Special formulas --------------------------------------------------------

library(lme4)

lmer(distance ~ Sex + (age | Subject), data = Orthodont)


# Multiple workflows ------------------------------------------------------

library(workflowsets)

location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

location_models <- workflow_set(preproc = location, models = list(lm = lm_model))

location_models

location_models$info[[1]]

extract_workflow(location_models, id = "coords_lm")

location_models <-
  location_models %>% 
  mutate(fit = map(info, ~fit(.x$workflow[[1]], ames_train)))

location_models

final_lm_res <- last_fit(lm_wflow, ames_split)

final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)

collect_predictions(final_lm_res) %>% slice(1:5)

