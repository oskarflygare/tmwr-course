
# Fitting Models with parsnip ---------------------------------------------
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Prep --------------------------------------------------------------------

data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)


# Fit model ---------------------------------------------------------------

lm_model <-
  linear_reg() %>% 
  set_engine("lm")

# Fit using formula interface
lm_form_fit <-
  lm_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit

lm_xy_fit


# Use model results -------------------------------------------------------

model_res <-
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

model_res

param_est <- coef(model_res)

param_est

tidy(lm_form_fit)


# Make predictions --------------------------------------------------------

ames_test_small <- ames_test %>% slice(1:5)

predict(lm_form_fit, new_data = ames_test_small)

# Merge predictions with original data
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))


# Fit another model (showing homogeneity) ---------------------------------

tree_model <-
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <-
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))
