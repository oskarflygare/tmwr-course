
# 08 - Feature engineering with recipes -----------------------------------

library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)

# Standard lm-model vs recipe ---------------------------------------------

# Standard model
lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)

# Recipe structure
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

simple_ames


# Add recipe to model -----------------------------------------------------

lm_wflow <-
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test %>% slice(1:3))

# Get tidy model output
lm_fit %>% 
  extract_fit_parsnip() %>% 
  tidy() %>%
  slice(1:5)


# Variable interactions ---------------------------------------------------

ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) +
  geom_point(alpha = .2) +
  facet_wrap(~Bldg_Type) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Gross Living Area",
       y = "Sale Price (USD)")

# Defining interactions using recipes
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_type_"))


# Spline functions --------------------------------------------------------

library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

# Add spline function to recipe
simple_ames <- simple_ames %>% 
  step_ns(Latitude, deg_free = 20)

simple_ames


# PCA ---------------------------------------------------------------------

# Use a regular expression to capture house size predictors: 
simple_ames %>% 
step_pca(matches("(SF$)|(Gr_Liv)"))


# Subsampling -------------------------------------------------------------

# To address class imbalance in the outcome
step_downsample(outcome_column_name)


# Tidying a recipe --------------------------------------------------------

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)


# Add a custom ID for the step_other function
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

# Extract the recipe
estimated_recipe <-
  lm_fit %>% 
  extract_recipe(estimated = TRUE)

# Specify which one to tidy (by name)
tidy(estimated_recipe, id = "my_id")

# Specify which one to tidy (by position)
tidy(estimated_recipe, number = 2)


# Column roles ------------------------------------------------------------

ames_rec %>% update_role(address, new_role = "street address")
