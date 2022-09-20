
# 05-spending-our-data ----------------------------------------------------

library(tidyverse)
library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(501)


# Data splitting ----------------------------------------------------------

ames_split <- initial_split(ames, prop = 0.80)

# n Train, n Test, n Total
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)

# Stratified split
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
