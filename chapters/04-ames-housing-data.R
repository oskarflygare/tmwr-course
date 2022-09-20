
# 04 - The Ames Housing Data ----------------------------------------------

library(tidyverse)
library(tidymodels)
tidymodels_prefer()


# Load data ---------------------------------------------------------------

data(ames, package = "modeldata")

dim(ames)


# Exploratory data analysis -----------------------------------------------

# Plot house sale prices
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

# It's right-skewed so log-transforming makes sense
# Benefits: No house is predicted with negative sale prices, stabilizing variance to make inferences more legitimate.
# Drawbacks: Hard to interpret model results.

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white") +
  scale_x_log10()

# Log-transform the sale price column
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))
