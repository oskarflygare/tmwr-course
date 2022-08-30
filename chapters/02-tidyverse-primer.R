
# 02 - A Tidyverse Primer -------------------------------------------------

library(tidyverse)
library(tidymodels)
library(lubridate)


# Example function

compute_log_ratio <- function(mpg, wt, log_base = exp(1)) {
  log(mpg/wt, base = log_base)
}

compute_log_ratio(mtcars$mpg, mtcars$wt)

# The same thing done using purrr:map()
map2_dbl(mtcars$mpg, mtcars$wt, ~ log(.x/.y)) %>% 
  head()


# Tidyverse example of data wrangling

url <- "https://data.cityofchicago.org/api/views/5neh-572f/rows.csv?accessType=DOWNLOAD&bom=true&format=true"

all_stations <- 
  # Step 1: Read in the data.
  read_csv(url) %>% 
  # Step 2: filter columns and rename stationname
  dplyr::select(station = stationname, date, rides) %>% 
  # Step 3: Convert the character date field to a date encoding.
  # Also, put the data in units of 1K rides
  mutate(date = mdy(date), rides = rides / 1000) %>% 
  # Step 4: Summarize the multiple records using the maximum.
  group_by(date, station) %>% 
  summarize(rides = max(rides), .groups = "drop")

head(all_stations)
