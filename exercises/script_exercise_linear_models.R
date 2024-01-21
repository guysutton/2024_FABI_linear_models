# Run rows 3 - 23 and then answer the questions based on the 'Intro to Linear Model' section

# Use package manager to check, install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # List the packages you need below
  tidyverse
)

# Set reproducible seed 
set.seed(1234)

# Generate simulated data (continuous x variable)
data <- tibble(
  x1 = rnorm(n = 100, mean = 10, sd = 3),
  error = rnorm(n = 100, mean = 0, sd = 5),
  y = 100 - (3 * x1) + error
) %>%
  dplyr::select(
    conc_n = x1, 
    fly_mass = y
  )
head(data)

## Question #1 
## - Is there a statistically significant relationship between `fly_mass` and `conc_n`? 




## Question #2
## - How much does `fly_mass` increase/decrease for each 1 unit increase in `conc_n`?



## Question #3
## - What is the predicted `fly_mass` when `conc_n` = 0? 







