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

# Fit global model 
m_h1 <- glm(
  data = data,
  family = gaussian(link = "identity"),
  fly_mass ~ 1 + conc_n
)

# Fit null model 
m_h0 <- glm(
  data = data,
  family = gaussian(link = "identity"),
  fly_mass ~ 1
)

# Use LRT to test for significant relationship 
lmtest::lrtest(m_h0, m_h1)

# Yes, there is a statistically significant relationship between fly mass 
# and the concentration of nitrogen (X2 = 142.54, df = 1, P < 0.001).

## Question #2
## - How much does `fly_mass` increase/decrease for each 1 unit increase in `conc_n`?

summary(m_h1)

# Fly mass decreases by 3.04 units, on average, for every 1 additional unit of 
# nitrogen. 

## Question #3
## - What is the predicted `fly_mass` when `conc_n` = 0? 

# Predicted fly mass when nitrogen concentration equals 0 = 100.62 grams
# (= (Intercept) estimate)