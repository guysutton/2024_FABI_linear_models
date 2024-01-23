# Exercise: Fit probit models to compare insect survival on three 
# different diet treatments, and calculate LC50, LC90 and LC99 dose responses 

###########################################################################
# Load required packages --------------------------------------------------
###########################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr, 
               DHARMa,
               MuMIn,
               janitor,
               car,
               MASS,
               ecotox)

###########################################################################
# Set global defaults -----------------------------------------------------
###########################################################################

# Set ggplot theme (makes nice plots)
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))


# Write a custom function to calculate 95% confidence intervals around the LC50, LC90 and 
# LC99 from a fitted probit model 
calc_lc_ci <- function(model){
  xp <- dose.p(model, p = c(0.50, 0.90, 0.99))
  xp.ci <- xp + attr(xp, "SE") %*% matrix(qnorm(1-0.05/2)*c(-1,1), nrow = 1)
  zp.est <- cbind(xp, attr(xp, "SE"), xp.ci[,1], xp.ci[,2])
  dimnames(zp.est)[[2]] <- c("LD", "SE", "LCL", "UCL")
  zp.est
}

###########################################################################
# Import data -------------------------------------------------------------
###########################################################################

# Import data
data <- readxl::read_excel(here::here("./data/LLT_adults.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::select(
    diet,
    temperature,
    no_exposed,
    no_dead
  )
head(data)


###########################################################################
# Visualise data ----------------------------------------------------------
###########################################################################

# Do lethal limits vary between diet treatments? 
data %>%
  dplyr::mutate(prop_dead = no_dead / no_exposed) %>%
  ggplot(data = ., aes(x = temperature,
                       y = prop_dead,
                       colour = diet)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "right")

# It appears that insects are less tolerant of lower temperatures on the low diet versus the standard
# diet, and the high diet seemed to be the most effective at buffering the insects against cold... 

###########################################################################
# Modelling ----------------------------------------------------------
###########################################################################

###########################
# Model #1 - additive model 
###########################

# I assume that you want to test the hypothesis that the LT/CT limits differ between diet treatments? 
# - The model belows fits a probit GLM which asks, 
#   - Does the proportion of insects that died vary with temperature and the diet treatment? 
mod1 <- glm(cbind(no_dead, no_exposed - no_dead) ~
              # What variables could explain the response variable?
              1 + diet + temperature,
            data = data,
            # Let's fit a probit link function
            family = binomial(link = "probit")
)

# Check model fit 
DHARMa::simulateResiduals(fittedModel = mod1, plot = T)

# Model seems to fit fine - KS test P > 0.05 and no problems in residual vs fitted plot 

# Perform hypothesis test 
# - Uses a Likelihood Ratio Test with type II comparisons 
car::Anova(
  mod1, 
  type = "II", 
  test = "LR"
  )

# So, the results tell us that:
# - Temperature had a significant effect on insect survival (X2 = 239.12, df = 1, p < 0.001).
# - Diet treatment had a significant effect on insect survival (X2 = 33.94, df = 2, p < 0.001). 
#   - NOTE: The diet treatment effect here is averaged over all temperatures. 
#           - I can't believe that is a very interesting hypothesis, and not what you want to test, per se. 

##############################
# Model #2 - interaction model 
##############################

# - The model belows fits a probit GLM which asks, 
#   - Does the proportion of insects that died vary with temperature and the diet treatment,
#     and we fit an interaction term to test the hypothesis that the temperature effect 
#     varies between treatment?
#     - Model #1 differs from this model in that in model #1 we assume that the effect of 
#       temperature is consistent for all three diets.
#     - Model #2 allows the effect of temperature to vary between the three diets. 
mod2 <- glm(cbind(no_dead, no_exposed - no_dead) ~
              # What variables could explain the response variable?
              1 + diet + temperature + diet:temperature,
            data = data,
            # Let's fit a probit link function
            family = binomial(link = "probit")
)

# Check model fit 
DHARMa::simulateResiduals(fittedModel = mod2, plot = T)

# Model seems to fit fine - KS test P > 0.05 and no problems in residual vs fitted plot 

# Perform hypothesis tests 
# - Uses a Likelihood Ratio Test with type III comparisons 
# - Use type = III when an interaction term is included (*)
car::Anova(
  mod2, 
  type = "III", 
  test = "LR"
)

# So, the results tell us that:
# - There was a significant temperature by diet interaction effect (X2 = 19.31, df = 2, P < 0.001). 
#   - This means that insect survival was effected by the diet treatment applied and temperature. 
#   - However, the temperature effect was dependent on the diet treatment applied, which I think is 
#     probably the hypothesis that you really want to be testing. 
#     - The interaction term translates into a biological interpretation of the diet treatment 
#       mediates the effect of temperature on insect survival. Very cool!!! 
# - NOTE: YOU DO NOT REPORT THE SINGULAR EFFECTS OF DIET AND TREATMENT WHEN THE INTERACTION TERM 
#         IS SIGNIFICANT!!! 

###########################################################################
# Plot model predictions --------------------------------------------------
###########################################################################

# Extract predictions from the fitted model
preds <-
  ggeffects::ggeffect(mod2,
                      terms = c("temperature [-14:0 by = 0.1]", "diet"),
                      type = "fe") %>%
  as.data.frame(.) %>%
  dplyr::rename(temperature = x,
                diet = group)
head(preds)

# Make plot
preds %>%
  dplyr::mutate(diet = dplyr::if_else(diet == "STD", "Std", as.character(diet))) %>%
  ggplot(data = ., aes(
    x = temperature,
    y = predicted,
    colour = diet,
    fill = diet
  )) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  scale_x_continuous(limits = c(-14, 0),
                     breaks = seq(-14, 0, 1)) +
  theme(legend.position = "right") +
  labs(x = "Temperature",
       y = "Proportion of insects that died",
       fill = "Diet Treatment") +
  guides(colour = "none")


# If you want the x-axis reversed (this plot makes more sense to me)
preds %>%
  dplyr::mutate(diet = dplyr::if_else(diet == "STD", "Std", as.character(diet))) %>%
  ggplot(data = ., aes(
    x = temperature,
    y = predicted,
    colour = diet,
    fill = diet
  )) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  # Note the use of scale_x_reverse
  scale_x_reverse(limits = c(0,-14),
                  breaks = seq(0,-14,-1)) +
  theme(legend.position = "right") +
  labs(x = "Temperature",
       y = "Proportion of insects that died",
       fill = "Diet Treatment") +
  guides(colour = "none")


###########################################################################
# Calculating LC values _--------------------------------------------------
###########################################################################

# To find lethal concentration values (e.g. LC50, LC90), we have to refit a probit 
# model to each group that we want to calculate the LC values for individually.
# - E.g. Continuing on with the example above, we need to refit 3 probit GLM's, 
#   - 1. High diet only
#     2. Low diet only
#     3. Std diet only 

# Below, I show how to do this for the high diet treatment 

# Refit high diet probit GLM 
mod_high <- glm(cbind(no_dead, no_exposed - no_dead) ~
              1 + temperature,
            data = data,
            # Note we subset the data like this
            subset = c(diet == "High"),
            family = binomial(link = "probit"))

# Can calculate any LC values we want by specifying the numbers below
# e.g. p = 0.50 = LC50, p = 0.90 = LC90
lc_data_high <- MASS::dose.p(mod_high,
                             p = c(0.5, 0.9, 0.99))
lc_data_high

# I wrote a custom function to calculate 95% confidence intervals around the 
# LC50, LC90 and LC99 values at the start of this script
# - You can change the LC values you want CI's for in the function definition, not here 
# - LCL = lower 95% confidence interval
# - HCL = upper 95% confidence interval 
calc_lc_ci(mod_high)


