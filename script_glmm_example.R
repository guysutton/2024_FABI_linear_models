###########################################################################
# Load required packages --------------------------------------------------
###########################################################################

library(tidyverse)
library(tidyr)
library(lme4)

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

######################################
# Example #1: Each tank is a genotype
######################################

# Here, each tank contains all 20 reps for each plant genotype
# - So, the data are NOT independent whatsoever.
# - We cannot separate an effect of the tank (e.g. shade, soil) from a
#   plant genotype effect

# Simulate some data
set.seed(2002)
data <- tidyr::crossing(
  genotype = c(
    "A",
    "B",
    "C",
    "D",
    "E"
  ),
  rep = 1:20
) %>%
  dplyr::mutate(count = rpois(n = 100, lambda = 5)) %>%
  dplyr::mutate(
    tank = dplyr::case_when(
      genotype == "A" ~ "Tank 1",
      genotype == "B" ~ "Tank 2",
      genotype == "C" ~ "Tank 3",
      genotype == "D" ~ "Tank 4",
      TRUE ~ "E"
    )
  ) %>%
  dplyr::select(genotype, tank, rep, count)
head(data)

# Plot data
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot() +
  labs(
    x = "Plant genotype",
    y = "Insect count"
  )

# Poisson GLM
# - Here, we model insect counts (no. of insects emerged) as a function
#   of plant genotype
m1 <- glm(
  data = data, 
  family = poisson(link = "log"), 
  count ~ genotype
)
DHARMa::simulateResiduals(fittedModel = m1, plot = T)
car::Anova(m1, test = "LR")

# Model diagnostics look good, right?
# - They are misleading, because model diagnostics don't really
#   allow us to test whether data is independent or not.
#   - You have to evaluate whether your model accurately captures
#     the non-independence between data points based on your knowledge
#     of the experimental design. 

# Unfortunately, you can still assess parameter significance and 
# calculate p-values, even when your model is not right, most of the time.


# GLMM
# - Let's add a random effect for tank to account for non-independence between
#   data points from the same tank.
m1 <- lme4::glmer(
  data = data, 
  family = poisson(link = "log"), 
  count ~ genotype + (1| tank)
)

# Why doesn't this work?
# - The singular fit means that the model cannot find a solution.

# - The reason is that the model can't distinguish between a treatment effect
#   and a tank effect:
#   - e.g. if tank 1 (representing 1 genotype) was 20% more shaded than 
#     the other 3 tanks, we could find that insect abundance was 20% lower 
#     in this genotype.
#     - But, using the current model, we cannot distinguish between 
#       whether there is a plant genotype effect, or just a tank effect.

# In this example, there is no way to account for the poor experimental design in
# your statistical analysis. No mixed model can help you here.
# - You effectively only have a single replicate for
#   each plant genotype (pseudoreplication),
#   and each of the 20 plants in each tank are sub-samples. 


######################################
# Example #2: Each tank contains 4 reps for each plant genotype
######################################

# Here, each of the 5 tanks contains 4 reps for each plant genotype
# - So, the data are now independent
# - We can separate an effect of the tank (e.g. shade) from a
#   plant genotype effect

# Simulate data
set.seed(2012)
data <- crossing(
  genotype = c(
    "A",
    "B",
    "C",
    "D",
    "E"
  ),
  rep = 1:20
) %>%
  dplyr::mutate(count = rpois(n = 100, lambda = 5)) %>%
  dplyr::mutate(
    tank = dplyr::case_when(
      rep %in% c(1, 2, 3, 4) ~ "Tank 1",
      rep %in% c(5, 6, 7, 8) ~ "Tank 2",
      rep %in% c(9, 10, 11, 12) ~ "Tank 3",
      rep %in% c(13, 14, 15, 16) ~ "Tank 4",
      rep %in% c(17, 18, 19, 20) ~ "Tank 5"
    )
  ) %>%
  dplyr::select(genotype, tank, rep, count)
head(data)

# Plot data
# - Each panel is a distinct tank and each boxplot represents 4 plants 
#   per genotype per tank 
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot() +
  facet_wrap(~ tank, nrow = 1)

# GLMM
# - Let's add a random effect for tank to account for non-independence between
#   data points from the same tank.
m2 <- lme4::glmer(
  data = data, 
  family = poisson(link = "log"), 
  count ~ genotype + (1| tank)
)

DHARMa::simulateResiduals(fittedModel = m2, plot = T)
car::Anova(m2, test = "Chisq")

# Calculate GLMM-R2
MuMIn::r.squaredGLMM(m2)

# The R2m (delta = 0.0146) is the proportion of variance explained by the
# fixed effects (plant genotype) (R2 = 1.46%), while the R2c is the proportion of
# variance explained by the plant genotype fixed effect and the 
# random effect for tank (R2c = 3.11%).
# - So, minus R2m from R2c to calculate the variation explained by 
#   the random effect
#   - The the tank accounts for 1.65% of the total variation in the model.

######################################
# Example #3: Each tank contains 4 reps for each plant genotype
#             - simulating some random noise affecting one tank
######################################

# Here, each tank of the 5 tanks contains 4 reps for each plant genotype
# - So, the data are now independent
# - We can separate an effect of the tank (e.g. shade) from a
#   plant genotype effect
# - Here, we artificially double the number of insects produced in tank 1
#   (simulating a shading effect, or wind-effect)

# Simulate data
set.seed(2012)
data <- crossing(
  genotype = c(
    "A",
    "B",
    "C",
    "D",
    "E"
  ),
  rep = 1:20
) %>%
  dplyr::mutate(count = rpois(n = 100, lambda = 5)) %>%
  dplyr::mutate(
    tank = dplyr::case_when(
      rep %in% c(1, 2, 3, 4) ~ "Tank 1",
      rep %in% c(5, 6, 7, 8) ~ "Tank 2",
      rep %in% c(9, 10, 11, 12) ~ "Tank 3",
      rep %in% c(13, 14, 15, 16) ~ "Tank 4",
      rep %in% c(17, 18, 19, 20) ~ "Tank 5"
    )
  ) %>%
  dplyr::select(genotype, tank, rep, count)
head(data)

# Artificially double the insect counts in tank 1
data_tank1 <- data %>%
  dplyr::filter(tank == "Tank 1") %>%
  dplyr::mutate(count = count * 2)

# Data from the other tanks
data_tanks_all <- data %>%
  dplyr::filter(!tank == "Tank 1")

# Combine the new data
data <- dplyr::bind_rows(data_tank1, data_tanks_all)

# Plot data (all replicates - no genotype effect)
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot()

# Plot data
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot() +
  facet_wrap(~ tank, nrow = 1)

# Poisson GLM
m3 <- glm(
  data = data,
  family = poisson(link = "log"),
  count ~ genotype
)
DHARMa::simulateResiduals(fittedModel = m3, plot = T)
car::Anova(m3, test = "LR")

# Significant issue with our fixed-effects model only.
# - We haven't accounted for the random shade effect on tank 1.

# GLMM
# - Here, we model the insect counts as a function of plant genotype,
#   accounting for the random and non-random variation between tanks.
#   - i.e. we can measure how much variation occurs between the tanks caused
#          some other factor (e.g. wind, shade) before calculating the
#          main fixed effect of interest (plant genotype).
#          - we can only do this because we randomised the allocation of the
#            reps across the different tanks, we could never have done this
#            with all the reps for one genotype in one tank.
m4 <- glmer(
  data = data,
  family = poisson(link = "log"), 
  count ~ genotype + (1 | tank)
)
DHARMa::simulateResiduals(fittedModel = mod4, plot = T)

# The model with the random effect is significantly better fit.

# Still no evidence of a genotype effect (correctly so!)
car::Anova(m4, test = "Chisq")

# Calculate GLMM-R2
MuMIn::r.squaredGLMM(m4)

# The R2m (delta = 0.010) is the proportion of variance explained by the
# fixed effects (plant genotype) (R2 = 1.04%), while the R2c is the 
# proportion of variance explained by the plant genotype fixed effect 
# and the random effect for tank (R2c = 43.8%).
# - So, minus R2m from R2c to calculate the variation explained by 
#   the random effect
#   - The the tank accounts for 42.76% of the total variation in the model.
# - Our model was able to account for the extra variation introduced by 
#   the shade effect on tank 1: 
#   - Notice how the variation for the random effect increased hugely, 
#     but we didn't overestimate any difference in the main effect... 

# Beautiful!!!










######################################
# Example #4: Each tank contains 4 reps for each plant genotype
#             - simulating some random noise affecting one tank +
#               adding a plant genotype effect
######################################

# Here, each tank of the 5 tanks contains 4 reps for each plant genotype
# - So, the data are now independent
# - We can separate an effect of the tank (e.g. shade) from a
#   plant genotype effect
# - Here, we artifically double the number of insects produced in tank 1
#   (simulating a shading effect, or soil-effect), and artficially make
#   the counts for genotype D double that of the 
#   other counts (there should now be a significant plant genotype effect)

# Simulate data
set.seed(2012)
data <- crossing(
  genotype = c(
    "A",
    "B",
    "C",
    "D",
    "E"
  ),
  rep = 1:20
) %>%
  dplyr::mutate(count = rpois(n = 100, lambda = 5)) %>%
  dplyr::mutate(
    tank = dplyr::case_when(
      rep %in% c(1, 2, 3, 4) ~ "Tank 1",
      rep %in% c(5, 6, 7, 8) ~ "Tank 2",
      rep %in% c(9, 10, 11, 12) ~ "Tank 3",
      rep %in% c(13, 14, 15, 16) ~ "Tank 4",
      rep %in% c(17, 18, 19, 20) ~ "Tank 5"
    )
  ) %>%
  dplyr::select(genotype, tank, rep, count)
head(data)

# Double the insect counts in tank 1
data_tank1 <- data %>%
  dplyr::filter(tank == "Tank 1") %>%
  dplyr::mutate(count = count * 2)

# Data from the other tanks
data_tanks_all <- data %>%
  dplyr::filter(!tank == "Tank 1")

# Combine the new data
data <- dplyr::bind_rows(data_tank1, data_tanks_all)

# Make genotype D counts 2x higher
data_genotype <- data %>%
  dplyr::filter(genotype == "D") %>%
  dplyr::mutate(count = count * 2)

# Data from the other plant genotypes
data_tanks_all <- data %>%
  dplyr::filter(!genotype == "D")

# Combine
data <- dplyr::bind_rows(data_genotype, data_tanks_all)
head(data)

# Plot data
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot()

# Plot data
data %>%
  ggplot(data = ., aes(x = genotype,
                       y = count)) +
  geom_boxplot() +
  facet_wrap(~ tank, nrow = 1)

# Poisson GLM
m5 <- glm(
  data = data,
  family = poisson(link = "log"),
  count ~ genotype
)
DHARMa::simulateResiduals(fittedModel = m5, plot = T)

# Significant issues with our fixed-effects model only.
# - We haven't accounted for the random shade effect on tank 1.

# GLMM
# - Here, we model the insect counts as a function of plant genotype,
#   accounting for the random and non-random variation between tanks.
#   - i.e. we can measure how much variation occurs between the tanks caused
#          some other factor (e.g. wind, shade) before calculating the
#          main fixed effect of interest (plant genotype).
#          - we can only do this because we randomised the allocation of the
#            reps across the different tanks, we could never have done this
#            with all the reps for one genotype in one tank.
m6 <- lme4::glmer(
  data = data,
  family = poisson(link = "log"), 
  count ~ genotype + (1 | tank)
)
DHARMa::simulateResiduals(fittedModel = m6, plot = T)

# The model with the random effect is significantly better fit.

# Assess parameter significance
car::Anova(m6, test = "Chisq")

# Calculate GLMM-R2
MuMIn::r.squaredGLMM(m6)

# The R2m (delta = 0.258) is the proportion of variance explained by the
# fixed effects (plant genotype) (R2 = 25.8%), while the R2c is the
# proportion of # variance explained by the plant genotype fixed effect 
# and the random effect for tank # R2c = 60.2%).
# - So, minus R2m from R2c to calculate the variation explained by the 
#   random effect
#   - The the tank accounts for 34.22% of the total variation in the model.
#   - Our model was able to account for the extra variation introduced 
#     by the shade effect on tank 1, and identify the plant genotype effect!!!

preds1 <- ggeffects::ggeffect(
  model = m6, 
  terms = c("genotype"),
  type = "re"
  ) %>%
  as.data.frame()%>%
  dplyr::mutate(model = "GLMM")
preds2 <- ggeffects::ggeffect(
  model = m5, 
  terms = c("genotype"),
  type = "fe"
) %>%
  as.data.frame() %>%
  dplyr::mutate(model = "GLM")
preds <- dplyr::bind_rows(
  preds1,
  preds2
)
head(preds)


preds %>%
  ggplot(aes(
    x = x,
    y = predicted,
    colour = model,
    group = model
  )) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    position = position_dodge(width = 0.2)
  ) +
  theme(legend.position = "right") +
  labs(x = "Plant genotype",
       y = "No. of F1 insects produced \n(95% confidence interval)",
       colour = "Model") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 4))
