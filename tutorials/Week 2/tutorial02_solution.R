##################
#### Stats II ####
##################

###############################
#### Tutorial 2: GLMs ####
###############################

# In today's tutorial, we'll begin to explore GLMs
#     1. Import/wrangle data
#     2. Execute lm() and glm() of RQ
#     3. Compare models

#### Case study
# We're interested in central bank governors, specifically their occupational turnover, for almost all countries in the world starting from the year 1970

#### Creat the dataset
# For this task, we first need data.
# 1. Go to https://kof.ethz.ch/en/data/data-on-central-bank-governors.html and download the data on Central Bank Governors
# https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/central_bank_governors/cbg_turnover_v23upload.xlsx
# 2. Gather necessary variables
#    codewdi: Country code or name
#    year
#    time to regular turnover	
#    regular turnover dummy	
#    irregular turnover dummy	
#    legal duration

#### Import the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in your libraries

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("tidyverse", "ggplot2", "readxl"),  pkgTest)


# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## loading the data
data_raw <- read_excel("dataset_t2.xlsx")
str(data_raw)  
names(data_raw)

# MAKE SURE THERE AREN'T MISSING VALUES!
# Now, you've got your dataset

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 

data <- data_raw %>%
  select(
    codewdi,
    country,
    year,
    `time to regular turnover`,
    `regular turnover dummy`,
    `irregular turnover dummy`,
    `legal duration`
  ) %>%
  mutate(
    codewdi = as.factor(codewdi),
    country = as.factor(country),
    year = as.integer(year),
    time_to_regular_turnover = as.integer(`time to regular turnover`),
    regular_turnover = as.integer(`regular turnover dummy`),
    irregular_turnover = as.integer(`irregular turnover dummy`),
    legal_duration = as.integer(`legal duration`)
  ) %>%
  drop_na()

data <- data %>%
  select(
    codewdi,
    country,
    year,
    time_to_regular_turnover,
    regular_turnover,
    irregular_turnover,
    legal_duration
  )

str(data)

unique(data$year) 
unique(data$time_to_regular_turnover)
unique(data$regular_turnover)
unique(data$irregular_turnover)
unique(data$legal_duration)


bad_codes <- c(-999, -666, -555, -881)

data <- data %>%
  mutate(
    across(
      c(year, time_to_regular_turnover, regular_turnover, irregular_turnover, legal_duration
      ),
      ~ replace(., . %in% bad_codes, NA)
    )
  )

lapply(
  data[c("year",
         "time_to_regular_turnover",
         "regular_turnover",
         "irregular_turnover",
         "legal_duration")],
  unique
)

data <- data %>% drop_na()

#### Descriptive patterns in turnover
# Compute the average turnover rate (mean of turnover) by country over the full sample period

country_turnover <- data %>%
  group_by(country) %>%
  summarize(
    avg_turnover = mean(irregular_turnover),
    n = n()
  )

# In a given year, how likely is it that the central bank governor is removed irregularly in this country?

# (a) Which five countries have the highest average turnover rates?
country_turnover %>%
  arrange(desc(avg_turnover)) %>%
  slice(1:5)

#In almost 47% of years, Bolivia experienced an irregular replacement of its central bank governor.
#That is extremely high and signals:political interference; regime instability; weak de facto central bank independence
  
# (b) Which five have the lowest average turnover rates?
country_turnover %>%
  arrange(avg_turnover) %>%
  slice(1:5)

#Across all observed years, there were no irregular removals of central bank governors

# (c) Plot the distribution of country‑level average turnover rates (e.g. histogram or density) 
#     Briefly comment on whether high turnover is concentrated in a small set of countries

ggplot(country_turnover, aes(x = avg_turnover)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    x = "Average Irregular Turnover Rate",
    y = "Number of Countries",
    title = "Distribution of Country-Level Irregular Turnover Rates"
  )

####  Estimate a linear probability model (LPM) with OLS:
  
# (a) Fit lm() with:
  # Outcome: irregular turnover dummy
  # Covariates: 
  #   time to regular turnover	
  #   legal duration

lpm <- lm(
  irregular_turnover ~ time_to_regular_turnover + legal_duration,
  data = data
)

summary(lpm)
## Interpretation:
## Intercept: When time_to_regular_turnover = 0 and legal_duration = 0, 
##the predicted probability of irregular turnover is 14.9%.

## B1: time_to_regular_turnover: 0.0626
##Each additional year farther away from a scheduled (regular) turnover is associated with a 6.26 percentage point increase in the probability of irregular turnover, holding legal duration constant.
##Governors who are early in their term are more likely to be removed irregularly
##Approaching the end of a term reduces the need for political interference

## B2: legal_duration = -0.0418
##Each additional year in the legally mandated term length reduces the probability of irregular turnover by about 4.18 percentage points, holding timing constant.
##Longer legal terms → stronger protection
##Short terms → easier political interference

# (b) For a “typical” observation  (e.g. median time to regular turnover & legal duration), compute the predicted probability

typical <- data.frame(
  time_to_regular_turnover = median(data$time_to_regular_turnover, na.rm = TRUE),
  legal_duration = median(data$legal_duration, na.rm = TRUE)
)

predict(lpm, newdata = typical)
#This is the predicted probability of irregular turnover for a country-year with median distance to regular turnover and median legal term length.
## For a country-year with median time to regular turnover (3 years) and median legal duration (5 years), the model predicts a 12.8% probability of irregular governor turnover.

# (c) Identify at least one observation for which lm() prediction is below 0 or above 1 and explain why such predictions are problematic for a probability
data$lpm_pred <- predict(lpm)

data %>%
  filter(lpm_pred < 0 | lpm_pred > 1) %>%
  select(country, year, lpm_pred) %>%
  head()
## LPM probabilities are not bounded as it assumes linearity; Probabilities must lie in [0,1]


# Using the full sample, construct a plot of predicted probability of turnover vs time to regular turnover:
grid <- data.frame(
  time_to_regular_turnover = seq(
    min(data$time_to_regular_turnover, na.rm = TRUE),
    max(data$time_to_regular_turnover, na.rm = TRUE),
    length.out = 100
  ),
  legal_duration = median(data$legal_duration, na.rm = TRUE)
)

grid$lpm_fit <- predict(lpm, newdata = grid)

library(ggplot2)

ggplot(grid, aes(x = time_to_regular_turnover, y = lpm_fit)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Time to Regular Turnover",
    y = "Predicted Probability of Irregular Turnover",
    title = "LPM Predicted Probability"
  )

#### Baseline logistic regression
  
# Estimate a logistic regression with governor turnover as the binary outcome and same covariates using glm(family = "binomial")
  
logit <- glm(
  irregular_turnover ~ time_to_regular_turnover + legal_duration,
  data = data,
  family = binomial
)

summary(logit)

# (a) Report coefficient estimates and standard errors

# (b) Interpret the sign of each coefficient in terms of how they affect the probability of turnover

# time_to_regular_turnover:
# Interpretation: +5.193
# Being further away from a scheduled (regular) turnover substantially increases the probability that a governor is removed irregularly.

# legal_duration:
# Interpretation: −4.979
# Longer legally mandated terms reduce the probability of irregular turnover.


# (c) For the same “typical” observation used above, compute the predicted probability of turnover (type = "response"), and compare it to the lm() prediction

predict(logit, newdata = typical, type = "response")

# Interpretation:
# For:time_to_regular_turnover = 3;legal_duration = 5,
# The predicted probability is:0.003% (essentially zero)
# Compare to LPM: ~12–13%
# Why such a huge difference?
# Because: LPM extrapolates linearly; Logit respects probability bounds;
# Logit pushes predictions toward 0 when covariates imply stability;
# Logit reveals that “typical” cases are extremely stable, and irregular turnover is driven by extreme cases.

#### Compare lm() and glm()  

# (a) Use the lm() to compute fitted values across the observed range of time to regular turnoner, holding legal duration at median value
grid <- data.frame(
  time_to_regular_turnover = seq(
    min(data$time_to_regular_turnover, na.rm = TRUE),
    max(data$time_to_regular_turnover, na.rm = TRUE),
    length.out = 100
  ),
  legal_duration = median(data$legal_duration, na.rm = TRUE)
)

grid$lpm_fit <- predict(lpm, newdata = grid)

# (b) Use the logit model to compute fitted probabilities for the same legal duration values
grid$logit_fit <- predict(logit, newdata = grid, type = "response")

# (c) Plot both curves on the same graph (e.g. blue for lm(), red for glm()) 

ggplot(grid, aes(x = time_to_regular_turnover)) +
  geom_line(aes(y = lpm_fit, color = "LPM"), linewidth = 1) +
  geom_line(aes(y = logit_fit, color = "Logit"), linewidth = 1) +
  scale_color_manual(values = c("LPM" = "blue", "Logit" = "red")) +
  labs(
    x = "Time to Regular Turnover",
    y = "Predicted Probability of Irregular Turnover",
    color = "Model",
    title = "LPM vs Logit Predictions"
  )

# Interpretation of the plot:
# LPM (blue): Linear
  # Can exceed 0 and 1; Overstates probability in “typical” cases
# Logit (red): S-shaped
  # Bounded; Shows irregular turnover is rare except in extreme situations
# This is the core justification for using logit.

#### Country heterogeneity and fixed effects

# (a) Introduce country fixed effects into the logit specification using dummy variables 

logit_fe <- glm(
  irregular_turnover ~ time_to_regular_turnover + legal_duration + factor(country),
  data = data,
  family = binomial
)

summary(logit_fe)
# (b) Compare the estimated coefficients with and without country fixed effects. How does controlling for unobserved country characteristics affect the relationships w/ turnover?

##Fixed effects eliminate all time-invariant cross-country differences.
##Within a given country, as the timing within the term or legal duration changes, how does the probability of irregular turnover change, keeping legal duration constant?
##This is a within-country question.

# Interpretation:
  # Time to Regular Turnover: +5.88 (The coefficient remains positive, large (in fact larger), and highly significant)
  # For a given country, being further away from regular turnover substantially increases the probability of irregular removal.

  # Legal Duration: −5.56 (Still strongly negative; Slightly larger in magnitude)
  # When a country changes its legal term length (rare, but it happens), longer terms sharply reduce irregular turnover within that country.

##Controlling for country fixed effects does not eliminate the effects of timing within the term or legal duration. 
##Both covariates remain strongly significant and even increase in magnitude, indicating that the relationships are 
##not driven by cross-country institutional differences but reflect robust within-country dynamics.
  
  # (c) What kinds of country‑specific factors might be absorbed by these fixed effects in this context

#In this context, fixed effects absorb anything that: 
  #Differs across countries
  #Is constant (or nearly constant) over time
  #Eg: regime type (democracy vs autocracy); appointment rules; size of the economy; level of development; etc.
  #Once FE are included, none of these can explain the remaining variation.

###Using country fixed effects helps controlling for all time-invariant country-specific characteristics. 
###Comparing the logit models with and without fixed effects shows that the estimated effects of time to regular turnover and legal term length 
###remain large and statistically significant, and even increase in magnitude. This indicates that the relationship is not driven by cross-country 
###heterogeneity but reflects robust within-country dynamics. 
###The fixed effects absorb persistent country characteristics such as regime type, political culture, historical stability, and institutional quality.
