#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("nnet", "MASS", "tidyverse", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data_raw <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

gdp_data <- gdp_data_raw

## Question 1
# Data manipulation
str(gdp_data$GDPWdiff)

gdp_data <- gdp_data |>
  mutate(
    GDPWdiff = case_when(
      GDPWdiff < 0  ~ "Negative",
      GDPWdiff == 0 ~ "No_change",
      GDPWdiff > 0  ~ "Positive"))|>
  mutate(GDPWdiff = as.factor(GDPWdiff))

str(gdp_data$REG)
str(gdp_data$OIL)

gdp_data$REG <- factor(gdp_data$REG,
                       levels = c(0,1),
                       labels = c("Non_Democracy", "Democracy"))

gdp_data$OIL <- factor(gdp_data$OIL,
                       levels = c(0,1),
                       labels = c("Larger_50", "Lower"))

gdp_data <- gdp_data |>
  select(GDPWdiff, REG, OIL)

# Multinomial logit (Unordered)

gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "No_change")
mult.log <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)
summary(mult.log)

stargazer(mult.log, type = "latex",
          title = "Multinomial Logistic Regression",
          dep.var.labels = c("Neg GDP Diff", "Pos GDP Diff"),
          covariate.labels = c("Democracy", "Low Fuel Exp")) 

#Ordered Multinomial logit 
levels(gdp_data$GDPWdiff)

gdp_data$GDPWdiff <- ordered(
  gdp_data$GDPWdiff,
  levels = c("Negative", "No_change", "Positive"))

ord.log <- polr(
  GDPWdiff ~ REG + OIL, data = gdp_data, Hess = TRUE)
ctable <- coef(summary(ord.log))
p_values <- pnorm(
  abs(ctable[, "t value"]),
  lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_values)
p_vals <- ctable[, "p value"]

cat("The cut-off points are:\n",
    paste( paste0(ord.log$zeta, 2)),
           collapse = "\n")

stargazer(ord.log, type = "latex",
  p  = list(p_vals),
  title = "Ordered Logit Model",
  dep.var.labels = "GDP Change",
  covariate.labels = c("Democracy", "Low Fuel Exp")) 

#####################
# Problem 2
#####################

# load data
mexico_elections_raw <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

mexico_elections <- mexico_elections_raw 

str(mexico_elections$PAN.visits.06)
str(mexico_elections$PAN.governor.06)
str(mexico_elections$competitive.district)
str(mexico_elections$marginality.06)

mexico_elections <- mexico_elections|>
  select(PAN.visits.06, competitive.district, marginality.06, PAN.governor.06)|>
  mutate(PAN.governor.06 = as.factor(PAN.governor.06),
         competitive.district = as.factor(competitive.district))
## Question a & b

pois_06 <- glm(PAN.visits.06 ~ ., data = mexico_elections, family = poisson)
summary(pois_06)

stargazer(pois_06, type = "latex",
          title = "Poisson Regression",
          dep.var.labels = c("Number of Visits in 2006"),
          covariate.labels = c("Contested District", "District Poverty", 
                               "PAN affiliated Gov")) 

exp(pois_06$coefficients)

## Question c 

new_data <- data.frame(
  competitive.district = 
    factor(1, levels = levels(mexico_elections$competitive.district)),
  marginality.06 = 0,
  PAN.governor.06 = 
    factor(1, levels = levels(mexico_elections$PAN.governor.06))
)

predict(pois_06, new_data, type = "response")
