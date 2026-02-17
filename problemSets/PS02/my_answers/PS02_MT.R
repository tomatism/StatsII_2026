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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "kableExtra", "stargazer", "modelsummary"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport$countries)
climateSupport$countries <- factor(climateSupport$countries,
                                   ordered = FALSE)
str(climateSupport$sanctions)
climateSupport$sanctions <- factor(climateSupport$sanctions,
                                   ordered = FALSE)

##  Question 1 ##
## Fitting an additive model 

add_model <- glm(choice ~ countries + sanctions,
                  family = binomial(link = "logit"), climateSupport)


summary(add_model)
null_model <- glm(choice ~ 1, data = climateSupport,
                  family = binomial(link = "logit"))
anova(add_model, null_model, test = "LRT")

stargazer(add_model, type = "latex",
          title = "Additive Logistic Regression",
          dep.var.labels = c("Agreement with Policy"),
          covariate.labels = c("Participating Countries: 80 of 192", "Participating Countries: 160 of 192",
                               "Sanctions: 5", "Sanctions: 15", "Sanctions: 20")) 

## Question 2 ##

## Checking the change in betas

coefficients <- add_model$coefficients
coefficients["sanctions15%"] - coefficients["sanctions5%"]
OR <- exp(delta_logodds)

paste("The change in log-odds is equal to:", delta_logodds,
"The multiplicative factor (OR) associated with the change is equal to:", OR)

## Estimated probability ##

newdata <- data.frame(
  countries = factor("80 of 192", levels = levels(climateSupport$countries)),
  sanctions = factor("None", levels = levels(climateSupport$sanctions))
)

probability <- as.numeric(predict(add_model, newdata = newdata, type = "response"))

logodds <- predict(add_model, newdata = newdata, type = "link")
prob_check <- as.numeric(exp(logodds) / (1 + exp(logodds)))

print(c(probability, prob_check))

## Question 3 ##

## Creating an interaction model ##

int_model <- glm(choice ~ countries + sanctions + countries * sanctions,
                 family = binomial(link = "logit"), climateSupport)
anova(int_model, add_model, test = "LRT")

