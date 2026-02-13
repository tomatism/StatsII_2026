##################
#### Stats II ####
##################

###############################
#### Tutorial 4: Logit ####
###############################

# In today's tutorial, we'll begin to explore logit regressions
#     1. Estimate logit regression in R using glm()
#     2. Practice makes inferences using logit regression
#     3. Compare logit models

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

lapply(c("tidyverse", "ggplot2", "stringr", "lmtest"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

str(graduation)

yn_var <- c("hsgrad", "mhs", "fhs", "intact", "nonwhite")
graduation[yn_var] <- lapply(graduation[yn_var], as.factor)
str(graduation)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.

modela <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
              family = binomial(link = "logit"), graduation)
summary(modela)


# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 

modela_null <- glm(hsgrad ~ 1, family = binomial(link = "logit"), graduation)

anova(modela, modela_null, test = "LRT")
# Since we obtain a very small p-value (< 2.2e-16 ***) we reject the null hypothesis,
# rejecting t we state that at least one predictor is explanatory

# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 

confint(modela)

# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 

graduation_sibfac <- graduation |>
  mutate(nsibs = as.factor(nsibs))
str(graduation)

modelb <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
              family = binomial(link = "logit"), graduation_sibfac)
summary(modelb)
anova(modela, modelb, test = "LRT")

#The p-value obtained is above the 0.05 threshold, which suggest that the two 
# models do not differ statistically


# In the course of working this problem, you should discover an issue in the data. 
#We have huge standard errors for some of the categorical variables (ie. nsibs f14, f15,f17)
#Moreover, our reference level right now is -3 siblings 

# Deal with the issue in a reasonable manner. 

graduation_clean <- graduation|>
  filter(nsibs >= 0)

graduation_clean$nsibs_cat <- cut(
  graduation_clean$nsibs,
  breaks = c(-1, 1, 3 , 5 , 10 , 20),
  labels= c("0-1", "2-3", "4-5", "6-10", "11+")
)

table(graduation_clean$nsibs, graduation_clean$nsibs_cat)

modela2 <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
              family = binomial(link = "logit"), graduation_clean)
modelb2 <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_cat + intact, 
              family = binomial(link = "logit"), graduation_clean)
summary(modelb2)

# Does the result of the test change?

anova(modela2, modelb2, test = "LRT")
#The p-value is even bigger (0.9776), which is an even stronger suggestion
# of no improvement using a factorised variable for the number of siblings 