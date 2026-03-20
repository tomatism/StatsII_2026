#######################
# Tutorial 9: Poisson #
#######################

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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)


# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three years of their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’s PhD department (phd); 
# - number of articles published by the student’s mentor during the three-year period (ment)

# Make sure your data are in the correct format.

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?
# Do we meet the OLS assumptions?

mod_lm <- lm(art ~ ., data = long_data)
summary(mod_lm)
#We can make a prediction, however in certain scenarios we might obtaina a 
#negative count pf the articles, which wouldn't make sense



# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# Do we meet assumptions for Poisson?
# What conclusions would you draw from this analysis (i.e. interpret your estimated coefficients)?

pois_mod <- glm(art ~ ., data = long_data, family = poisson(link = "log"))
#the link = "log" would be the default option
summary(pois_mod)

#Check the assumptions 

mean(long_data$art)
var(long_data$art)

#plotting
coeffs <- coefficients(pois_mod)
xvalues <- sort(long_data$ment)
means <- exp(coeffs[1] + coeffs[3]*xvalues)
windows()
plot(long_data$ment, long_data$art)
lines(xvalues, means, lty=2, col = "red")

# What is the predicted number of articles for a married male PhD researcher with 1 child at 2-rated institute whose PhD supervisor published 5 articles?
multiplicative <- exp(coeffs)
multiplicative

new_data <- data.frame(
  fem = 0,
  mar = 1,
  kid5 = 1,
  phd = 2,
  ment = 5
)

predict(pois_mod, new_data, type = "response")


# Plot predictions vs count.

pred <- predict(pois_mod, type = "response")
plot(long_data$art, pred)
abline(0, 1, col = "red")
#In a perfect model all the point would be on the read line, but we know 
# that our model has some problems 

# Calculate pseudo R squared.
1 - pois_mod$deviance / pois_mod$null.deviance
#how much better is my model than a model with no predictors 

# Calculate RMSE.

rmse <- sqrt(mean(long_data$art - pred)^2)
rmse 

# Should we add an interaction for gender with our covariates?


# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account?

library(AER)
dispersiontest(pois_mod)

