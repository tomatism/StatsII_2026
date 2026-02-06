#########################
## MATILDA TOMATIS ######
## PS02 - APPLIED STATS##
#########################
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


lapply(c("tidyverse", "kableExtra"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

set.seed(123)

#Let's create the data 

data_PS1 <- rcauchy(1000, location = 0, scale = 1)

# create empirical distribution of observed data
## In the case in analysis this is a Cauchy random variables 



D_statistic <- function(data){
  
  ECDF <- ecdf(data)
  
  empiricalCDF <- ECDF(data)

  D <- max(abs(empiricalCDF - pnorm(data)))

  denominator <- 0
  
  for (i in seq_along(empiricalCDF)) {
  
  denominator <- denominator +
    exp(-((2 * i)^2) * pi^2 / (8 * D^2))
  }

  p_value <- (sqrt(2*pi) / D) * denominator 

  results_test <- if (p_value < 0.05) {
  paste("The distributions are statistically different,", p_value, "smaller than 0.05.",
        "The null hypothesis is rejected.")
  } else {
  paste("The distributions are not statistically different,", p_value, "bigger than 0.05.",
        "The null hypothesis is accepted.")
  } 

  return(list(D = D, p_value = p_value, 
              results_test = results_test))
  }

result_PS1 <- D_statistic(data_PS1)

print(result_PS1)


#####################
# Problem 2
#####################

set.seed (123)
data_PS1_2 <- data.frame(x = runif(200, 1, 10))
data_PS1_2$y <- 0 + 2.75*data_PS1_2$x + rnorm(200, 0, 1.5)


min_RSS <- function(betas, data) {
  beta0 <- betas[1]
  beta1 <- betas[2]
  y_hat <- beta0 + beta1 * data$x
  residuals <- data$y - y_hat
  sum(residuals^2)
}

estimated_betas <- optim(par = c(0,0), 
      fn = min_RSS, 
      data = data_PS1_2, 
      method = "BFGS")

coefficients_BFGS <- estimated_betas$par
  
print(c(estimated_betas$par[1], estimated_betas$par[2]))

model_lm <- lm(y ~ x, data = data_PS1_2)

summary(model_lm)  

coefficients_lm <- model_lm$coefficients

coefficients <- data.frame( lm = coefficients_lm, BFGS = coefficients_BFGS) 

coefficients |>
  kbl(
    caption = "Coefficient estimations - BFGS optimisation vs LM",
    format = "latex",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")
