#########################################
# MATILDA TOMATIS - REPLICATION PROJECT #
#########################################


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

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("haven", "tidyverse", "stringr", "lubridate",
         "ISCO08ConveRsions", "purrr"),  pkgTest)

# Read data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ess <- read_dta("ESSround9.dta")

# Clean and recode

ess_clean <- ess |>
  mutate(
    # Convert ISCO safely
    Job = map_dbl(isco08, ~ {
      if (is.na(.x)) return(NA_real_)
      tryCatch(
        isco08tosiops08(sprintf("%04d", as.integer(.x))),
        error = function(e) NA_real_
      )
    }),
    
    Job_partner = map_dbl(isco08p, ~ {
      if (is.na(.x)) return(NA_real_)
      tryCatch(
        isco08tosiops08(sprintf("%04d", as.integer(.x))),
        error = function(e) NA_real_
      )
    }),
 
    
    # Rename household size
    Household_size = hhmmb,
    
    # Parents' education
    Dad_Edu = as.numeric(eiscedf),
    Mum_Edu = as.numeric(eiscedm),
    Dad_Edu = if_else(Dad_Edu == 55, NA_real_, Dad_Edu),
    Mum_Edu = if_else(Mum_Edu == 55, NA_real_, Mum_Edu),
    Parents_gap = Dad_Edu - Mum_Edu,
    Parents_gap_cat = case_when(
      Parents_gap < 0 ~ "Mother more",
      Parents_gap == 0 ~ "Equal",
      Parents_gap > 0 ~ "Father more",
      TRUE ~ NA_character_
    ),
    
    # Parent employment at age 14
    emp14Father = emprf14,
    emp14Mother = emprm14,
    emp14Diff = case_when(
      emp14Mother %in% 1:2 & emp14Father %in% 3:4 ~ 3,
      emp14Mother %in% 1:2 & emp14Father %in% 1:2 ~ 2,
      emp14Mother %in% 3:4 & emp14Father %in% 1:2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Respondent & partner education 
    Education = if_else(eisced < 8, as.numeric(eisced), NA_real_),
    Education_partner = if_else(eiscedp < 8, as.numeric(eiscedp), NA_real_),
    Edu_diff = Education - Education_partner,
    
    # Replace missing gender indicators with 0
    gndr2  = replace_na(as.numeric(gndr2), 0),
    gndr3  = replace_na(as.numeric(gndr3), 0),
    gndr4  = replace_na(as.numeric(gndr4), 0),
    gndr5  = replace_na(as.numeric(gndr5), 0),
    gndr6  = replace_na(as.numeric(gndr6), 0),
    gndr7  = replace_na(as.numeric(gndr7), 0),
    gndr8  = replace_na(as.numeric(gndr8), 0),
    gndr9  = replace_na(as.numeric(gndr9), 0),
    gndr10 = replace_na(as.numeric(gndr10), 0),
    gndr11 = replace_na(as.numeric(gndr11), 0),
    gndr12 = replace_na(as.numeric(gndr12), 0),
    gndr13 = replace_na(as.numeric(gndr13), 0),
    gndr14 = replace_na(as.numeric(gndr14), 0),
    gndr15 = replace_na(as.numeric(gndr15), 0),
    
    # Share of female at home
    Gender_share = (gndr2 + gndr3 + gndr4 + gndr5 + gndr6 + gndr7 +
                      gndr8 + gndr9 + gndr10 + gndr11 + gndr12 +
                      gndr13 + gndr14 + gndr15) / (2 * Household_size),
    
    # Core variables
    Interfere = prewhp,
    Female = gndr,
    Immigrant = brncntr,
    Vignette_gender = 2 - admge,
    Age = agea,
    Domicile = domicil,
    
    # Income: treat tagged missing .b as 0 (refusal)
    Income = as.numeric(hinctnta),      # deciles 1–10
    Income = if_else(haven::na_tag(hinctnta) %in% c("b","c","d"), NA_real_, Income),
    Income_sq = Income^2,
    
    Maritalstatus = rshpsts,
    
    # Children: treat tagged missing .a as 0
    Child = as.numeric(nbthcld),
    Child = if_else(haven::na_tag(nbthcld) == "a", 0, Child),
    
    Minority = blgetmg,
    Interviewe_Female = intgndr - 1,
    Approval = 5 - aftjbyc,
    Job_diff = Job - Job_partner,
    
    Unemployed = uempla,
    Unemployed_partner = uemplap,
    Housework_partner = hswrkp,
    Employment_partner = emprelp,
    
    Trust = ppltrst,
    Religiosity = rlgdgr,
    Age_sq = Age^2,
    
    # Weight (design and post-stratification)
    weight = dweight * pweight,
    
    # What scenario were they interview about?
    Vignette_gender = factor(Vignette_gender, levels = c(0, 1), 
                           labels = c("Man", "Woman"),
  )) |>
  select(
    # Occupation
    Job, Job_partner,
    
    # Household structure
    Household_size,
    
    # Respondent & partner education 
    Education, Education_partner, Edu_diff,
    
    # Parents' education
    Dad_Edu, Mum_Edu, Parents_gap, Parents_gap_cat,
    
    # Childhood parental employment 
    emp14Father, emp14Mother, emp14Diff,
    
    # Household gender composition
    gndr2:gndr15, Gender_share,
    
    # Core experimental variables
    Approval, Interfere, Female, Immigrant,
    
    # Demographics
    Age, Age_sq, Domicile, Maritalstatus, Child, Minority,
    
    # Income
    Income, Income_sq,
    
    # Interview characteristics
    Interviewe_Female,
    
    # Job characteristics
    Job_diff,
    Unemployed, Unemployed_partner,
    Housework_partner, Employment_partner,
    
    # Attitudes
    Trust, Religiosity,
    
    # ESS weight
    weight,

    # Vignette gender 
    Vignette_gender
  ) 

# Save cleaned file
write.csv(ess_clean, "ESSgenderUpdate.csv")
