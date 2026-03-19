#### DATA PREPARATION ####

# setwd("/Users/oyvindskorge/Dropbox/Papers/Giani-Hope-Skorge/7-Replication/")

#### SETUP ####

if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}

p_load(data.table,estimatr,lfe,
       tidyverse, sjlabelled, sjmisc, 
       swfscMisc, mgcv, devtools, update=FALSE) 

# inteflex version >=1.1.3 needed for the analyses,
# which can be installed by running the following line:
# devtools::install_github("xuyiqing/interflex")
p_load(interflex, install=FALSE, update=FALSE)

source("utils.r")

#### DATA ####

dt <- sjlabelled::read_data("ESSgenderUpdate1.dta")

#### RECODING OF VARIABLES ####

#### Education and household education gap  ####

dt <- dt %>% 
  mutate(Education = ifelse(eisced<8,eisced,NA))

dt <- dt %>% 
  mutate(Education_partner = ifelse(eiscedp<8,eiscedp,NA))

dt <- dt %>% 
  mutate(Edu_diff = ifelse(eisced<8,eisced,NA) - ifelse(eiscedp<8,eiscedp,NA))

#### Income ####

dt <- dt %>% 
  mutate(Income = as_numeric(Income,start.at=0))
dt <- dt %>% 
  mutate(Income_sq = Income^2)
summary(dt$Income_sq)

#### Father's education ####

dt <- dt %>% 
  mutate(eduFather = rec(eiscedf, rec="55=NA;else=copy"))

#### Mothers's education ####

dt <- dt %>% 
  mutate(eduMother = rec(eiscedm, rec="55=NA;else=copy"))

#### Difference in education ####

dt <- dt %>% 
  mutate(eduParentsDiff = remove_all_labels(eduFather) - remove_all_labels(eduMother)) %>% 
  var_labels(eduParentsDiff = "Education difference between parents") 

dt <- dt %>% 
  mutate(eduParentsDiffCat = rec(eduParentsDiff, rec="-6:-1=3; 0=2; 1:6=1",
                                 val.labels = c("Father more","Equal","Mother more"))) %>% 
  var_labels(eduParentsDiffCat = "Education difference between parents") 

#### Father's employment when respondent 14 ####

dt <- dt %>% 
  mutate(emp14Father = emprf14)

#### Mother's employment when respondent 14 ####

dt <- dt %>% 
  mutate(emp14Mother = emprm14)

#### Difference between mother and father's employment ####

# Categories
  # 1 Mother (self) employed, father not (or dead/absent)
  # 2 Both (self) employed
  # 3 Father (self) employed, mother not (or dead/absent)

dt <- dt %>% 
  mutate(emp14Diff = ifelse(emp14Mother %in% 1:2 & emp14Father %in% 3:4, 3,
                            ifelse(emp14Mother %in% 1:2 & emp14Father %in% 1:2, 2,
                                   ifelse(emp14Mother %in% 3:4 & emp14Father %in% 1:2, 1, NA)))) %>% 
  val_labels(emp14Diff = c("Father employed, mother not","Both employed","Mother employed, father not")) %>% 
  var_labels(emp14Diff = "Employment difference between parents")

#### Weights ####

dt <- dt %>% 
  mutate(wgt = dweight*pweight)

#### SAVING AMENDED DATA #####

write_stata(dt,"ESSgenderUpdate2.dta")
