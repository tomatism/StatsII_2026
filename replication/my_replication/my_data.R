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
         "ISCO08ConveRsions", "purrr", "data.table","estimatr",
         "lfe","sjmisc", "swfscMisc", "mgcv", "devtools", 
         "binsreg", "car", "numform", "kableExtra"),  pkgTest)
  

## Data manipulation 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ess <- read_dta("ESSround9.dta")

ess_clean <- ess |>
  mutate(
    # Convert ISCO 
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
    
    # Replace missing gender indicators (for family members) with 0
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
    
    Interfere = prewhp,
    Female = gndr,
    Immigrant = brncntr,
    Vignette_gender = 2 - admge,
    Age = agea,
    Domicile = domicil,
    
    Income = as.numeric(hinctnta),      # deciles 1–10
    Income = if_else(haven::na_tag(hinctnta) %in% c("b","c","d"), NA_real_, Income),
    Income_sq = Income^2,
    
    Maritalstatus = rshpsts,
    
    Child = as.numeric(nbthcld),
    
    Minority = blgetmg,
    Interviewe_Female = intgndr - 1,
    
    # Approval = Independent variable
    # 1 = More approval to 5 = Less approval (opposite to original)
    Approval = 5 - aftjbyc,
    Job_diff = Job - Job_partner,
    
    Unemployed = uempla,
    Unemployed_partner = uemplap,
    Housework_partner = hswrkp,
    Employment_partner = emprelp,
    
    Trust = ppltrst,
    Religiosity = rlgdgr,
    Age_sq = Age^2,
    Country = cntry,
    
    # Weight (design and post-stratification)
    weight = dweight * pweight,
    
    # What scenario were they interview about?
    Vignette_gender = 2 - admge
    
  ) |>
  filter(!is.na(Education), !is.na(Education_partner)) |>
  mutate(
    Edu_diff = Education - Education_partner
  )|>
  select(

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
    
    # Core experimental variable
    Approval, 
    
    # Demographics
    Age, Age_sq, Domicile, Maritalstatus, Child, Minority, Country,
    Interfere, Female, Immigrant,
    
    # Income
    Income, Income_sq,
    
    # Interview characteristics
    Interviewe_Female,
    
    # Job characteristics
    Job_diff, Job, Job_partner,
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


### Data description / visuals 

dt_plot <- read.csv("ESSgenderUpdate.csv")

dt_plot <- dt_plot |> 
  filter(!is.na(Edu_diff) & !is.na(Approval))

dt_plot$Vignette_gender <- factor(
  dt_plot$Vignette_gender,
  levels = c("0","1"),    
  labels = c("Man","Woman")  
)

## Setting the standard for plots ##

plot.a <- function(...,mar=c(3.5,3.5,0,0)) {
  par(mar=mar, mgp=c(3,.5,0), las=1, tck=-.012,
      lwd=1, cex.axis=1) 
  plot(..., pch="",xaxt="n",yaxt="n",
       ylab="",xlab="",frame.plot=FALSE)
}

axis.a <- function(side.set=1, ...) {
  axis(side=side.set, lwd.ticks=1, ...)
}

title.a <- function(mgp.new=c(2,1,1), ...) {
  title(mgp=mgp.new, ...)
}

##  Figure 1a ---> Approval for man and female (by gender of the responder )

dt.w.plot <- dt_plot |> filter(Female == 2)
dt.m.plot <- dt_plot |> filter(Female == 1)


# --- Prepare counts and percentages ---
dt.p.n <- dt_plot |>
  flat_table(Female, Vignette_gender) |>
  as.data.table()

dt.p <- dt_plot |>
  flat_table(Female, Vignette_gender, Approval, margin = "row") |>
  as.data.table() |>
  mutate(
    count = as.numeric(Approval),
    pct = Freq,
    groups = paste0(Female, Vignette_gender),
    labs = ifelse(grepl(" ", Approval),
                  gsub(" ", "\n", Approval),
                  paste0(Approval, "\n"))
  )

setDT(dt.p)

dt.p.w <- dt.p[Female == 2]
dt.p.m <- dt.p[Female == 1]

h <- 6
pdf("Figure1a.pdf", family="serif", height = h, width = h*2)
par(mfrow = c(1,2), oma = c(3, 0, 0, 0)) 
x.adj <- 0.4

# --- Female respondents ---
with(dt.p, plot.a(count, pct, xlim = c(.5,5.5), ylim = c(0, max(pct, na.rm=TRUE)),
                  mar = c(3.5, 3.5, 2, 0)))

with(dt.p.w[dt.p.w$Vignette_gender == "Man", ],
     rect(count - x.adj, 0, count + x.adj, pct, border = NA, col = "darkgrey"))
with(dt.p.w[dt.p.w$Vignette_gender == "Woman", ],
     rect(count - x.adj, 0, count + x.adj, pct, lwd = 2))

abline(h = 0, lwd = 2.5, col = "white")

with(dt.p.w[dt.p.w$Vignette_gender == "Woman", ],
     axis.a(at = as.numeric(Approval), labels = labs,
            pos = -0.5, padj = 0.2, lwd = 1, cex.axis = 0.7))

axis.a(2, hadj = 1)
title.a(main = "Female respondents", font.main = 3,
        ylab = "Percentage of respondents",
        xlab = "Approval of: \n\"[Woman/man] has full-time job while [she/he]\nhas children aged under 3\"")

legend(3.5, par("usr")[4],
       bty = "n",
       title = expression(italic(Assignment)),
       title.adj = 0.1,
       legend = c(
         as.expression(bquote("\"Woman\","~italic(N) == .(dt.p.n[Female == 2 & Vignette_gender == "Woman", Freq]))),
         as.expression(bquote("\"Man\","~italic(N) == .(dt.p.n[Female == 1 & Vignette_gender == "Man", Freq])))),
       fill = c("darkgrey", NA),
       border = c(NA, "black"))

# --- Male respondents ---
with(dt.p, plot.a(count, pct, xlim = c(.5,5.5), ylim = c(0, max(pct, na.rm=TRUE)),
                  mar = c(3.5, 3.5, 2, 0)))

with(dt.p.m[dt.p.m$Vignette_gender == "Woman", ],
     rect(count - x.adj, 0, count + x.adj, pct, border = NA, col = "darkgrey"))
with(dt.p.m[dt.p.m$Vignette_gender == "Man", ],
     rect(count - x.adj, 0, count + x.adj, pct, lwd = 2))

abline(h = 0, lwd = 2.9, col = "white")

with(dt.p.m[dt.p.m$Vignette_gender == "Woman", ],
     axis.a(at = as.numeric(Approval), labels = labs,
            pos = -0.5, padj = 0.2, lwd = 1, cex.axis = 0.7))

axis.a(2, hadj = 1)
title.a(main = "Male respondents", font.main = 3,
        ylab = "",
        xlab = "Approval of: \n\"[Woman/man] has full-time job while [she/he]\nhas children aged under 3\"")

legend(3.5, par("usr")[4],
       bty = "n",
       title = expression(italic(Assignment)),
       title.adj = 0.1,
       legend = c(
         as.expression(bquote("\"Woman\","~italic(N) == .(dt.p.n[Female == 2 & Vignette_gender == "Woman", Freq]))),
         as.expression(bquote("\"Man\","~italic(N) == .(dt.p.n[Female == 1 & Vignette_gender == "Man", Freq])))),
       fill = c("darkgrey", NA),
       border = c(NA, "black"))
mtext("Note: Approval ranges from Approval (1) to Disapproval (5).",
      side = 1, line = 1, outer = TRUE, cex = 0.8)

dev.off()

#### Figure 1b ---> Educational difference between partners (by gender of the responder)

# ---- Prepare counts and percentages ----
dt.p <- dt_plot |>
  flat_table(Female, Vignette_gender, Edu_diff, margin = "row") |> 
  as.data.table() |>
  mutate(
    count = as.numeric(as.character(Edu_diff)),
    pct   = Freq,                               
    groups = paste0(Female, Vignette_gender)
  )

setDT(dt.p)
dt.p.w <- dt.p[Female == 2]
dt.p.m <- dt.p[Female == 1]

h <- 6
pdf("Figure1b.pdf", family = "serif", height = h, width = h*2)
par(mfrow = c(1,2), oma = c(3,0,0,0))  
x.adj <- 0.4

# --- Female respondents ---

with(dt.p, plot.a(count, pct,
                  ylim = range(0, pct),
                  mar = c(3.5, 3.5, 2, 0)))

with(dt.p.w[Vignette_gender == "Woman"],
     rect(count - x.adj, 0, count + x.adj, pct,
          border = NA, col = "darkgrey"))

with(dt.p.w[Vignette_gender == "Man"],
     rect(count - x.adj, 0, count + x.adj, pct,
          lwd = 2))

abline(h = 0, lwd = 2.5, col = "white")

with(dt.p.w[Vignette_gender == "Man"],
     axis.a(at = count, pos = -.5, padj = .2,
            lwd = 1, cex.axis = 1))

axis.a(2, hadj = 1)

title.a(
  main = "Female respondents",
  font.main = 3,
  ylab = "Percentage of respondents",
  xlab = expression(
    paste(Delta, "Education = ",
          Education[respondent], " − ",
          Education[partner])
  )
)

legend("topleft",
       bty = "n",
       title = expression(italic(Assignment)),
       title.adj = 0.3,
       legend = c("Woman", "Man"),
       fill = c("darkgrey", NA),
       border = c(NA, "black"))

# --- Male respondents ---
with(dt.p, plot.a(count, pct,
                  ylim = range(0, pct),
                  mar = c(3.5, 3.5, 2, 0)))

with(dt.p.m[Vignette_gender == "Woman"],
     rect(count - x.adj, 0, count + x.adj, pct,
          border = NA, col = "darkgrey"))

with(dt.p.m[Vignette_gender == "Man"],
     rect(count - x.adj, 0, count + x.adj, pct,
          lwd = 2))

abline(h = 0, lwd = 2.9, col = "white")

with(dt.p.m[Vignette_gender == "Man"],
     axis.a(at = count, pos = -.5, padj = .2,
            lwd = 1, cex.axis = 1))

axis.a(2, hadj = 1)

title.a(
  main = "Male respondents",
  font.main = 3,
  ylab = "",
  xlab = expression(
    paste(Delta, "Education = ",
          Education[respondent], " − ",
          Education[partner])
  )
)

mtext(
  "Note: Positive values indicate respondents are more educated than their partner.",
  side = 1, line = 1, outer = TRUE, cex = 0.8
)

dev.off()