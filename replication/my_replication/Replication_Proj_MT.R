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
         "binsreg", "car", "numform", "kableExtra", "stargazer"),  pkgTest)
  

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
    
    # Respondent & partner education + their gap
    Education = if_else(eisced < 8, as.numeric(eisced), NA_real_),
    Education_partner = if_else(eiscedp < 8, as.numeric(eiscedp), NA_real_),
    Edu_diff = Education - Education_partner,
    
    # Approval re-coded
    # 1 = More approval to 5 = Less approval (opposite to original)
    Approval = 5 - aftjbyc,
    
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
    # The original variable is female = 2, male = 1
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


#  Prepare counts and percentages 
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

#  Female respondents 
with(dt.p, plot.a(count, pct, xlim = c(.5,5.5), ylim = c(0, max(pct, na.rm=TRUE)),
                  mar = c(3.5, 3.5, 2, 0)))

with(dt.p.w[dt.p.w$Vignette_gender == "Woman", ],
     rect(count - x.adj, 0, count + x.adj, pct, border = NA, col = "darkgrey"))
with(dt.p.w[dt.p.w$Vignette_gender == "Man", ],
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
         as.expression(bquote("\"Man\","~italic(N) == .(dt.p.n[Female == 2 & Vignette_gender == "Man", Freq])))),
       fill = c("darkgrey", NA),
       border = c(NA, "black"))

#  Male respondents
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
         as.expression(bquote("\"Woman\","~italic(N) == .(dt.p.n[Female == 1 & Vignette_gender == "Woman", Freq]))),
         as.expression(bquote("\"Man\","~italic(N) == .(dt.p.n[Female == 1 & Vignette_gender == "Man", Freq])))),
       fill = c("darkgrey", NA),
       border = c(NA, "black"))

dev.off()

#### Figure 1b ---> Educational difference between partners (by gender of the responder)

# Prepare counts and percentages
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

# Female respondents

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

#  Male respondents 
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


####### REPLICATION ANALYSIS ####

lapply(c("tidyverse", "estimatr", "data.table","devtools", "interflex",
         "sjlabelled", "sjmisc", "swfscMisc", "mgcv"),  pkgTest)

### Preparing the two datasets for the analysis 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("ESSgenderUpdate.csv")

data <- data |>
  mutate(
    ApprovalZ = as.numeric(scale(Approval)),
    Female = sjlabelled::set_labels(
      Female,
      labels = c("Man" = 1, "Female" = 2))
  )





dt.w <- data |> 
  filter(Female==2)

dt.m <- data |>
  filter(Female==1)

### Analysis 

v.cores <- 4
v.parallel <- TRUE
v.boots <- 500

v.z <- c("Age","Age_sq","Gender_share","Child","Minority","Immigrant","Domicile",
         "Education","Income","Income_sq","Job","Unemployed","Unemployed_partner",
         "Religiosity","Interviewe_Female","Interfere")

dt.w <- dt.w |>
  select(all_of(c("ApprovalZ","Approval","Vignette_gender","Edu_diff","Country","weight",
                  unique(v.z)))) |>
  mutate(across(all_of(v.z), ~ as.numeric(arm::rescale(.))))

dt.m <- dt.m |>
  select(all_of(c("ApprovalZ","Approval","Vignette_gender","Edu_diff","Country","weight",
                  unique(v.z)))) |>
  mutate_at(v.z, arm::rescale)
set.seed(123)

### Analysis with covariates for women --> core 
ix.kern.z.w <- interflex(dt.w, 
                         Y="Approval",
                         D="Vignette_gender",
                         X="Edu_diff",
                         Z=v.z,
                         estimator="kernel",
                         FE=c("Country"),
                         parallel=v.parallel,
                         cores=v.cores,
                         nboots=v.boots,
                         neval=13,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         diff.values=c(-1,1),
                         bw = 12
)
## For men --> core 
ix.kern.z.m <- interflex(dt.m, 
                         Y="Approval",
                         D="Vignette_gender",
                         X="Edu_diff",
                         Z=v.z,
                         estimator="kernel",
                         FE=c("Country"),
                         parallel=v.parallel,
                         cores=v.cores,
                         nboots=v.boots,
                         neval=13,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         diff.values=c(-1,1),
                         bw = 12
)

saveRDS(ix.kern.z.w,file="ix_kern_z_w.rds")
ix.kern.z.w <- readRDS("ix_kern_z_w.rds")
saveRDS(ix.kern.z.m,file="ix_kern_z_m.rds")
ix.kern.z.m <- readRDS("ix_kern_z_m.rds")

ix.kern.z.w$diff.estimate
ix.kern.z.w$figure
ix.kern.z.m$diff.estimate
ix.kern.z.m$figure

## Analysis with covariates for women --> core + weights
dt.w.wgt <- dt.w |>
  select(c("Approval","Vignette_gender","Edu_diff","weight","Country",v.z)) |> 
  na.omit()

ix.kern.z.w.wgt <- interflex(dt.w.wgt, 
                             Y="Approval",
                             D="Vignette_gender",
                             X="Edu_diff",
                             Z=v.z,
                             estimator="kernel",
                             FE=c("Country"),
                             parallel=v.parallel,
                             cores=v.cores,
                             nboots=v.boots,
                             neval=13,
                             cutoffs = c(-6,-1,.9,6),
                             na.rm=TRUE,
                             wald=TRUE,
                             diff.values=c(-1,1),
                             bw = 12
)
saveRDS(ix.kern.z.w.wgt,file="ix_kern_z_w_wgt.rds")
ix.kern.z.w.wgt <- readRDS("ix_kern_z_w_wgt.rds")
ix.kern.z.w.wgt$diff.estimate
ix.kern.z.w.wgt$figure


## for men --> core + weights 
dt.m.wgt <- dt.m |>
  select(c("Approval","Vignette_gender","Edu_diff","weight","Country",v.z)) |> 
  na.omit()


ix.kern.z.m.wgt <- interflex(dt.m.wgt, 
                             Y="Approval",
                             D="Vignette_gender",
                             X="Edu_diff",
                             Z=v.z,
                             estimator="kernel",
                             FE=c("Country"),
                             parallel=v.parallel,
                             cores=v.cores,
                             nboots=v.boots,
                             neval=13,
                             cutoffs = c(-6,-1,.9,6),
                             na.rm=TRUE,
                             wald=TRUE,
                             diff.values=c(-1,1),
                             bw = 12
)
saveRDS(ix.kern.z.m.wgt,file="ix_kern_z_m_wgt.rds")
ix.kern.z.m.wgt <- readRDS("ix_kern_z_m_wgt.rds")
ix.kern.z.m.wgt$diff.estimate
ix.kern.z.m.wgt$figure


# Creating an object containing my results 
ls.k <- lapply(list.files(pattern=".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))
## Figure 2 --> Plotting the main results 
# Creating a 2 plots figure
h <- 4
pdf("Figure2.pdf", family="serif",height=h, width=h*2)
v.ylim <- c(-.2,1)
par(mfrow = c(1,2))
#Women's results
dt.p <- as.data.frame(ls.k$ix.kern.z.w.wgt$est.kernel[[1]])
setnames(
  dt.p,
  old = c("TE", "lower CI(95%)", "upper CI(95%)"),
  new = c("ME", "CI_lower", "CI_upper")
)
setDT(dt.p)
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,6,2,1)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA,
                   col=adjustcolor("black", alpha.f=.2)))
with(dt.p, lines(X,ME,lwd=2,col="black"))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

dt.p.d <- ls.k$ix.kern.z.w$diff.estimate[[1]]
v.d.pvalue <- as.numeric(dt.p.d$`p-value`)
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5))
with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label = ifelse(
            v.d.pvalue < 0.001,
            as.expression(bquote(Delta:~italic(p)<.001)),
            as.expression(
              bquote(Delta:~italic(p)==.(sprintf("%.3f", v.d.pvalue)))
            )
          ),
          adj = c(-.1,.5),
          cex = .7))
with(dt.p[abs(X) %in% c(1,6),],
     axis.a(2,
            at = c(ME, v.ylim, 0),
            labels = sprintf("%.2f", c(ME, v.ylim, 0))))
legend("bottomright",
       inset = c(0.05, 0.05),
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.w[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.z.w$bw)))),
       border=c(NA),
       cex=.5)

dt.p <- as.data.frame(ls.k$ix.kern.z.m$est.kernel[[1]])
setnames(
  dt.p,
  old = c("TE", "lower CI(95%)", "upper CI(95%)"),
  new = c("ME", "CI_lower", "CI_upper")
)
setDT(dt.p)
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,6,2,1)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA,
                   col=adjustcolor("black", alpha.f=.2)))
with(dt.p, lines(X,ME,lwd=2,col="black"))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

dt.p.d <- ls.k$ix.kern.z.m$diff.estimate[[1]]
v.d.pvalue <- as.numeric(dt.p.d$`p-value`)
with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label = ifelse(
            v.d.pvalue < 0.001,
            as.expression(bquote(Delta:~italic(p)<.001)),
            as.expression(
              bquote(Delta:~italic(p)==.(sprintf("%.3f", v.d.pvalue)))
            )
          ),
          adj = c(-.1,.5),
          cex = .7))
with(dt.p[abs(X) %in% c(1,6),],
     axis.a(2,
            at = c(ME, v.ylim, 0),
            labels = sprintf("%.2f", c(ME, v.ylim, 0))))
legend("bottomright",
       inset = c(0.05, 0.05),
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.m[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.z.m$bw)))),
       border=c(NA),
       cex=.5)
dev.off()

## Figure A2

solarizedRed <- "#DC322F"

# Creating an object containing my results 
ls.k <- lapply(list.files(pattern=".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))

h <- 4
pdf("FigureA2.pdf", family="serif", height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))


dt.p.z <- as.data.table(ls.k$ix.kern.z.w$est.kernel[[1]])      # unweighted
dt.p   <- as.data.table(ls.k$ix.kern.z.w.wgt$est.kernel[[1]])  # weighted

setnames(dt.p.z,
         c("TE","lower CI(95%)","upper CI(95%)"),
         c("ME","CI_lower","CI_upper"))
setnames(dt.p,
         c("TE","lower CI(95%)","upper CI(95%)"),
         c("ME","CI_lower","CI_upper"))

dt.p.d.z <- ls.k$ix.kern.z.w$diff.estimate[[1]]
v.d.est.z  <- as.numeric(dt.p.d.z$diff.estimate)
v.d.low.z  <- as.numeric(dt.p.d.z$`lower CI(95%)`)
v.d.high.z <- as.numeric(dt.p.d.z$`upper CI(95%)`)

dt.p.d <- ls.k$ix.kern.z.w.wgt$diff.estimate[[1]]
v.d.est  <- as.numeric(dt.p.d$diff.estimate)
v.d.low  <- as.numeric(dt.p.d$`lower CI(95%)`)
v.d.high <- as.numeric(dt.p.d$`upper CI(95%)`)

v.mean.me.z <- mean(dt.p.z$ME, na.rm=TRUE)
v.mean.me   <- mean(dt.p$ME, na.rm=TRUE)

with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey")
abline(v=0, col="darkgrey")

axis.a(2,
       at = pretty(v.ylim),
       labels = sprintf("%.2f", pretty(v.ylim)))

axis.a(4,
       at = pretty(v.ylim),
       labels = FALSE)

# Unweighted (black)
with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=adjustcolor("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))

# Weighted (red)
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=adjustcolor(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))

abline(h=v.mean.me.z, col="black", lty=2)
abline(h=v.mean.me, col=solarizedRed, lty=2)

v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))

title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

x_pos <- min(dt.p$X) + 0.05 * diff(range(dt.p$X))
y_top <- v.ylim[2]
y_gap <- 0.08 * diff(v.ylim)

legend("topright",
       bty="n", cex=.6,
       legend=c("Unweighted","Weighted"),
       col=c("black",solarizedRed),
       lty=1)

dt.p.z <- as.data.table(ls.k$ix.kern.z.m$est.kernel[[1]])
dt.p   <- as.data.table(ls.k$ix.kern.z.m.wgt$est.kernel[[1]])

setnames(dt.p.z,
         c("TE","lower CI(95%)","upper CI(95%)"),
         c("ME","CI_lower","CI_upper"))
setnames(dt.p,
         c("TE","lower CI(95%)","upper CI(95%)"),
         c("ME","CI_lower","CI_upper"))

dt.p.d.z <- ls.k$ix.kern.z.m$diff.estimate[[1]]
v.d.est.z  <- as.numeric(dt.p.d.z$diff.estimate)
v.d.low.z  <- as.numeric(dt.p.d.z$`lower CI(95%)`)
v.d.high.z <- as.numeric(dt.p.d.z$`upper CI(95%)`)

dt.p.d <- ls.k$ix.kern.z.m.wgt$diff.estimate[[1]]
v.d.est  <- as.numeric(dt.p.d$diff.estimate)
v.d.low  <- as.numeric(dt.p.d$`lower CI(95%)`)
v.d.high <- as.numeric(dt.p.d$`upper CI(95%)`)

v.mean.me.z <- mean(dt.p.z$ME, na.rm=TRUE)
v.mean.me   <- mean(dt.p$ME, na.rm=TRUE)

with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey")
abline(v=0, col="darkgrey")

with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=adjustcolor("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))

with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=adjustcolor(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))

abline(h=v.mean.me.z, col="black", lty=2)
abline(h=v.mean.me, col=solarizedRed, lty=2)

v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))

title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

x_pos <- min(dt.p$X) + 0.05 * diff(range(dt.p$X))
y_top <- v.ylim[2]
y_gap <- 0.08 * diff(v.ylim)

legend("topright",
       bty="n", cex=.6,
       legend=c("Unweighted","Weighted"),
       col=c("black",solarizedRed),
       lty=1)

dev.off()


#### MY CONTRIBUTION ###

## Females --> Linear
ix.z.w.lin <- interflex(dt.w, 
                        Y="Approval",
                        D="Vignette_gender",
                        X="Edu_diff",
                        FE=c("Country"),
                        Z=v.z,
                        estimator="linear",
                        parallel=v.parallel,
                        cores=v.cores,
                        nboots=v.boots,
                        diff.values=c(-1,1),
                        na.rm = TRUE
)

## Males --> Linear 

ix.z.m.lin <- interflex(dt.m, 
                        Y="Approval",
                        D="Vignette_gender",
                        X="Edu_diff",
                        Z=v.z,
                        estimator="linear",
                        FE=c("Country"),
                        parallel=v.parallel,
                        cores=v.cores,
                        nboots=v.boots,
                        diff.values=c(-1,1),
                        na.rm=TRUE,
)

saveRDS(ix.z.w.lin,file="ix_z_w.lin.rds")
ix.z.w.lin <- readRDS("ix_z_w.lin.rds")
ix.z.w.lin$diff.estimate
ix.z.w.lin$figure
saveRDS(ix.z.m.lin,file="ix_z_m.lin.rds")
ix.z.m.lin <- readRDS("ix_z_m.lin.rds")
ix.z.m.lin$diff.estimate
ix.z.m.lin$figure


## Plotting 

ls.k <- lapply(list.files(pattern = ".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))


pdf("Figure3_linear.pdf", family = "serif", height = h, width = h * 2)
par(mfrow = c(1, 2))

# Female respondents 

dt.p <- as.data.frame(ls.k$ix.z.w.lin$est.lin[[1]])
names(dt.p) <- c("X", "ME", "sd", "CI_lower", "CI_upper")
setDT(dt.p)

## Nearest-value indices for X ≈ -1 and X ≈ 1
idx.w <- c(which.min(abs(dt.p$X - (-1))), which.min(abs(dt.p$X - 1)))

with(dt.p, plot.a(X, ME,
                  ylim = v.ylim,
                  mar = c(3.5, 6, 2, 1)))
abline(h = 0, col = "darkgrey", lty = "solid")
abline(v = 0, col = "darkgrey", lty = "solid")

with(dt.p, polygon(c(X, rev(X)),
                   c(CI_upper, rev(CI_lower)),
                   border = NA,
                   col = adjustcolor("black", alpha.f = .2)))
with(dt.p, lines(X, ME, lwd = 2, col = "black"))

v.x <- with(dt.p, seq(range(X)[1], range(X)[2], 1))
with(dt.p, axis.a(at = v.x,
                  labels = ifelse(v.x %% 2 == 0, v.x, "")))

title.a(main = "Female respondents (linear)", font.main = 3,
        ylab = "Marg. childcare bias (95% CIs)",
        xlab = expression(paste(Delta, "Education")))

## p-value for the difference at X ≈ -1 vs X ≈ 1
dt.p.d <- ls.k$ix.z.w.lin$diff.estimate[[1]]
v.d.pvalue <- as.numeric(dt.p.d$`p-value`)

with(dt.p[idx.w], points(X, ME, pch = 16, cex = .5))
with(dt.p[idx.w], segments(X, ME, 2, ME, lty = "dotted"))

braces(xfrom = 2.2, xto = 2.5,
       yfrom = dt.p$ME[which.min(abs(dt.p$X - (-1)))],
       yto   = dt.p$ME[which.min(abs(dt.p$X - 1))],
       radius = 1)

with(dt.p[idx.w],
     text(2.5, mean(ME),
          label = ifelse(
            v.d.pvalue < 0.001,
            as.expression(bquote(Delta:~italic(p) < .001)),
            as.expression(
              bquote(Delta:~italic(p) == .(sprintf("%.3f", v.d.pvalue)))
            )
          ),
          adj = c(-.1, .5),
          cex = .7))

## Y-axis: use nearest to X = 1 and X = 6 for tick labels
idx.w.ax <- c(which.min(abs(dt.p$X - 1)), which.min(abs(dt.p$X - 6)))
with(dt.p[idx.w.ax],
     axis.a(2,
            at = c(ME, v.ylim, 0),
            labels = sprintf("%.2f", c(ME, v.ylim, 0))))

legend("bottomright",
       inset = c(0.05, 0.05),
       bty = "n",
       legend = c(as.expression(
         bquote(italic(N) == .(ls.k$ix.z.w.lin[["de"]]$n))
       )),
       border = c(NA),
       cex = .5)

# Male respondents 

dt.p <- as.data.frame(ls.k$ix.z.m.lin$est.lin[[1]])
names(dt.p) <- c("X", "ME", "sd", "CI_lower", "CI_upper")
setDT(dt.p)

idx.m <- c(which.min(abs(dt.p$X - (-1))), which.min(abs(dt.p$X - 1)))

with(dt.p, plot.a(X, ME,
                  ylim = v.ylim,
                  mar = c(3.5, 6, 2, 1)))
abline(h = 0, col = "darkgrey", lty = "solid")
abline(v = 0, col = "darkgrey", lty = "solid")

with(dt.p, polygon(c(X, rev(X)),
                   c(CI_upper, rev(CI_lower)),
                   border = NA,
                   col = adjustcolor("black", alpha.f = .2)))
with(dt.p, lines(X, ME, lwd = 2, col = "black"))

v.x <- with(dt.p, seq(range(X)[1], range(X)[2], 1))
with(dt.p, axis.a(at = v.x,
                  labels = ifelse(v.x %% 2 == 0, v.x, "")))

title.a(main = "Male respondents (linear)", font.main = 3,
        ylab = "Cond. marginal treatment effect",
        xlab = expression(paste(Delta, "Education")))

dt.p.d <- ls.k$ix.z.m.lin$diff.estimate[[1]]
v.d.pvalue <- as.numeric(dt.p.d$`p-value`)

with(dt.p[idx.m], points(X, ME, pch = 16, cex = .5))
with(dt.p[idx.m], segments(X, ME, 2, ME, lty = "dotted"))

braces(xfrom = 2.2, xto = 2.5,
       yfrom = dt.p$ME[which.min(abs(dt.p$X - (-1)))],
       yto   = dt.p$ME[which.min(abs(dt.p$X - 1))],
       radius = 1)

with(dt.p[idx.m],
     text(2.5, mean(ME),
          label = ifelse(
            v.d.pvalue < 0.001,
            as.expression(bquote(Delta:~italic(p) < .001)),
            as.expression(
              bquote(Delta:~italic(p) == .(sprintf("%.3f", v.d.pvalue)))
            )
          ),
          adj = c(-.1, .5),
          cex = .7))

idx.m.ax <- c(which.min(abs(dt.p$X - 1)), which.min(abs(dt.p$X - 6)))
with(dt.p[idx.m.ax],
     axis.a(2,
            at = c(ME, v.ylim, 0),
            labels = sprintf("%.2f", c(ME, v.ylim, 0))))

legend("bottomright",
       inset = c(0.05, 0.05),
       bty = "n",
       legend = c(as.expression(
         bquote(italic(N) == .(ls.k$ix.z.m.lin[["de"]]$n))
       )),
       border = c(NA),
       cex = .5)

dev.off()

## The beta_3 coefficents 


library(stargazer)

stargazer(
  ix.z.w.lin$model.linear,
  ix.z.m.lin$model.linear,
  title    = "Linear Interflex Models - Core Explanatory Variables",
  column.labels = c("Women (Linear)", "Men (Linear)"),
  keep          = c("Edu_diff", "D.Group.2", "DX.Group.2"),
  covariate.labels = c(
    "Educational Gap ($\\beta_1$)",
    "Vignette Gender ($\\beta_2$)",
    "Edu. Gap $\\times$ Vignette Gender ($\\beta_3$)",
    type  = "latex",
  label = "tab:comparison"
))

