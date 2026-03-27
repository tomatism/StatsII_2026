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


### Trying a linear predictor 

### Analysis with covariates for women --> core 
set.seed(123)

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
saveRDS(ix.z.w.lin,file="ix_z_w.lin.rds")

ix.z.w.lin <- readRDS("ix_z_w.lin.rds")
ix.z.w.lin$diff.estimate
ix.z.w.lin$figure


## For men --> core 

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
saveRDS(ix.z.m.lin,file="ix_z_m.lin.rds")
ix.z.m.lin <- readRDS("ix_z_m.lin.rds")
ix.z.m.lin$diff.estimate
ix.z.m.lin$figure


## Plotting 
## Again, creating a plot structure, for cohesivness

## Plot helpers
plot.a <- function(..., mar = c(3.5, 3.5, 0, 0)) {
  par(mar = mar, mgp = c(3, .5, 0), las = 1, tck = -.012,
      lwd = 1, cex.axis = 1)
  plot(..., pch = "", xaxt = "n", yaxt = "n",
       ylab = "", xlab = "", frame.plot = FALSE)
}

axis.a <- function(side.set = 1, ...) {
  axis(side = side.set, lwd.ticks = 1, ...)
}

title.a <- function(mgp.new = c(2, 1, 1), ...) {
  title(mgp = mgp.new, ...)
}

## If you already have these in memory, keep them;
## otherwise they should be loaded as before:
ls.k <- lapply(list.files(pattern = ".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))

h <- 4
v.ylim <- c(-.2, 1)

pdf("Figure3_linear.pdf", family = "serif", height = h, width = h * 2)
par(mfrow = c(1, 2))

# ── Panel 1: Female respondents ───────────────────────────────────────────────

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

# ── Panel 2: Male respondents ─────────────────────────────────────────────────

dt.p <- as.data.frame(ls.k$ix.z.m.lin$est.lin[[1]])
names(dt.p) <- c("X", "ME", "sd", "CI_lower", "CI_upper")
setDT(dt.p)

## Nearest-value indices for X ≈ -1 and X ≈ 1
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

## Y-axis: use nearest to X = 1 and X = 6 for tick labels
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