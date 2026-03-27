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

### Analysis with covariates for women --> core 
set.seed(123)

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
saveRDS(ix.kern.z.w,file="ix_kern_z_w.rds")

ix.kern.z.w <- readRDS("ix_kern_z_w.rds")
ix.kern.z.w$diff.estimate
ix.kern.z.w$figure


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
saveRDS(ix.kern.z.m,file="ix_kern_z_m.rds")
ix.kern.z.m <- readRDS("ix_kern_z_m.rds")
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


## Again, creating a plot structure, for cohesivness

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

# Creating an object containing my results 
ls.k <- lapply(list.files(pattern=".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))

## Figure 2 --> Plotting the main results 
# Creating a 2 plots figure
h <- 4
pdf("Figure2.pdf", family="serif",height=h, width=h*2)
v.ylim <- c(-.2,1)
par(mfrow = c(1,2))


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

