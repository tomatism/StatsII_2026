##################
#### GRAPHS ####
##################

# setwd("/Users/oyvindskorge/Dropbox/Papers/Giani-Hope-Skorge/7-Replication/")

#### SETUP ####

if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}

p_load(data.table,estimatr,lfe,
       tidyverse, sjlabelled, sjmisc, 
       swfscMisc, mgcv, devtools, 
       binsreg, car, numform,
       knitr, kableExtra, 
       update=FALSE) 

# inteflex version >=1.1.3 needed for the analyses,
# which can be installed by running the following line:
# devtools::install_github("xuyiqing/interflex")
p_load(interflex, install=FALSE, update=FALSE)

source("utils.r")

solarizedRed <- "#DC322F"
solarizedBlue <- "#268BD2"

#### DATA ####

dt <- sjlabelled::read_data("ESSgenderUpdate2.dta")

dt <- dt %>% 
  filter(!is.na(Edu_diff) & !is.na(Bad_job))

dt <- dt %>% 
  set_labels(Female, labels = c("Men", "Women"))

dt.w <- dt %>% 
  filter(Female==2)

dt.m <- dt %>% 
  filter(Female==1)

#### DESCRIPTIVES IN TEXT AND FIGURES ####

#### Figure 1a: distribution of dv ####

dt.p.n <- dt %>% 
  flat_table(Female,Treatment) %>% 
  as.data.table()

dt.p <- dt %>% 
  flat_table(Female,Treatment,Bad_job,margin="row") %>% 
  as.data.table() %>% 
  mutate(count = as.numeric(Bad_job)) %>% 
  mutate(pct = Freq) %>% 
  mutate(groups = paste0(Female,Treatment)) %>% 
  mutate(labs = ifelse(grepl(" ",Bad_job), gsub(" ","\n", Bad_job), paste0(Bad_job, "\n")))

setDT(dt.p)
dt.p.w <- dt.p[Female=="Women"]
dt.p.m <- dt.p[Female=="Men"]

h <- 6
pdf("Figure1a.pdf", family="serif",height=h, width=h*2)
par(mfrow=c(1,2))
x.adj <- .4
with(dt.p, plot.a(count,pct, xlim=c(.5,5.5), ylim=range(0,pct),
                  mar=c(3.5, 3.5, 2, 0)))
with(dt.p.w[Treatment=="Treatment"], rect(count-x.adj, 0,
                                          count+x.adj, pct,
                                          border=NA,
                                          col="darkgrey"))
with(dt.p.w[Treatment=="Control"], rect(count-x.adj, 0,
                                        count+x.adj, pct,
                                        lwd=2))
abline(h=0,lwd=2.5, col="white")
with(dt.p.w[Treatment=="Control"], axis.a(at=as.numeric(Bad_job), labels=labs, 
                                          pos=-.5, padj=.2, lwd=1, cex.axis=.7))
axis.a(2, hadj=1)
title.a(main="Female respondents", font.main=3,
        ylab="Percentage of respondents",
        xlab="\"[Woman/man] has full-time job while [she/he]\nhas children aged under 3\"")
legend(3.5,par("usr")[4],
       bty="n", #cex=.8,
       title=expression(italic(Assignment)),
       title.adj=0.1,
       legend=c(
         as.expression(bquote("\"Woman\","~italic(N)==.(dt.p.n[Female=="Women" & Treatment=="Control",Freq]))),
         as.expression(bquote("\"Man\","~italic(N)==.(dt.p.n[Female=="Women" & Treatment=="Treatment",Freq])))),
       fill=c("darkgrey",NA),
       border=c(NA,"black"))

x.adj <- .4
with(dt.p, plot.a(count,pct, xlim=c(.5,5.5), ylim=range(0,pct),
                  mar=c(3.5, 3.5, 2, 0)))
with(dt.p.m[Treatment=="Treatment"], rect(count-x.adj, 0,
                                          count+x.adj, pct,
                                          border=NA,
                                          col="darkgrey"))
with(dt.p.m[Treatment=="Control"], rect(count-x.adj, 0,
                                        count+x.adj, pct,
                                        lwd=2))
abline(h=0,lwd=2.9, col="white")
with(dt.p.m[Treatment=="Control"], axis.a(at=as.numeric(Bad_job), labels=labs, 
                                          pos=-.5, padj=.2, lwd=1, cex.axis=.7))
axis.a(2, hadj=1)
title.a(main="Male respondents", font.main=3,
        ylab="",
        xlab="\"[Woman/man] has full-time job while [she/he]\nhas children aged under 3\"")
legend(3.5,par("usr")[4],
       bty="n", #cex=.8,
       title=expression(italic(Assignment)),
       title.adj=0.1,
       legend=c(
         as.expression(bquote("\"Woman\","~italic(N)==.(dt.p.n[Female=="Men" & Treatment=="Control",Freq]))),
         as.expression(bquote("\"Man\","~italic(N)==.(dt.p.n[Female=="Men" & Treatment=="Treatment",Freq])))),
       fill=c("darkgrey",NA),
       border=c(NA,"black"))
dev.off()

#### Footnotes 2 and 3: no gender diff in treatment ####

difference_in_means(Bad_job~Treatment, data=dt, subset=Female==2)
lm_robust(Bad_job~Treatment, 
          data=dt, 
          subset=Female==2,
          fixed_effects = ~country)

difference_in_means(Bad_job~Treatment, data=dt, subset=Female==1)
lm_robust(Bad_job~Treatment, 
          data=dt, 
          subset=Female==1,
          fixed_effects = ~country)

summary(lm_robust(Bad_job ~ Treatment*Female, 
                  data=dt))

#### Figure 1b: distribution of moderator ####

dt.p <- dt %>% 
  flat_table(Female,Treatment,Edu_diff,margin="row") %>% 
  as.data.table() %>% 
  mutate(count = as.numeric(as.character(Edu_diff))) %>% 
  mutate(pct = Freq) %>% 
  mutate(groups = paste0(Female,Treatment))

setDT(dt.p)
dt.p.w <- dt.p[Female=="Women"]
dt.p.m <- dt.p[Female=="Men"]

h <- 6
pdf("Figure1b.pdf", family="serif",height=h, width=h*2)
par(mfrow=c(1,2))
x.adj <- .4
with(dt.p, plot.a(count, pct, ylim=range(0,pct),
                  mar=c(3.5, 3.5, 2, 0)))
with(dt.p.w[Treatment=="Treatment"], rect(count-x.adj, 0,
                                          count+x.adj, pct,
                                          border=NA,
                                          col="darkgrey"))
with(dt.p.w[Treatment=="Control"], rect(count-x.adj, 0,
                                        count+x.adj, pct,
                                        lwd=2))
abline(h=0,lwd=2.5, col="white")
with(dt.p.w[Treatment=="Control"], axis.a(at=count, 
                                          pos=-.5, padj=.2, lwd=1, cex.axis=1))
axis.a(2, hadj=1)
title.a(main="Female respondents", font.main=3,
        ylab="Percentage of respondents",
        xlab=expression(paste(Delta,'Education = ','Education'[respondent] - 'education'[partner])))
legend("topright",
       bty="n", #cex=.8,
       title=expression(italic(Assignment)),
       title.adj=0.3,
       legend=c("\"Woman\"","\"Man\""),
       fill=c("darkgrey",NA),
       border=c(NA,"black"))

x.adj <- .4
with(dt.p, plot.a(count,pct, ylim=range(0,pct),
                  mar=c(3.5, 3.5, 2, 0)))
with(dt.p.m[Treatment=="Treatment"], rect(count-x.adj, 0,
                                          count+x.adj, pct,
                                          border=NA,
                                          col="darkgrey"))
with(dt.p.m[Treatment=="Control"], rect(count-x.adj, 0,
                                        count+x.adj, pct,
                                        lwd=2))
abline(h=0,lwd=2.9, col="white")
with(dt.p.m[Treatment=="Control"], axis.a(at=count, 
                                          pos=-.5, padj=.2, lwd=1, cex.axis=1))
axis.a(2, hadj=1)
title.a(main="Male respondents", font.main=3,
        ylab=" ",
        xlab=expression(paste(Delta,'Education = ','Education'[respondent] - 'education'[partner])))
dev.off()

#### Numbers for text ####

# "Many couples also have either the same (46%) or one-point different (24%) levels of education.

dt %>% 
  flat_table(abs(Edu_diff), margin="row")


#### PLOTS OF MAIN ANALYSIS ####

ls.k <- lapply(list.files(pattern=".rds"), readRDS)
names(ls.k) <- gsub(".rds","",gsub("_",".",list.files(pattern=".rds")))

#### Figure 2: main kernel interaction model ####

h <- 4
# pdf(here("figures","histMV.pdf"), family="serif",height=h, width=h*2)
pdf("Figure2.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p <- ls.k$ix.kern.z.w$est[[1]]
setDT(dt.p)
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans("black",.2)))
with(dt.p, lines(X,ME,lwd=2,col="black"))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

dt.p.d <- ls.k$ix.kern.z.w$t.test.diffs[[1]]
v.d.pvalue <- ls.k$ix.kern.z.w$t.test.diffs[[1]][4][[1]]

with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5))
with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue<0.001,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(2, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.w[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.z.w$bw)))),
       border=c(NA),
       cex=.5)

dt.p <- ls.k$ix.kern.z.m$est[[1]]
setDT(dt.p)
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans("black",.2)))
with(dt.p, lines(X,ME,lwd=2,col="black"))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

dt.p.d <- ls.k$ix.kern.z.w$t.test.diffs[[1]]
v.d.pvalue <- ls.k$ix.kern.z.m$t.test.diffs[[1]][4][[1]]

with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5))
with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(4, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.m[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.z.m$bw)))),
       border=c(NA),
       cex=.5)
dev.off()

#### Figure A.1: without covariates ####

h <- 4
pdf("FigureA1.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p.z <- ls.k$ix.kern.z.w$est[[1]]
dt.p <- ls.k$ix.kern.x.w$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.x.w$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.x.w$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(2, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.x.w[["de"]]$n)*";"~"bandwidth = "*.(f_num(ls.k$ix.kern.x.w$bw,1))))),
       border=c(NA),
       cex=.5)
legend("topright",
       bty="n", cex=.5,
       legend=c("Main kernel (Fig. 2)",
                "Without covariates"),
       fill=c(NA,NA),
       col=c("black",solarizedRed),
       lty=c("solid","solid"),
       border=c(NA,NA))

dt.p.z <- ls.k$ix.kern.z.m$est[[1]]
dt.p <- ls.k$ix.kern.x.m$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.x.m$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.x.m$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(4, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.x.m[["de"]]$n)*";"~"bandwidth = "*.(f_num(ls.k$ix.kern.x.m$bw,1))))),
       border=c(NA),
       cex=.5)
dev.off()



#### Figure A.2: with survey weights ####

h <- 4
pdf("FigureA2.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p.z <- ls.k$ix.kern.z.w$est[[1]]
dt.p <- ls.k$ix.kern.z.w.wgt$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.z.w.wgt$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.z.w.wgt$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue<0.001,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(2, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.w.wgt[["de"]]$n)*";"~"bandwidth = "*.(f_num(ls.k$ix.kern.z.w.wgt$bw,1))))),
       border=c(NA),
       cex=.5)
legend("topright",
       bty="n", cex=.5,
       legend=c("Main kernel (Fig. 2)",
                "With survey weights"),
       fill=c(NA,NA),
       col=c("black",solarizedRed),
       lty=c("solid","solid"),
       border=c(NA,NA))

dt.p.z <- ls.k$ix.kern.z.m$est[[1]]
dt.p <- ls.k$ix.kern.z.m.wgt$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p.z, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
with(dt.p.z, lines(X,ME,lwd=2,col="black"))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.z.m.wgt$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.z.m.wgt$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(4, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.z.m.wgt[["de"]]$n)*";"~"bandwidth = "*.(f_num(ls.k$ix.kern.z.m.wgt$bw,1))))),
       border=c(NA),
       cex=.5)
dev.off()


#### Figure A.3: parental controls ####

h <- 4
pdf("FigureA3.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p <- ls.k$ix.kern.zp.w$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(ls.k$ix.kern.z.w$est[[1]], polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans("black",.2)))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(ls.k$ix.kern.z.w$est[[1]], lines(X,ME,lwd=2))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.zp.w$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.zp.w$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue<0.001,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(2, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("topright",
       bty="n", cex=.5,
       legend=c("Main (Fig. 2)",
                "+ parental ctrls."),
       fill=c(NA,NA),
       col=c("black",solarizedRed),
       lty=c("solid","solid"),
       border=c(NA,NA))
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.zp.w[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.zp.w$bw)))),
       border=c(NA),
       cex=.5)

dt.p <- ls.k$ix.kern.zp.m$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(ls.k$ix.kern.z.m$est[[1]], polygon(c(X,rev(X)),
                                        c(CI_upper,rev(CI_lower)),
                                        border=NA, col=trans("black",.2)))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(ls.k$ix.kern.z.m$est[[1]], lines(X,ME,lwd=2))
with(dt.p, lines(X,ME,lwd=2,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.zp.m$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.zp.m$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedRed))
braces(xfrom = 2.2, xto = 2.38, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(4, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.zp.m[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.zp.m$bw)))),
       border=c(NA),
       cex=.5)
dev.off()


#### Figure A.4: income-job prestige scatter ####

dt.w <- dt.w %>% 
  mutate(Job_hh = ifelse(!is.na(Job_partner), (Job + Job_partner)/2, NA))
dt.m <- dt.m %>% 
  mutate(Job_hh = ifelse(!is.na(Job_partner), (Job + Job_partner)/2, NA))

dt.w <- cbind(as.data.frame(dt.w), dt.w %>% to_dummy(country, suffix="label"))
dt.m <- cbind(as.data.frame(dt.m), dt.m %>% to_dummy(country, suffix="label"))

# data women
dt.b.w <- dt.w %>% 
  filter(!is.na(Job_partner)) %>% 
  mutate(Job_hh = (Job + Job_partner)/2) %>% 
  select(Job_hh, Job, Job_partner, Income, starts_with("country_")) %>% 
  na.omit() %>% 
  as.data.frame()

bs.w <- binsreg(y=dt.b.w[,"Income"], 
                x=dt.b.w[,"Job_hh"],
                w=dt.b.w[,str_subset(names(dt.b.w),"country_")[-1]],
                line=c(3,3),
                noplot=FALSE,
                polyreg=3
)
dev.off()
dt.p.w.pt <- ggplot_build(bs.w[["bins_plot"]])$data[[1]][,1:2]
dt.p.w.s <- ggplot_build(bs.w[["bins_plot"]])$data[[2]][,1:2]
dt.p.w.l <- ggplot_build(bs.w[["bins_plot"]])$data[[3]][,1:2]

# data men
dt.b.m <- dt.m %>% 
  filter(!is.na(Job_partner)) %>% 
  mutate(Job_hh = (Job + Job_partner)/2) %>% 
  select(Job_hh, Job, Job_partner, Income, starts_with("country_")) %>% 
  na.omit() %>% 
  as.data.frame()

bs.m <- binsreg(y=dt.b.m[,"Income"], 
                x=dt.b.m[,"Job_hh"],
                w=dt.b.m[,str_subset(names(dt.b.m),"country_")[-1]],
                line=c(3,3),
                noplot=FALSE,
                polyreg=3
)
dev.off()
dt.p.m.pt <- ggplot_build(bs.m[["bins_plot"]])$data[[1]][,1:2]
dt.p.m.s <- ggplot_build(bs.m[["bins_plot"]])$data[[2]][,1:2]
dt.p.m.l <- ggplot_build(bs.m[["bins_plot"]])$data[[3]][,1:2]

h <- 4
pdf("FigureA4.pdf", family="serif",height=h, width=h*1.6)

par(mfrow=c(1,2))

plot.a(dt.p.w.pt, 
       xlim=range(dt.p.w.s$x),
       ylim=c(4.2,7.5),
       mar=c(3.5,3.5,2,.5))
lines(dt.p.w.l,col="darkgrey")
points(dt.p.w.pt, pch=16)
rug(x=dt.b.w$Job_hh,col=trans("black",1), lwd = .01)
axis.a(2, at=seq(4.5,7.5,.5))
axis.a()
title.a(xlab=expression(paste('(Job prestige'['resp.'],' + job prestige'['partner'],')','/2')),
        ylab="Income decile", cex.lab=.8,
        main="Female respondents", font.main=3)

plot.a(dt.p.m.pt, 
       xlim=range(dt.p.w.s$x),
       ylim=c(4.2,7.5),
       mar=c(3.5,.5,2,3.5)) #range(dt.p.s$y)
lines(dt.p.m.l,col="darkgrey")
points(dt.p.m.pt, pch=16)
rug(x=dt.b.w$Job_hh,col=trans("black",1), lwd = .01)
axis.a(4, at=seq(4.5,7.5,.5))
axis.a()
title.a(xlab=expression(paste('(Job prestige'['resp.'],' + job prestige'['partner'],')','/2')),
        cex.lab=.8,
        main="Male respondents", font.main=3)
dev.off()


#### Figure A.5: job prestige control ####

solarizedBlue <- solarizedRed

h <- 4
pdf("FigureA5.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p <- ls.k$ix.kern.zo.w$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(ls.k$ix.kern.z.w$est[[1]], polygon(c(X,rev(X)),
                                        c(CI_upper,rev(CI_lower)),
                                        border=NA, col=trans("black",.2)))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedBlue,.2)))
with(ls.k$ix.kern.z.w$est[[1]], lines(X,ME,lwd=2))
with(dt.p, lines(X,ME,lwd=2,col=solarizedBlue))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.zo.w$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.zo.w$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedBlue))
braces(xfrom = 2.2, xto = 2.5, yfrom = dt.p[X==-1,ME], yto = dt.p[X==1,ME], 
       radius=1)
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue<0.001,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(2, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("topright",
       bty="n", cex=.5,
       legend=c("Main (Fig. 2)",
                "+ hh. job prestige ctrl."),
       fill=c(NA,NA),
       col=c("black",solarizedBlue),
       lty=c("solid","solid"),
       border=c(NA,NA))
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.zo.w[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.zo.w$bw)))),       border=c(NA),
       cex=.5)

dt.p <- ls.k$ix.kern.zo.m$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(ls.k$ix.kern.z.m$est[[1]], polygon(c(X,rev(X)),
                                        c(CI_upper,rev(CI_lower)),
                                        border=NA, col=trans("black",.2)))
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedBlue,.2)))
with(ls.k$ix.kern.z.m$est[[1]], lines(X,ME,lwd=2))
with(dt.p, lines(X,ME,lwd=2,col=solarizedBlue))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))

v.d.pvalue <- ls.k$ix.kern.zo.m$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.zo.m$est[[1]]))

with(dt.p[abs(X)==1], segments(X, ME, 2, ME, lty="dotted"))
with(dt.p[abs(X)==1], points(X, ME, pch=16, cex=.5, col=solarizedBlue))
with(dt.p[abs(X)==1], 
     text(2.5, mean(ME), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[abs(X) %in% c(1,6),], axis.a(4, 
                                       at=c(ME, v.ylim, 0), 
                                       labels=f_num(c(ME, v.ylim, 0),2)))
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.zo.m[["de"]]$n)*";"~"bandwidth = "*.(ls.k$ix.kern.zo.m$bw)))),
       border=c(NA),
       cex=.5)
dev.off()



#### Figure A.6: job prestige ####

h <- 4
pdf("FigureA6.pdf", family="serif",height=h, width=h*1.6)

solarizedBlue <- "#268BD2"

v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

dt.p <- ls.k$ix.kern.job.w$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedBlue,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedBlue))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
axis.a(at=seq(-50,50,25))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Job prestige')))

v.d.pvalue <- ls.k$ix.kern.job.w$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.job.w$est[[1]]))

dt.p <- dt.p %>% 
  add_row(X=10) %>% 
  add_row(X=-10) %>% 
  arrange(X) %>% 
  mutate(MEi = zoo::na.approx(ME))

with(dt.p[abs(X)==10], segments(X, MEi, 20, MEi, lty="dotted"))
with(dt.p[abs(X)==10], points(X, MEi, pch=16, cex=.5, col=solarizedBlue))
braces(xfrom = 22, xto = 23, yfrom = dt.p[X==-10,MEi], yto = dt.p[X==10,MEi], 
       radius=1)
with(dt.p[abs(X)==10], 
     text(25, mean(MEi), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[X %in% c(-10,10),], axis.a(2, 
                                       at=c(MEi, v.ylim, 0), 
                                       labels=f_num(c(MEi, v.ylim, 0),2)))
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.job.w[["de"]]$n)*";"~"bandwidth = "*.(round(ls.k$ix.kern.job.w$bw,0))))),
       border=c(NA),
       cex=.5)

dt.p <- ls.k$ix.kern.job.m$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedBlue,.2)))
with(dt.p, lines(X,ME,lwd=2,col=solarizedBlue))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
axis.a(at=seq(-50,50,25))
title.a(main="Male respondents",font.main=3,
        ylab=NULL,
        xlab=expression(paste(Delta,'Job prestige')))

v.d.pvalue <- ls.k$ix.kern.job.m$t.test.diffs[[1]][4][[1]]
dt.p <- data.table(unique(ls.k$ix.kern.job.m$est[[1]]))

dt.p <- dt.p %>% 
  add_row(X=10) %>% 
  add_row(X=-10) %>% 
  arrange(X) %>% 
  mutate(MEi = zoo::na.approx(ME))

with(dt.p[abs(X)==10], segments(X, MEi, 20, MEi, lty="dotted"))
with(dt.p[abs(X)==10], points(X, MEi, pch=16, cex=.5, col=solarizedBlue))
braces(xfrom = 22, xto = 24, yfrom = dt.p[X==-10,MEi], yto = dt.p[X==10,MEi], 
       radius=1)
with(dt.p[abs(X)==10], 
     text(25, mean(MEi), 
          label=ifelse(v.d.pvalue==0,
                       as.expression(bquote(Delta:~italic(p)<.001)),
                       as.expression(bquote(Delta:~italic(p)==.(f_num(v.d.pvalue,3))))),
          adj=c(-.1,.5), cex=.7))
with(dt.p[X %in% c(-10,10),], axis.a(4, 
                                     at=c(MEi, v.ylim, 0), 
                                     labels=f_num(c(MEi, v.ylim, 0),2)))
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ix.kern.job.m[["de"]]$n)*";"~"bandwidth = "*.(round(ls.k$ix.kern.job.m$bw,0))))),
       border=c(NA),
       cex=.5)
dev.off()



#### Figure A.7: varying bandwidth ####

ls.w.bw <- c(readRDS("ls_w_bw.rds")[1:4],
             readRDS("ls_w_bw_2.rds"))
ls.m.bw <- c(readRDS("ls_m_bw.rds"),
             readRDS("ls_m_bw_2.rds"))
v.bw <- names(ls.w.bw)
names(ls.m.bw)

h <- 4
pdf("FigureA7.pdf", family="serif",height=h, width=h*1.6)
v.ylim <- c(-.2,1)
par(mfrow=c(1,2))

  dt.p <- ls.k$ix.kern.z.w$est[[1]]
  with(dt.p, plot.a(X, ME,
                    ylim=v.ylim,
                    mar=c(3.5,3.5,2,.5)))
  abline(h=0, col="darkgrey",lty="solid")
  abline(v=0, col="darkgrey", lty="solid")
  with(dt.p, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
  with(dt.p, lines(X,ME,lwd=3,col=trans("black")))
  for (b in v.bw[-1]) {
    with(ls.w.bw[[b]]$est[[1]], 
         polygon(c(X,rev(X)),
                 c(CI_upper,rev(CI_lower)),
                 border=trans(solarizedRed,1), lwd=.5, col=NA))
    with(ls.w.bw[[b]]$est[[1]], lines(X,ME,lwd=.5,col=trans(solarizedRed,1)))
  }
  v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
  with(dt.p, axis.a(at=v.x,
                    labels=ifelse(v.x %% 2==0, v.x,"")))
  title.a(main="Female respondents",font.main=3,
          ylab="Marg. childcare bias (95% CIs)",
          xlab=expression(paste(Delta,'Education')))
  axis.a(2,at=seq(-.2,1,.2))
  legend("topright",
         bty="n", cex=.5,
         legend=c("Main (Figure 2)",
                  "Varying bandwidth"),
         fill=c(NA,NA),
         col=c("black",solarizedRed),
         lty=c("solid","solid"),
         border=c(NA,NA))
  
  dt.p <- ls.k$ix.kern.z.m$est[[1]]
  with(dt.p, plot.a(X, ME,
                    ylim=v.ylim,
                    mar=c(3.5,.5,2,3.5)))
  abline(h=0, col="darkgrey",lty="solid")
  abline(v=0, col="darkgrey", lty="solid")
  with(dt.p, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
  with(dt.p, lines(X,ME,lwd=3,col=trans("black")))
  for (b in v.bw[-1]) {
    with(ls.m.bw[[b]]$est[[1]], 
         polygon(c(X,rev(X)),
                 c(CI_upper,rev(CI_lower)),
                 border=trans(solarizedRed,1), lwd=.5, col=NA))
    with(ls.m.bw[[b]]$est[[1]], lines(X,ME,lwd=.5,col=trans(solarizedRed,1)))
  }
  axis.a(4,at=seq(-.2,1,.2))
  v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
  with(dt.p, axis.a(at=v.x,
                    labels=ifelse(v.x %% 2==0, v.x,"")))
  title.a(main="Male respondents",font.main=3,
          ylab="Cond. marginal treatment effect",
          xlab=expression(paste(Delta,'Education')))
dev.off()


#### Figure A.8: GAM ####

h <- 4
pdf("FigureA8.pdf", family="serif",height=h, width=h*1.6)

v.ylim <- c(-.2,1.2)
par(mfrow=c(1,2))

dt.p.g <- data.frame(X=ls.k$ls.gam.w$plot$x,
                   ME=ls.k$ls.gam.w$plot$fit,
                   CI_lower=ls.k$ls.gam.w$plot$fit - (1.96*ls.k$ls.gam.w$plot$se),
                   CI_upper=ls.k$ls.gam.w$plot$fit + (1.96*ls.k$ls.gam.w$plot$se))
dt.p <- ls.k$ix.kern.z.w$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,3.5,2,.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans("black",.2)))
with(dt.p, lines(X,ME,lwd=1,col="black"))
with(dt.p.g, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p.g, lines(X,ME,lwd=1,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Female respondents",font.main=3,
        ylab="Marg. childcare bias (95% CIs)",
        xlab=expression(paste(Delta,'Education')))
axis.a(2)
legend("bottomright",
       bty="n", 
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ls.gam.w$summary$n)))),
       border=c(NA),
       cex=.5)
legend("topright",
       bty="n", cex=.5,
       legend=c("Main (Figure 2)",
                "GAM spline"),
       fill=c(NA,NA),
       col=c("black",solarizedRed),
       lty=c("solid","solid"),
       border=c(NA,NA))

dt.p.g <- data.frame(X=ls.k$ls.gam.m$plot$x,
                     ME=ls.k$ls.gam.m$plot$fit,
                     CI_lower=ls.k$ls.gam.m$plot$fit - (1.96*ls.k$ls.gam.m$plot$se),
                     CI_upper=ls.k$ls.gam.m$plot$fit + (1.96*ls.k$ls.gam.m$plot$se))
dt.p <- ls.k$ix.kern.z.m$est[[1]]
with(dt.p, plot.a(X, ME,
                  ylim=v.ylim,
                  mar=c(3.5,.5,2,3.5)))
abline(h=0, col="darkgrey",lty="solid")
abline(v=0, col="darkgrey", lty="solid")
with(dt.p, polygon(c(X,rev(X)),
                     c(CI_upper,rev(CI_lower)),
                     border=NA, col=trans("black",.2)))
with(dt.p, lines(X,ME,lwd=1,col="black"))
with(dt.p.g, polygon(c(X,rev(X)),
                   c(CI_upper,rev(CI_lower)),
                   border=NA, col=trans(solarizedRed,.2)))
with(dt.p.g, lines(X,ME,lwd=1,col=solarizedRed))
v.x <- with(dt.p, seq(range(X)[1],range(X)[2],1))
with(dt.p, axis.a(at=v.x,
                  labels=ifelse(v.x %% 2==0, v.x,"")))
title.a(main="Male respondents",font.main=3,
        ylab="Cond. marginal treatment effect",
        xlab=expression(paste(Delta,'Education')))
axis.a(4)
legend("bottomright",
       bty="n",
       legend=c(as.expression(bquote(italic(N)==.(ls.k$ls.gam.m$summary$n)))),
       border=c(NA),
       cex=.5)
dev.off()



#### Footnote 8: country F-test ####

lm.cntry <- lm_robust(Bad_job ~ as.factor(country)*Edu_diff*Treatment, data=dt.w)
linearHypothesis(lm.cntry, test="F", vcov=lm.cntry$vcov, matchCoefs(lm.cntry, ":Edu_diff:Treatment"))

lm.cntry <- lm_robust(Bad_job ~ as.factor(country)*Edu_diff*Treatment, data=dt.m)
linearHypothesis(lm.cntry, test="F", vcov=lm.cntry$vcov, matchCoefs(lm.cntry, ":Edu_diff:Treatment"))

#### Table A.2: summary stats ####

tab.out <- dt %>% 
  filter(!is.na(Edu_diff) & !is.na(Treatment) & !is.na(Bad_job)) %>% 
  mutate(Female = as_label(Female)) %>% 
  group_by(Female) %>%
  select(Badjob=Bad_job,Treatment,Edudiff=Edu_diff,Education,Income,Job,Unemployed,Unemployedpartner=Unemployed_partner,Age,gendershare=gender_share,Minority,Immigrant,Domicile,Religiosity,IntervieweFemale=Interviewe_Female,Interfere) %>% 
  summarise_all(list(Mean=Mean, 
                     Median=function(x) median(x,na.rm=TRUE), 
                     SD=SD, Min.=Min, Max.=Max,
                     N=function(x) sum(!is.na(x)),
                     na=function(x) sum(is.na(x)))) %>% 
  pivot_longer(-c(Female)) %>% 
  separate(name, into = c("variable", "stat"), sep = "_") %>% 
  pivot_wider(id_cols=c("variable","Female"),
              names_from=stat, values_from=value) %>% 
  arrange(match(variable, c("Badjob","Treatment","Edudiff",
                            "Education","Income","Job",
                            "Unemployed","Unemployedpartner",
                            "Age","gendershare","Minority",
                            "Immigrant","Domicile","Religiosity",
                            "IntervieweFemale","Interfere"))) %>%
  mutate_if(is.numeric,list(~round(., 1))) %>% 
  ungroup() 

tab.out <- tab.out %>% 
  mutate(variable = ifelse(row_number() %in% seq(1,nrow(tab.out),2), 
                           variable, ""))

tab.out$variable <- dplyr::recode(tab.out$variable, 
                                  Badjob="Work full-time while child<3",
                                  Treatment="Treatment",
                                  Edudiff="Household education gap",
                                  Education="Education (ES-ISCED)",
                                  Income="Household income",
                                  Job="Occupational prestige",
                                  Unemployed="Unemployed",
                                  Unemployedpartner="Partner unemployed",
                                  Age="Age",
                                  gendershare="Fraction female household members",
                                  Minority="Ethnic minority",
                                  Immigrant="Born in country",
                                  Domicile="Urban/rural",
                                  Religiosity="Religiosity",
                                  IntervieweFemale="Female interviewer",
                                  Interfere="Interference in interview")

tab.out <- tab.out %>% 
  kable(caption="Summary statistics",
        format="latex",
        booktabs=TRUE,
        # align=c("lll",rep("c",ncol(tab.out)-3))
        escape=FALSE,
        linesep="",
        col.names=c("Variable","Group",
                    "Mean","Median","SD","Min","Max","N","Missing")
  ) %>% 
  kable_styling(latex_options=c("hold_position"),
                font_size = 10,
                position = "center",
                full_width = FALSE)
write(tab.out,file="TableA2.tex")
