##################
#### ANALYSES ####
##################

# setwd("/Users/oyvindskorge/Dropbox/Papers/Giani-Hope-Skorge/7-Replication/")

#### SETTINGS FOR THE ANALYSES ####

# Please change the cores argument to the number of 
# cores on your computer
v.cores <- 1
v.parallel <- TRUE

# 10,000 bootstraps
v.boots <- 10000

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

dt <- sjlabelled::read_data("ESSgenderUpdate2.dta")

dt <- dt %>% 
  mutate(Bad_jobZ = scale(Bad_job)) 

dt <- dt %>% 
  set_labels(Female, labels = c("Men", "Women"))

dt.w <- dt %>% 
  filter(Female==2)

dt.m <- dt %>% 
  filter(Female==1)

#### MAIN ANALYSIS ####

#### Setup ####

v.z <- c("Age","Age_sq","gender_share","Child","Minority","Immigrant","Domicile",
  "Education","Income","Income_sq","Job","Unemployed","Unemployed_partner",
  "Religiosity","Interviewe_Female","Interfere")
v.zp <- c(v.z,"eduParentsDiff","emp14Diff")
v.zo <- c(v.z,"Job_diff")

v.z.s <- c("Age","Age_sq","gender_share","Child","Minority","Immigrant","Domicile",
         "Income","Income_sq","Job","Unemployed",
         "Religiosity","Interviewe_Female","Interfere")
v.zp.s <- c(v.z.s,"eduParentsDiff","emp14Diff")

dt.w <- dt.w %>% 
  select(all_of(c("Bad_jobZ","Bad_job","Treatment","Edu_diff","country","cntry","wgt",
           unique(c(v.zp,v.zo))))) %>% 
  mutate_at(v.zp, arm::rescale)

dt.m <- dt.m %>% 
  select(all_of(c("Bad_jobZ","Bad_job","Treatment","Edu_diff","country","cntry","wgt",
                  unique(c(v.zp,v.zo))))) %>% 
  mutate_at(v.zp, arm::rescale)


#### Results for Figure 2. Interflex: with covariates ####

ix.kern.z.w <- interflex(dt.w, 
                         Y="Bad_job",
                         D="Treatment",
                         X="Edu_diff",
                         Z=v.z,
                         estimator="kernel",
                         FE=c("country"),
                         parallel=v.parallel,
                         cores=v.cores,
                         nboots=v.boots,
                         neval=13,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         bw.adaptive=TRUE,
                         quantile.eval=FALSE,
                         diff.values=c(-1,1),
                         predict=FALSE
)
saveRDS(ix.kern.z.w,file="ix_kern_z_w.rds")
# ix.kern.z.w <- readRDS("ix_kern_z_w.rds")
# ix.kern.z.w$t.test.diffs
# ix.kern.z.w$graph

ix.kern.z.m <- interflex(dt.m, 
                         Y="Bad_job",
                         D="Treatment",
                         X="Edu_diff",
                         Z=v.z,
                         estimator="kernel",
                         FE=c("country"),
                         parallel=v.parallel,
                         cores=v.cores,
                         nboots=v.boots,
                         neval=13,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         bw.adaptive=TRUE,
                         quantile.eval=FALSE,
                         diff.values=c(-1,1),
                         predict=FALSE
)
saveRDS(ix.kern.z.m,file="ix_kern_z_m.rds")
# ix.kern.z.m <- readRDS("ix_kern_z_m.rds")
# ix.kern.z.m$t.test.diffs
# ix.kern.z.m$graph


#### Results for Figure A.1. Interflex: without covariates ####

ix.kern.x.w <- interflex(dt.w, 
                         Y="Bad_job",
                         D="Treatment",
                         X="Edu_diff",
                         estimator="kernel",
                         parallel=v.parallel,
                         cores=v.cores,
                         nboots=v.boots,
                         neval=13,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         bw.adaptive=TRUE,
                         quantile.eval=FALSE,
                         diff.values=c(-1,1),
                         predict=FALSE
)
saveRDS(ix.kern.x.w,file="ix_kern_x_w.rds")
# ix.kern.x.w <- readRDS("ix_kern_x_w.rds")
# ix.kern.x.w$t.test.diffs
# ix.kern.x.w$graph

ix.kern.x.m <- interflex(dt.m, 
                         Y="Bad_job",
                         D="Treatment",
                         X="Edu_diff",
                         estimator="kernel",
                         parallel=v.parallel,
                         cores=v.cores,
                         neval=13,
                         nboots=v.boots,
                         cutoffs = c(-6,-1,.9,6),
                         na.rm=TRUE,
                         wald=TRUE,
                         bw.adaptive=TRUE,
                         quantile.eval=FALSE,
                         diff.values=c(-1,1),
                         predict=FALSE
)
saveRDS(ix.kern.x.m,file="ix_kern_x_m.rds")
# ix.kern.x.m <- readRDS("ix_kern_x_m.rds")
# ix.kern.x.m$t.test.diffs
# ix.kern.x.m$graph

#### Results for Figure A.2. Interflex: with covariates and weights ####

dt.w.wgt <- dt.w %>% 
  select(c("Bad_job","Treatment","Edu_diff","wgt","country",v.z)) %>% 
  na.omit()

ix.kern.z.w.wgt <- interflex(dt.w.wgt, 
                             Y="Bad_job",
                             D="Treatment",
                             X="Edu_diff",
                             Z=v.z,
                             estimator="kernel",
                             FE=c("country"),
                             weights = "wgt",
                             parallel=v.parallel,
                             cores=v.cores,
                             nboots=v.boots,
                             neval=13,
                             na.rm=TRUE,
                             wald=TRUE,
                             bw.adaptive=TRUE,
                             quantile.eval=FALSE,
                             diff.values=c(-1,1),
                             predict=FALSE
)
saveRDS(ix.kern.z.w.wgt,file="ix_kern_z_w_wgt.rds")
# ix.kern.z.w.wgt <- readRDS("ix_kern_z_w_wgt.rds")
# ix.kern.z.w.wgt$t.test.diffs
# ix.kern.z.w.wgt$graph

dt.m.wgt <- dt.m %>% 
  select(c("Bad_job","Treatment","Edu_diff","wgt","country",v.z)) %>% 
  na.omit()

ix.kern.z.m.wgt <- interflex(dt.m.wgt, 
                             Y="Bad_job",
                             D="Treatment",
                             X="Edu_diff",
                             Z=v.z,
                             estimator="kernel",
                             FE=c("country"),
                             weights = "wgt",
                             # cl="country",
                             parallel=v.parallel,
                             cores=v.cores,
                             nboots=v.boots,
                             neval=13,
                             cutoffs = c(-6,-1,.9,6),
                             na.rm=TRUE,
                             wald=TRUE,
                             bw.adaptive=TRUE,
                             quantile.eval=FALSE,
                             diff.values=c(-1,1),
                             predict=FALSE
)
saveRDS(ix.kern.z.m.wgt,file="ix_kern_z_m_wgt.rds")
# ix.kern.z.m.wgt <- readRDS("ix_kern_z_m_wgt.rds")
ix.kern.z.m.wgt$t.test.diffs
ix.kern.z.m.wgt$graph


#### Results for Figure A.3. Interflex: with parents covariates ####

ix.kern.zp.w <- interflex(dt.w, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Edu_diff",
                          Z=v.zp,
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          cutoffs = c(-6,-1,.9,6),
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-1,1),
                          predict=FALSE
)
saveRDS(ix.kern.zp.w,file="ix_kern_zp_w.rds")
# ix.kern.zp.w <- readRDS("ix_kern_zp_w.rds")
# ix.kern.zp.w$t.test.diffs
# ix.kern.zp.w$graph

ix.kern.zp.m <- interflex(dt.m, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Edu_diff",
                          Z=v.zp,
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          cutoffs = c(-6,-1,.9,6),
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-1,1),
                          predict=FALSE
)
saveRDS(ix.kern.zp.m,file="ix_kern_zp_m.rds")
# ix.kern.zp.m <- readRDS("ix_kern_zp_m.rds")
# ix.kern.zp.m$t.test.diffs
# ix.kern.zp.m$graph


#### Results for Figure A.5. Interflex: with job prestige ####

ix.kern.zo.w <- interflex(dt.w, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Edu_diff",
                          Z=v.zo,
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          cutoffs = c(-6,-1,.9,6),
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-1,1),
                          predict=FALSE
)
saveRDS(ix.kern.zo.w,file="ix_kern_zo_w.rds")
# ix.kern.zp.w <- readRDS("ix_kern_zp_w.rds")
# ix.kern.zo.w$t.test.diffs
# ix.kern.zo.w$graph

ix.kern.zo.m <- interflex(dt.m, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Edu_diff",
                          Z=v.zo,
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          cutoffs = c(-6,-1,.9,6),
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-1,1),
                          predict=FALSE
)
saveRDS(ix.kern.zo.m,file="ix_kern_zo_m.rds")
# ix.kern.zp.m <- readRDS("ix_kern_zp_m.rds")
# ix.kern.zo.m$t.test.diffs
# ix.kern.zo.m$graph


#### Results for Figure A.6. Interflex: by job prestige household gap ####

ix.kern.job.w <- interflex(dt.w, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Job_diff",
                          Z=c(v.zo[-length(v.zo)],"Edu_diff"),
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-10,10),
                          predict=FALSE
)
saveRDS(ix.kern.job.w,file="ix_kern_job_w.rds")
# ix.kern.job.w$t.test.diffs
# ix.kern.job.w$graph

ix.kern.job.m <- interflex(dt.m, 
                          Y="Bad_job",
                          D="Treatment",
                          X="Job_diff",
                          Z=c(v.zo[-length(v.zo)],"Edu_diff"),
                          estimator="kernel",
                          FE=c("country"),
                          parallel=v.parallel,
                          cores=v.cores,
                          nboots=v.boots,
                          neval=13,
                          na.rm=TRUE,
                          wald=TRUE,
                          bw.adaptive=TRUE,
                          quantile.eval=FALSE,
                          diff.values=c(-10,10),
                          predict=FALSE
)
saveRDS(ix.kern.job.m,file="ix_kern_job_m.rds")
# ix.kern.job.m$t.test.diffs
# ix.kern.job.m$graph


#### Results for Figure A.7. Interflex: varying bandwidth ####

v.bw <- c(.1, .5, 1, 3, 5, 10)
ls.w.bw <- list()
ls.m.bw <- list()

for (b in v.bw[1:4]) {
  
  print(paste0("-------",b,"-------"))
  
  ls.w.bw[[paste0("bw",b)]] <- interflex(dt.w, 
                                         Y="Bad_job",
                                         D="Treatment",
                                         X="Edu_diff",
                                         Z=v.z,
                                         estimator="kernel",
                                         FE=c("country"),
                                         parallel=v.parallel,
                                         cores=v.cores,
                                         nboots=v.boots,
                                         neval=13,
                                         na.rm=TRUE,
                                         wald=TRUE,
                                         bw=b,
                                         quantile.eval=FALSE,
                                         diff.values=c(-1,1),
                                         predict=FALSE
  )
  
  ls.m.bw[[paste0("bw",b)]] <- interflex(dt.m, 
                                         Y="Bad_job",
                                         D="Treatment",
                                         X="Edu_diff",
                                         Z=v.z,
                                         estimator="kernel",
                                         FE=c("country"),
                                         parallel=v.parallel,
                                         cores=v.cores,
                                         nboots=v.boots,
                                         neval=13,
                                         na.rm=TRUE,
                                         wald=TRUE,
                                         bw=b,
                                         quantile.eval=FALSE,
                                         diff.values=c(-1,1),
                                         predict=FALSE
  )
}
saveRDS(ls.w.bw,file="ls_w_bw.rds")
saveRDS(ls.m.bw,file="ls_m_bw.rds")

ls.w.bw.2 <- list()
ls.m.bw.2 <- list()

for (b in v.bw[5:6]) {
  
  print(paste0("-------",b,"-------"))
  
  ls.w.bw.2[[paste0("bw",b)]] <- interflex(dt.w, 
                                           Y="Bad_job",
                                           D="Treatment",
                                           X="Edu_diff",
                                           Z=v.z,
                                           estimator="kernel",
                                           FE=c("country"),
                                           parallel=v.parallel,
                                           cores=v.cores,
                                           nboots=v.boots,
                                           neval=13,
                                           na.rm=TRUE,
                                           wald=TRUE,
                                           bw=b,
                                           quantile.eval=FALSE,
                                           diff.values=c(-1,1),
                                           predict=FALSE
  )
  
  ls.m.bw.2[[paste0("bw",b)]] <- interflex(dt.m, 
                                           Y="Bad_job",
                                           D="Treatment",
                                           X="Edu_diff",
                                           Z=v.z,
                                           estimator="kernel",
                                           FE=c("country"),
                                           parallel=v.parallel,
                                           cores=v.cores,
                                           nboots=v.boots,
                                           neval=13,
                                           na.rm=TRUE,
                                           wald=TRUE,
                                           bw=b,
                                           quantile.eval=FALSE,
                                           diff.values=c(-1,1),
                                           predict=FALSE
  )
}
saveRDS(ls.w.bw.2,file="ls_w_bw_2.rds")
saveRDS(ls.m.bw.2,file="ls_m_bw_2.rds")



#### Results for Figure A.8. GAM: with covariates ####

v.z.g <- str_subset(v.z,"_sq",negate=T)
v.z.s <- v.z.g[c(1:3,7,8,9,12)]
setdiff(v.z.g,v.z.s)
f.gam <- as.formula(
  paste0("Bad_job ~ s(Edu_diff) + s(Edu_diff, by=Treatment)",
         " + ", paste0("s(",v.z.s,", k=6)", collapse=" + "),
         "+ as.factor(country)",
         " + ", paste0("s(Edu_diff, by=",v.z.g,", k=6)", collapse=" + ")
         )
)

gam.w <- gam(f.gam, data=dt.w)
ls.gam.w <- list(plot=plot(gam.w,select=2)[[2]],
                 summary=summary(gam.w))
saveRDS(ls.gam.w, file="ls_gam_w.rds")

gam.m <- gam(f.gam, data=dt.m)
ls.gam.m <- list(plot=plot(gam.m,select=2)[[2]],
                 summary=summary(gam.m))
saveRDS(ls.gam.m, file="ls_gam_m.rds")


