######## Survey Data Fusion ############
rm(list = ls())

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")

#setwd(path) in path.R
source(".path.R")

####### Initiation ##########

#### Import SOEP #####

soep <- import(paste(path, "soep_2012_m_genau.dta" , sep = "/"), setclass = "data.table")

as.data.frame(soep)

stripAttributes(soep)

str(soep)



#### Import VSKT ######

vskt <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
str(vskt)


group.v <- c("area5","sex")
X.mtc <- "age"

rnd.2 <- RANDwNND.hotdeck(data.rec=samp.A, data.don=samp.B,
                          match.vars=X.mtc, don.class=group.v,
                          dist.fun="Manhattan",
                          cut.don="exact", k=20)

fA.knnd <- create.fused(data.rec=samp.A, data.don=samp.B,
                        mtc.ids=rnd.2$mtc.ids,
                        z.vars="labour5")

head(rnd.2$sum.dist)