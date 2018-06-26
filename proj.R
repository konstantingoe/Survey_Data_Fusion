######## Survey Data Fusion ############

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")
source("library.R")


####### Initiation ##########

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