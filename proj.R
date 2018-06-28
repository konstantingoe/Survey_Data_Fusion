######## Survey Data Fusion ############
rm(list = ls())

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")

#setwd(path) in path.R
source(".path.R")

####### Initiation ##########

#### Import SOEP #####

soep <- import(paste(path, "soep_2012_m_genau.dta" , sep = "/"), setclass = "data.table")

clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


soep <- clear.labels(soep)

head(soep)

#### Import VSKT ######

vskt <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
vskt <- clear.labels(vskt)

head(vskt)


#### Define common variables ####

# Variables available in both datasets
X.vars <- intersect(names(soep), names(vskt)); X.vars 

soep.vars <- setdiff(names(soep), names(vskt)) # available just in SOEP

vskt.vars <- setdiff(names(vskt), names(soep)) # available just in VSKT

X.mtc <- X.vars

rnd.hd <- RANDwNND.hotdeck(data.rec=soep, data.don=vskt,
                          match.vars=X.mtc, 
                          dist.fun="Mahalanobis",
                          cut.don="min", k=20)

fA.knnd <- create.fused(data.rec=soep, data.don=vskt,
                        mtc.ids=rnd.hd$mtc.ids,
                        z.vars=vskt.vars)

head(rnd.hd$sum.dist)


#checking distances
sum(rnd.hd$dist.rd) # 20 k nearest neighbors

#estimating marginal distribution of spez_scheidung
tt0 <- xtabs(~spez_scheidung, data=vskt) # reference distr.
tt <- xtabs(~spez_scheidung, data=fA.knnd) # synt unconstr.
#
# checking marginal distributions
cp1 <- comp.prop(p1=tt, p2=tt0, n1=nrow(fA.knnd), n2=NULL, ref=TRUE)

cp1$meas #marginal distribution after fusion of spez_scheidung


densityplot(~brutto_zens_2005, vskt)
densityplot(~brutto_zens_2005, fA.knnd)




