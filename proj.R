######## Survey Data Fusion ############
rm(list = ls())

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")

#setwd(path) in path.R
source(".path.R")

####### Initiation ##########

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

#### Import SOEP #####

soep <- import(paste(path, "soep_2012_m_genau.dta" , sep = "/"), setclass = "data.table")

soep <- clear.labels(soep)

head(soep)

#### Import VSKT ######

vskt <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
vskt <- clear.labels(vskt)

head(vskt)


#### Define common variables ####

## here a randomForest evaluation of the most important matching variables might be fruitful!


##


## First simple Adjusted R-Squarred evaluations of mathching variables

spearman2(brutto_zens_2012~age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=soep)

### it seems only rentenanspruch_2012 and exp_al_20_bis2012 show significant importance for SOEP

## doing the same for VSKT

spearman2(brutto_zens_2012~age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=vskt)

### rentenanspruch is again has high impact as well as exp_arbeit_20_bis2012, exp_al_20_bis2012 not so much
### problem...we don't have the same population --> apparently there is selection regarding age for those who
### give reliable information on their pension entitlements

### density/histogram plots for X.vars --> marginal/joint density should be the same!

# Variables available in both datasets

X.vars <- intersect(names(soep), names(vskt)); X.vars 
soep.small <- select(soep, one_of(X.vars))
vskt.small <- select(vskt, one_of(X.vars))


densityplot(~gbja,soep.small, xlab = "Birthyear in SOEP")
densityplot(~gbja,vskt.small, xlab = "Birthyear in VSKT")

densityplot(~rentenanspruch_2012,soep.small, xlab = "Pension entitlements in SOEP")
densityplot(~rentenanspruch_2012,vskt.small, xlab = "Pension entitlements in VSKT")

histogram(~exp_al_20_bis2012,soep.small, xlab = "Experience of unemployment in SOEP")
histogram(~exp_al_20_bis2012,vskt.small, xlab = "Experience of unemployment in VSKT")

histogram(~exp_arbeit_20_bis2012,soep.small, xlab = "Working experience in SOEP")
histogram(~exp_arbeit_20_bis2012,vskt.small, xlab = "Working experience in VSKT")

densityplot(~brutto_zens_2012,soep.small, xlab = "Labour income in SOEP")
densityplot(~brutto_zens_2012,vskt.small, xlab = "Labour income in VSKT")

densityplot(~alg_j_2012,soep.small, xlab = "Unemployment benefits in SOEP")
densityplot(~alg_j_2012,vskt.small, xlab = "Unemployment benefits in VSKT")

#### Clearly, setting bdp10301 to 1, meaning exact pension entitlements created a subset of 
#### observations that is not compatible with the VSKT population!


#### Choosing Variables with respect to their contribution to the reduction of uncertainty ####

















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




