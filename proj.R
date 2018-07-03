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

soep.small <- soep.small %>% 
  mutate(soep=1)

vskt.small <- select(vskt, one_of(X.vars))
vskt.small <- vskt.small %>% 
  mutate(soep=0)


soep.vskt = rbind(soep.small, vskt.small)

(birthyear <- ggplot(soep.vskt, aes(gbja, fill = soep)) +
            geom_density(aes(group=soep, alpha = 0.1)))

(pension.ent <- ggplot(soep.vskt, aes(rentenanspruch_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(income <- ggplot(soep.vskt, aes(brutto_zens_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(unemp.ben <- ggplot(soep.vskt, aes(alg_j_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)) +
    xlim(100, 40000))

(exp.al <- ggplot(soep.vskt, aes(exp_al_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1))+
  xlim(1, 100))

(exp.arbeit <- ggplot(soep.vskt, aes(exp_arbeit_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))


#### Clearly, setting bdp10301 to 1, meaning exact pension entitlements created a subset of 
#### observations that is not compatible with the VSKT population!

### Checking for full active population ####

#### Import SOEP #####

soep2 <- import(paste(path, "soep_2012_aktiv.dta" , sep = "/"), setclass = "data.table")

soep2 <- clear.labels(soep2)

head(soep2)

### density/histogram plots for X.vars --> marginal/joint density should now be the same!


X2.vars <- intersect(names(soep2), names(vskt)) 
soep2.small <- select(soep2, one_of(X2.vars))

soep2.small <- soep2.small %>% 
  mutate(soep=1)

soep2.vskt = rbind(soep2.small, vskt.small)

(birthyear <- ggplot(soep2.vskt, aes(gbja, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(pension.ent <- ggplot(soep2.vskt, aes(rentenanspruch_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(income <- ggplot(soep2.vskt, aes(brutto_zens_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(unemp.ben <- ggplot(soep2.vskt, aes(alg_j_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)) +
    xlim(100, 40000))

(exp.al <- ggplot(soep2.vskt, aes(exp_al_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1))+
    xlim(1, 100))

(exp.arbeit <- ggplot(soep2.vskt, aes(exp_arbeit_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

### that looks definitely more promising


### Now check again for imprtant variables:

spearman2(brutto_zens_2012~age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=soep2)

### now rentenanspruch_2012 and exp_al_20_bis2012 age_g relatively and exp.arb also show significant importance for full active population in SOEP

## doing the same for VSKT

spearman2(brutto_zens_2012~age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=vskt)

### rentenanspruch is again has high impact as well as exp_arbeit_20_bis2012, exp_al_20_bis2012 not so much



# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#
################################### Data Fusion ###################################################
# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#

## Perform nearest neighbor distance hot deck with maximum norm as distance measure with 
## rank option = TRUE such that scale differences are accounted for 

soep.vars <- setdiff(names(soep2), names(vskt)) # available just in SOEP

vskt.vars <- setdiff(names(vskt), names(soep2)) # available just in VSKT

(X.mtc <- c("rentenanspruch_2012", "exp_arbeit_20_bis2012", "exp_al_20_bis2012"))

nnd.hd <- NND.hotdeck(data.rec=soep2, data.don=vskt,
                          match.vars=X.mtc, 
                          dist.fun = "minimax",
                          rank = TRUE,
                          constrained = TRUE,
                          constr.alg = "lpSolve",
                          k=20)

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




