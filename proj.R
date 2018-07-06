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


kdens <- ggplot() + geom_density(data=soep.men.full, aes(x=rentenanspruch_2012), colour = "black", fill = "turquoise4", alpha = 0.4) +
                   geom_density(data=vskt, aes(x=rentenanspruch_2012), colour = "black", fill = "gold", alpha = 0.4)
kdens

### use this as basis for further analysis



#### Clearly, setting bdp10301 to 1, meaning exact pension entitlements created a subset of 
#### observations that is not compatible with the VSKT population!

### Checking for full active population ####

#### Import SOEP #####

soep.men.full <- import(paste(path, "soep_2012_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.men.full <- clear.labels(soep.men.full)

head(soep.men.full)

### density/histogram plots for X.vars --> marginal/joint density should now be the same!


X2.vars <- intersect(names(soep.men.full), names(vskt)) 
soep.men.full.small <- select(soep.men.full, one_of(X2.vars))

soep.men.full.small <- soep.men.full.small %>% 
  mutate(soep=1)

soep.men.full.vskt = rbind(soep.men.full.small, vskt.small)

(birthyear <- ggplot(soep.men.full.vskt, aes(gbja, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(pension.ent <- ggplot(soep.men.full.vskt, aes(rentenanspruch_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(income <- ggplot(soep.men.full.vskt, aes(brutto_zens_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

(unemp.ben <- ggplot(soep.men.full.vskt, aes(alg_j_2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)) +
    xlim(100, 40000))

(exp.al <- ggplot(soep.men.full.vskt, aes(exp_al_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1))+
    xlim(1, 100))

(exp.arbeit <- ggplot(soep.men.full.vskt, aes(exp_arbeit_20_bis2012, fill = soep)) +
    geom_density(aes(group=soep, alpha = 0.1)))

### that looks definitely more promising


### Now check again for imprtant variables:

spearman2(brutto_zens_2012~age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=soep.men.full)

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

soep.vars <- setdiff(names(soep.men.full), names(vskt)) # available just in SOEP

vskt.vars <- setdiff(names(vskt), names(soep.men.full)) # available just in VSKT

(X.mtc2 <- c("rentenanspruch_2012", "exp_arbeit_20_bis2012", "exp_al_20_bis2012"))

(X.mtc <- c("rentenanspruch_2012", "exp_arbeit_20_bis2012"))


### donation class define as factor first:

soep.men.full <- 
  soep.men.full %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) 

str(soep.men.full$age_g)


vskt <- 
  vskt %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) 
str(vskt$age_g)


### nearest neighbor distance matching using the L^{inf} norm and lpSolv constrained matching algorythm
 

nnd.hd <- NND.hotdeck(data.rec=soep.men.full, data.don=vskt,
                          match.vars=X.mtc, 
                          don.class = "age_g",
                          dist.fun = "minimax",
                          rank = TRUE,
                          constrained = TRUE,
                          constr.alg = "lpSolve",
                          k=5)

#nnd.hd2 <- NND.hotdeck(data.rec=soep.men.full, data.don=vskt,
 #                     match.vars=X.mtc, 
  #                    don.class = "age_g",
   #                   dist.fun = "mahalanobis",
    #                  constrained = TRUE,
     #                 constr.alg = "lpSolve",
      #                k=5)
                      

#### compare distances via boxplot --> geom_boxplot in ggplot

fA.nnd <- create.fused(data.rec=soep.men.full, data.don=vskt,
                        mtc.ids=nnd.hd$mtc.ids,
                        z.vars=vskt.vars)


summary(nnd.hd$dist.rd)


##########################################################
##########################################################
##########################################################
### diagnostics####




# Calculate the common x and y range for geyser1 and geyser2
xrng = range(c(geyser1$duration, geyser2$duration))
yrng = range(c(geyser1$waiting, geyser2$waiting))

# Calculate the 2d density estimate over the common range
d1 = kde2d(geyser1$duration, geyser1$waiting, lims=c(xrng, yrng), n=200)
d2 = kde2d(geyser2$duration, geyser2$waiting, lims=c(xrng, yrng), n=200)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
diff12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("Duration","Waiting","z")

# Plot difference between geyser2 and geyser1 density
ggplot(diff12.m, aes(Duration, Waiting, z=z, fill=z)) +
  geom_tile() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"), midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE)





