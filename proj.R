######## Survey Data Fusion ############
rm(list = ls())

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")

#setwd(path) in path.R
source(".path.R")


source("functions.R")
####### Initiation ##########


#### Import SOEP - full active population #####

soep.men.active <- import(paste(path, "soep_2012_m_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.men.active <- clear.labels(soep.men.active)

head(soep.men.active)


#### Import VSKT active  ######

vskt.m.active <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
vskt.m.active <- clear.labels(vskt.m.active)

head(vskt.m.active)




### Now check for important variables:

#### Define common variables ####

### density/histogram plots for X.vars --> marginal/joint density should be the same!

# Variables available in both datasets

(X.vars <- intersect(names(soep.men.active), names(vskt.m.active)))

soep.men.active <- soep.men.active %>% 
  mutate(soep=1)

vskt.m.active <- vskt.m.active %>% 
  mutate(soep=0)

joint <- bind_rows(soep.men.active, vskt.m.active)
joint <- joint %>% 
mutate(soep = factor(soep, ordered = F)) 

####### Pension entitlements
anwartschaften.plot <- mydensplot(joint, "rentenanspruch_2012", xname="Rentenanwartschaften in EUR", lmts = c(1,3000))
ggsave("anwartschaften.pdf")

####### Birthyear
birthyear.plot <- mydensplot(joint, "gbja", xname="Geburtsjahr")
ggsave("birthyear.pdf")

####### Icome
income.plot <- mydensplot(joint, "brutto_zens_2012", xname = "Arbeitseinkommen (2012) in €", lmts = c(1, 70000))
ggsave("income.pdf")

####### Worktime
worktime.plot <- mydensplot(joint, "exp_arbeit_20_bis2012",xname = "Arbeitserfahrung in Monaten")
ggsave("wtime.pdf")

####### Unemployment benefit 
unempben.plot <- mydensplot(joint, "alg_j_2012",xname = "Arbeitslosengeld in €", lmts = c(1,18000))
ggsave("unempben.pdf") 

####### Unemployment exprerience  
unempexp.plot <- mydensplot(joint, "exp_al_20_bis2012",xname = "Monate in Arbeitslosigkeit", lmts = c(1,200))
ggsave("unemptime.pdf")


p <- cowplot::plot_grid(birthyear.plot, anwartschaften.plot, ncol = 2)
ggsave("grid1.pdf", p)


q <- cowplot::plot_grid(unempexp.plot, unempben.plot, worktime.plot, income.plot, ncol=2, nrow=2)
ggsave("grid2.pdf", q)


#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#######  Kolmogorov-Smirnoff-Test for equality of conditional distributions #####
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

test.soep <- soep.men.active %>% 
         mutate(rentenanspruch_2012 = ifelse(rentenanspruch_2012>0,rentenanspruch_2012, NA)) %>% 
         mutate(brutto_zens_2012 = ifelse(brutto_zens_2012>0, brutto_zens_2012, NA)) %>% 
         mutate(alg_j_2012 = ifelse(alg_j_2012>0, alg_j_2012, NA)) %>% 
         mutate(exp_al_20_bis2012 = ifelse(exp_al_20_bis2012>0, alg_j_2012, NA))


test.vst <- vskt.m.active %>% 
  mutate(rentenanspruch_2012 = ifelse(rentenanspruch_2012>0,rentenanspruch_2012, NA))%>% 
  mutate(brutto_zens_2012 = ifelse(brutto_zens_2012>0, brutto_zens_2012, NA)) %>% 
  mutate(alg_j_2012 = ifelse(alg_j_2012>0, alg_j_2012, NA)) %>% 
  mutate(exp_al_20_bis2012 = ifelse(exp_al_20_bis2012>0, alg_j_2012, NA))



ks.test(test.soep$rentenanspruch_2012, test.vst$rentenanspruch_2012, alternative = "two.sided")
### reject equality of rentenanwartschaften

ks.test(test.soep$gbja, test.vst$gbja, alternative = "two.sided")
### reject equality of gebja equailty

ks.test(test.soep$brutto_zens_2012, test.vst$brutto_zens_2012, alternative = "two.sided")
### reject equality of income equailty

ks.test(test.soep$exp_arbeit_20_bis2012, test.vst$exp_arbeit_20_bis2012, alternative = "two.sided")
### reject equality of working experience

ks.test(test.soep$alg_j_2012, test.vst$alg_j_2012, alternative = "two.sided")
### reject equality of conditional distribution of unemployment benefit

ks.test(test.soep$exp_al_20_bis2012, test.vst$exp_al_20_bis2012, alternative = "two.sided")
### reject equality of conditional distribution of unemployment benefit




## here a randomForest evaluation of the most important matching variables might be fruitful!
## In general one should choose those X_m that have jointly the highest correlation (reduction in Variance)
## w.r.t.to the Variables only available in Y and Z.
## Namely: Variable of interest in SOEP: educ_cat 
##         Variable of interest in VSKT: not that easy---> full income cycle...

##

### first denote as factor: Doing a quick Regression Model for Educ to see which Variables are important:

soep.men.active <- 
  soep.men.active %>% 
  mutate(educ_cat = factor(educ_cat, ordered = TRUE)) 

str(soep.men.active$educ_cat)
### standardizing coefficients

modelformula <- educ~ brutto_zens_2012+age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012
all.vars(modelformula)
seop.stdz <- lapply(soep.men.active[, all.vars(modelformula)], scale) 

lm <- lm(educ~ brutto_zens_2012+age_g+ spez_scheidung + rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
         data=seop.stdz)
summary(lm)

### Income, Age and Working experience show the highest effect
spearman2(educ~ brutto_zens_2012+age_g+ spez_scheidung + rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=soep.men.active)

## doing the same for VSKT

spearman2(brutto_zens_2013~ brutto_zens_2012 + age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=vskt.m.active)

### brutto_zens_2012, Rentenanspruch has high impact as well as exp_arbeit_20_bis2012


# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#
################################### Data Fusion ###################################################
# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#

## Perform nearest neighbor distance hot deck with maximum norm as distance measure with 
## rank option = TRUE such that scale differences are accounted for 

soep.vars <- setdiff(names(soep.men.active), names(vskt.m.active)) # available just in SOEP

vskt.vars <- setdiff(names(vskt.m.active), names(soep.men.active)) # available just in VSKT

(X.mtc <- c("brutto_zens_2012", "rentenanspruch_2012", "exp_arbeit_20_bis2012"))
(X2.mtc <- c("rentenanspruch_2012", "exp_arbeit_20_bis2012"))


### donation class define as factor first:

soep.men.active <- 
  soep.men.active %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) 

str(soep.men.active$age_g)


vskt.m.active <- 
  vskt.m.active %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) 
str(vskt.m.active$age_g)


### nearest neighbor distance matching using the L^{inf} norm and lpSolv constrained matching algorythm
 

nnd.hd <- NND.hotdeck(data.rec=soep.men.active, data.don=vskt.m.active,
                          match.vars=X.mtc, 
                          don.class = "age_g",
                          dist.fun = "minimax",
                          rank = TRUE,
                          constrained = TRUE,
                          constr.alg = "lpSolve",
                          k=5)

## With fewer Matching Variables 

#nnd.hd <- NND.hotdeck(data.rec=soep.men.active, data.don=vskt.m.active,
 #                     match.vars=X.mtc, 
  #                    don.class = "age_g",
   #                   dist.fun = "minimax",
    #                  rank = TRUE,
     #                 constrained = TRUE,
      #                constr.alg = "lpSolve",
       #               k=5)

#nnd.hd2 <- NND.hotdeck(data.rec=soep.men.active, data.don=vskt.m.active,
 #                     match.vars=X.mtc, 
  #                    don.class = "age_g",
   #                   dist.fun = "mahalanobis",
    #                  constrained = TRUE,
     #                 constr.alg = "lpSolve",
      #                k=5)
                      

#### compare distances via boxplot --> geom_boxplot in ggplot

fA.nnd <- create.fused(data.rec=soep.men.active, data.don=vskt.m.active,
                        mtc.ids=nnd.hd$mtc.ids,
                        z.vars=vskt.vars)


summary(nnd.hd$dist.rd)

### make boxplot with dist.rd
pdf('boxplot.pdf')
box <- boxplot(nnd.hd$dist.rd, xlab="Supremumsnorm") 
title("Boxplot of distances between SOEP and VSKT cases")
dev.off()

save(fA.nnd, file = "fused.RDA")
save(nnd.hd, file = "matched.RDA")

##########################################################
##########################################################
##########################################################
### diagnostics####
load("fused.RDA")
load("matched.RDA")
### prefereably put densities of VSKT and fused data set into one grid in order to compare
### of course it now makes sense to compare the non matching variables!

# main focus! after fusion the distributional structure should have prevailed!

fA.nnd <- fA.nnd %>% 
  mutate(vskt=0)

vskt.m.active <- vskt.m.active %>% 
  mutate(vskt=1)

joint2 <- bind_rows(fA.nnd, vskt.m.active)

joint2 <- joint2 %>% 
  mutate(vskt=factor(vskt, ordered = F))

##### Income 1989
fused_inc89.plot <- mydensplot.post(joint2, "brutto_zens_1998", xname = "Einkommen in €")
ggsave("fuincome89.pdf")

### Kolmogorov-Smirnoff-Test tells us that Distributions are identical
ks.test(fA.nnd$rente_total_2013, vskt.m.active$rente_total_2013, alternative = "two.sided")


##### Unemployment benefit 1989
fused_alg89.plot <- mydensplot.post(joint2, "alg_j_1989", xname = "Arbeitslosengeld in €", lmts=c(1, 18000))
ggsave("fualg89.pdf")


ks.test(fA.nnd$alg_j_1989, vskt.m.active$alg_j_1989, alternative = "two.sided")
### KS Test tells us that they are identical


### Put them in a grid
pp <- cowplot::plot_grid(fused_inc89.plot, fused_alg89.plot, ncol = 2)
ggsave("grid3.pdf", pp)


### Graphical diagnostics

# Calculate the common x and y range 
(xrng = range(c(fA.nnd$brutto_zens_2011, vskt.m.active$brutto_zens_2011)))
(yrng = range(c(fA.nnd$gbja, vskt.m.active$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$brutto_zens_2011, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.m.active$brutto_zens_2011, vskt.m.active$gbja, lims=c(xrng, yrng), n=200)

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
names(diff12.m) = c("Einkommen","Geburtsjahr","z")

# Plot difference between densities
ggplot(diff12.m, aes(Einkommen, Geburtsjahr, z=z, fill=z)) +
  geom_tile() +
  theme_classic() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
  scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  ggtitle("Einkommens-Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("difference.pdf")


#mydiagnostics(fA.nnd, vskt.m.active, var1=brutto_zens_2011, var2=gbja)
# make it work later this week

##### Matching passive male population ######

## I would rather write a function that automates the Nearest Neighbor Random Hot Deck Matching
## However: Aint't nobody got time for this 

#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#

#### Import SOEP passive #####

soep.m.passive <- import(paste(path, "soep_2012_m_passiv.dta" , sep = "/"), setclass = "data.table")
soep.m.passive <- clear.labels(soep.m.passive)

head(soep.m.passive)

#### Import VSKT ######

vskt.m.passive <- import(paste(path, "vskt_m_passiv.dta" , sep = "/"), setclass = "data.table")
vskt.m.passive <- clear.labels(vskt.m.passive)

head(vskt.m.passive)

# Variables available in both datasets

(xp.vars <- intersect(names(soep.m.passive), names(vskt.m.passive)))

soep.m.passive <- soep.m.passive %>% 
  mutate(soep=1)

vskt.m.passive <- vskt.m.passive %>% 
  mutate(soep=0)

joint.p <- bind_rows(soep.m.passive, vskt.m.passive)
joint.p <- joint.p %>% 
  mutate(soep=factor(soep, ordered = F))

joint.p2 <- joint.p %>% 
  subset(em_rente==0) %>% 
  select(-em_rente)

### Pension total 
rente.plot <- mydensplot(joint.p2, "rente_total_2012", xname = "Rentenhöhe (ohne EM) in €", lmts =c(1, 65000))
ggsave("rente.pdf")

### Age density
age.plot <- mydensplot(joint.p2, "gbja", xname = "Geburtsjahr (ohne EM)")
ggsave("age.pdf")

### Workingtime passive
worktimerente.plot <- mydensplot(joint.p, "exp_arbeit_20_bis2012", xname = "Arbeitserfahrung in Monaten der Rentner (ikl. EM-Rente)")
ggsave("wtimerente.pdf")

### Unemployment entitlements
unempexprente.plot <- mydensplot(joint.p, "exp_al_20_bis2012", xname = "Monate in Arbeitslosigkeit (inkl. EM-Rentner)", lmts = c(1, 200))
ggsave("unemptimerente.pdf")


pp <- cowplot::plot_grid(age.plot, rente.plot, unempexprente.plot, worktimerente.plot, nrow = 2, ncol = 2)
ggsave("grid3.pdf", pp)



####### Identifying Matching Variables in Pension Sample ######


### standardizing coefficients

modelformulap <- educ~ age+spez_scheidung+rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012
all.vars(modelformulap)
seopp.stdz <- lapply(soep.m.passive[, all.vars(modelformulap)], scale) 

lm.p <- lm(educ~ age+ spez_scheidung + rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012,
         data=seopp.stdz)
summary(lm.p)

### Experience Arbeit und Experience AL sowie rente_total_2012
spearman2(educ~ age+ spez_scheidung + rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012, p=2,
   data=seopp.stdz)

## doing the same for VSKT

spearman2(rente_total_2013~ rente_total_2012 + age_g + spez_scheidung + exp_arbeit_20_bis2012 + exp_al_20_bis2012,
            p=2, data=vskt.m.passive)

### here merely age and rente_total_2012


##### deciding for age and rente_total_2012 as matching variables
##### further using em_rente as group variable 



## Perform nearest neighbor distance hot deck with maximum norm as distance measure with 
## rank option = TRUE such that scale differences are accounted for 

soep.passive.vars <- setdiff(names(soep.m.passive), names(vskt.m.passive)) # available just in SOEP

vskt.passive.vars <- setdiff(names(vskt.m.passive), names(soep.m.passive)) # available just in VSKT

(Xp.mtc <- c("rente_total_2012", "exp_arbeit_20_bis2012", "age"))
(Xp2.mtc <- c("rente_total_2012", "age"))


### donation class em_rente define as factor first:

soep.m.passive <- 
  soep.m.passive %>% 
  mutate(em_rente = factor(em_rente)) 

str(soep.m.passive$em_rente)


vskt.m.passive <- 
  vskt.m.passive %>% 
  mutate(em_rente = factor(em_rente)) 
str(vskt.m.passive$em_rente)


### nearest neighbor distance matching using the L^{inf} norm and lpSolv constrained matching algorythm


nnd.hd.passive <- NND.hotdeck(data.rec=soep.m.passive, data.don=vskt.m.passive,
                      match.vars=Xp.mtc, 
                      don.class = "em_rente",
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)

fA.nnd.passive <- create.fused(data.rec=soep.m.passive, data.don=vskt.m.passive,
                       mtc.ids=nnd.hd.passive$mtc.ids,
                       z.vars=vskt.passive.vars)


summary(nnd.hd.passive$dist.rd)

### make boxplot with dist.rd
pdf('boxplotpassive.pdf')
box <- boxplot(nnd.hd.passive$dist.rd, xlab="Supremumsnorm") 
title("Boxplot der Supremumsnormdistanzen zwischen SOEP and VSKT Rentnern")
dev.off()

save(fA.nnd.passive, file = "fusedpassive.RDA")
save(nnd.hd.passive, file = "matchedpassive.RDA")


### diagnostics for passive population ####
load("fusedpassive.RDA")
load("matchedpassive.RDA")
### prefereably put densities of VSKT and fused data set into one grid in order to compare
### of course it now makes sense to compare the non matching variables!

# main focus! after fusion the distributional structure should have prevailed!

fA.nnd.passive <- fA.nnd.passive %>% 
  mutate(vskt=0)

vskt.m.passive <- vskt.m.passive %>% 
  mutate(vskt=1)

joint.post <- bind_rows(fA.nnd.passive, vskt.m.passive)

joint.post <- joint.post %>% 
  mutate(vskt=factor(vskt,ordered = F))

### Pension in 2013
fused_rente13.plot <- mydensplot.post(joint.post, "rente_total_2013", xname = "Rentenhöhe 2013 in €", lmts =c(1, 30000))
ggsave("furente13.pdf")


#### Performing Kolmogorov-Smirnoff-Test for equality of Distributions

ks.test(fA.nnd.passive$rente_total_2013, vskt.m.passive$rente_total_2013, alternative = "two.sided")
        

### does not really work because there are so many zero values




#### Income 2000

fused_inc00.plot <- mydensplot.post(joint.post, "brutto_zens_2000", xname ="Einkommen 2000 in €", lmts =  c(1, 55000))
ggsave("fuincpassive.pdf")


ks.test(fA.nnd.passive$brutto_zens_2000, vskt.m.passive$brutto_zens_2000, alternative = "two.sided")
### does not really work because there are so many zero values


# Calculate the common x and y range 
(xrng = range(c(fA.nnd.passive$rente_total_2013, vskt.m.passive$rente_total_2013)))
(yrng = range(c(fA.nnd.passive$gbja, vskt.m.passive$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$brutto_zens_2011, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.m.active$brutto_zens_2011, vskt.m.active$gbja, lims=c(xrng, yrng), n=200)

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
names(diff12.m) = c("Rentenhöhe","Geburtsjahr","z")

# Plot difference between densities
ggplot(diff12.m, aes(Rentenhöhe, Geburtsjahr, z=z, fill=z)) +
  geom_tile() +
  theme_classic() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
  scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  ggtitle("2013-Renten Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("differencepassive.pdf")


##### save as .RDA and .dta
load("fused.RDA")
load("fusedpassive.RDA")

fA.nnd <- fA.nnd %>% 
  mutate(age_g = factor(age_g)) %>% 
  mutate(educ_cat = as.character(educ_cat)) %>% 
  mutate(em_rente = as.character(em_rente)) %>% 
  mutate(spez_scheidung = as.character(spez_scheidung)) %>% 
  mutate(rente_total_2012=0)

fA.nnd.passive <- fA.nnd.passive %>% 
  mutate(age_g = as.character(age_g)) %>% 
  mutate(educ_cat = as.character(educ_cat)) %>% 
  mutate(em_rente = as.character(em_rente)) %>% 
  mutate(spez_scheidung = as.character(spez_scheidung))

joint.men <- bind_rows(fA.nnd, fA.nnd.passive)
joint.men <- joint.men %>% 
  mutate(sex=0)

save(joint.men, file="joint_men.RDA")

write.dta(joint.men, file = "joint_men.dta")




