######## Survey Data Fusion ############
rm(list = ls())

knitr::opts_chunk$set(echo = TRUE, cache = T)
source("packages.R")

#setwd(path) in path.R
source(".path.R")

####### Initiation ##########

#### Functions

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


#### Import SOEP - full active population #####

soep.women.active <- import(paste(path, "soep_2012_f_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.women.active <- clear.labels(soep.women.active)

head(soep.women.active)


#### Import VSKT active  ######

vskt.w.active <- import(paste(path, "vskt_f_active.dta" , sep = "/"), setclass = "data.table")
vskt.w.active <- clear.labels(vskt.w.active)

head(vskt.w.active)




### Now check for imprtant variables:

#### Define common variables ####

### density/histogram plots for X.vars --> marginal/joint density should be the same!

# Variables available in both datasets

(X.vars <- intersect(names(soep.women.active), names(vskt.w.active)))

soep.women.active <- soep.women.active %>% 
  mutate(soep=1)

vskt.w.active <- vskt.w.active %>% 
  mutate(soep=0)

joint <- bind_rows(soep.women.active, vskt.w.active)


anwartschaftenw.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=rentenanspruch_2012, fill = factor(soep)), alpha = 0.4) 
anwartschaftenw.plot <- anwartschaftenw.plot + xlab("Rentenawartschaften in €") + ylab("Dichte")
anwartschaftenw.plot <- anwartschaftenw.plot + ggtitle("Vergleich der Rentenanwartschaften in 2012 in SOEP und VSKT") +theme_minimal() 
anwartschaftenw.plot <- anwartschaftenw.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

anwartschaftenw.plot

ggsave("anwartschaftenw.pdf")

birthyearw.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=gbja, fill = factor(soep)), alpha = 0.4) 
birthyearw.plot <- birthyearw.plot + xlab("Geburtsjahr") + ylab("Dichte")
birthyearw.plot <- birthyearw.plot + ggtitle("Vergleich der Geburtsjahrgänge in SOEP und VSKT") +theme_minimal() 
birthyearw.plot <- birthyearw.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

birthyearw.plot

ggsave("birthyearw.pdf")


incomew.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=brutto_zens_2012, fill = factor(soep)), alpha = 0.4) 
incomew.plot <- incomew.plot + xlab("Arbeitseinkommen in €") + ylab("Dichte")
incomew.plot <- incomew.plot + ggtitle("Vergleich der 2012 Einkommen in SOEP und VSKT") +theme_minimal() 
incomew.plot <- incomew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

incomew.plot

ggsave("incomew.pdf")


worktimew.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=exp_arbeit_20_bis2012, fill = factor(soep)), alpha = 0.4) 
worktimew.plot <- worktimew.plot + xlab("Arbeitserfahrung in Monaten") + ylab("Dichte")
worktimew.plot <- worktimew.plot + ggtitle("Vergleich der Arbeitserfahrung in SOEP und VSKT") +theme_minimal() 
worktimew.plot <- worktimew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

worktimew.plot

ggsave("wtimew.pdf")

unempbenw.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=alg_j_2012, fill = factor(soep)), alpha = 0.4) 
unempbenw.plot <- unempbenw.plot + xlab("Arbeitslosengeld in €") + ylab("Dichte")
unempbenw.plot <- unempbenw.plot + ggtitle("Vergleich Arbeitslosengeld in SOEP und VSKT") +theme_minimal() 
unempbenw.plot <- unempbenw.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"), 
                                                   values = c("gold","turquoise4"))
unempbenw.plot <- unempbenw.plot + scale_x_continuous(limits = c(10, 18000))
unempbenw.plot 

ggsave("unempbenw.pdf") 


unempexpw.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=exp_al_20_bis2012, fill = factor(soep)), alpha = 0.4) 
unempexpw.plot <- unempexpw.plot + xlab("Monate in Arbeitslosigkeit") + ylab("Dichte")
unempexpw.plot <- unempexpw.plot + ggtitle("Vergleich der Arbeitslosigkeitsdauer in SOEP und VSKT") +theme_minimal() 
unempexpw.plot <- unempexpw.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),
                                                   values = c("gold","turquoise4"))
unempexpw.plot <- unempexpw.plot + scale_x_continuous(limits = c(1, 200)) 

unempexpw.plot


ggsave("unemptimew.pdf")


p <- cowplot::plot_grid(birthyearw.plot, anwartschaftenw.plot)
ggsave("grid1w.pdf", p)


q <- cowplot::plot_grid(unempexpw.plot, unempbenw.plot, worktimew.plot, incomew.plot, ncol=2, nrow=2)
ggsave("grid2w.pdf", q)

## here a randomForest evaluation of the most important matching variables might be fruitful!
## In general one should choose those X_m that have jointly the highest correlation (reduction in Variance)
## w.r.t.to the Variables only available in Y and Z.
## Namely: Variable of interest in SOEP: educ_cat 
##         Variable of interest in VSKT: not that easy---> full income cycle...

##

### first denote as factor: Doing a quick Regression Model for Educ to see which Variables are important:

soep.women.active <- 
  soep.women.active %>% 
  mutate(educ_cat = factor(educ_cat, ordered = TRUE)) 

str(soep.women.active$educ_cat)
### standardizing coefficients

modelformula <- educ~ brutto_zens_2012+age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012
all.vars(modelformula)
seop.stdz <- lapply(soep.women.active[, all.vars(modelformula)], scale) 

lm <- lm(educ~ brutto_zens_2012+age_g+ spez_scheidung + rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
         data=seop.stdz)
summary(lm)

### Income, Age and Working experience show the highest effect
spearman2(educ~ brutto_zens_2012+age_g+ spez_scheidung + rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=soep.women.active)

## doing the same for VSKT

spearman2(brutto_zens_2013~ brutto_zens_2012 + age_g+spez_scheidung+rentenanspruch_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012+alg_j_2012,
          p=2, data=vskt.w.active)

### brutto_zens_2012, Rentenanspruch has high impact as well as exp_arbeit_20_bis2012


# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#
################################### Data Fusion ###################################################
# ------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------#

## Perform nearest neighbor distance hot deck with maximum norm as distance measure with 
## rank option = TRUE such that scale differences are accounted for 

soep.vars <- setdiff(names(soep.women.active), names(vskt.w.active)) # available just in SOEP

vskt.vars <- setdiff(names(vskt.w.active), names(soep.women.active)) # available just in VSKT

(X.mtc <- c("brutto_zens_2012", "rentenanspruch_2012", "exp_arbeit_20_bis2012"))
(X2.mtc <- c("rentenanspruch_2012", "exp_arbeit_20_bis2012"))


### donation class define as factor first:

soep.women.active <- 
  soep.women.active %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung))

str(soep.women.active$age_g)
str(soep.women.active$spez_scheidung)


vskt.w.active <- 
  vskt.w.active %>% 
  mutate(age_g = factor(age_g, ordered = TRUE)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung))

str(vskt.w.active$age_g)
str(vskt.w.active$spez_scheidung)


### nearest neighbor distance matching using the L^{inf} norm and lpSolv constrained matching algorythm
group.v <- c("age_g","spez_scheidung")

nnd.hd <- NND.hotdeck(data.rec=soep.women.active, data.don=vskt.w.active,
                      match.vars=X.mtc, 
                      don.class = group.v,
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)

## With fewer Matching Variables 

#nnd.hd <- NND.hotdeck(data.rec=soep.women.active, data.don=vskt.w.active,
#                     match.vars=X.mtc, 
#                    don.class = "age_g",
#                   dist.fun = "minimax",
#                  rank = TRUE,
#                 constrained = TRUE,
#                constr.alg = "lpSolve",
#               k=5)

#nnd.hd2 <- NND.hotdeck(data.rec=soep.women.active, data.don=vskt.w.active,
#                     match.vars=X.mtc, 
#                    don.class = "age_g",
#                   dist.fun = "mahalanobis",
#                  constrained = TRUE,
#                 constr.alg = "lpSolve",
#                k=5)


#### compare distances via boxplot --> geom_boxplot in ggplot

fA.nnd <- create.fused(data.rec=soep.women.active, data.don=vskt.w.active,
                       mtc.ids=nnd.hd$mtc.ids,
                       z.vars=vskt.vars)


summary(nnd.hd$dist.rd)

### make boxplot with dist.rd
pdf('boxplotw.pdf')
box <- boxplot(nnd.hd$dist.rd, xlab="Supremumsnorm") 
title("Boxplot of distances between SOEP and VSKT cases")
dev.off()

save(fA.nnd, file = "fusedw.RDA")
save(nnd.hd, file = "matchedw.RDA")

##########################################################
##########################################################
##########################################################
### diagnostics####
load("fusedw.RDA")
load("matchedw.RDA")
### prefereably put densities of VSKT and fused data set into one grid in order to compare
### of course it now makes sense to compare the non matching variables!

# main focus! after fusion the distributional structure should have prevailed!

fA.nnd <- fA.nnd %>% 
  mutate(vskt=0)

vskt.w.active <- vskt.w.active %>% 
  mutate(vskt=1)

joint2 <- bind_rows(fA.nnd, vskt.w.active)


fused_inc89w.plot <- joint2 %>% 
  ggplot() + 
  geom_density(aes(x=brutto_zens_1998, fill = factor(vskt)), alpha = 0.4) 
fused_inc89w.plot <- fused_inc89w.plot + xlab("Einkommen in €") + ylab("Dichte")
fused_inc89w.plot <- fused_inc89w.plot + ggtitle("Vergleich der Einkommen in 1989 in fused Data Set und VSKT") +theme_minimal() 
fused_inc89w.plot <- fused_inc89w.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))
fused_inc89w.plot <- fused_inc89w.plot + scale_x_continuous(limits = c(1, 60000))

fused_inc89w.plot

### Kolmogorov-Smirnoff-Test tells us that Distributions are identical

ks.active <- ks.test(fA.nnd$rente_total_2013, vskt.w.active$rente_total_2013, alternative = "two.sided")



ggsave("fuincome89w.pdf")

fused_alg89w.plot <- joint2 %>% 
  ggplot() + 
  geom_density(aes(x=alg_j_1989, fill = factor(vskt)), alpha = 0.4) 
fused_alg89w.plot <- fused_alg89w.plot + xlab("Arbeitslosengeld in €") + ylab("Dichte")
fused_alg89w.plot <- fused_alg89w.plot + ggtitle("Vergleich des Arbeitslosengeldes in 1989 in fused Data Set und VSKT") +theme_minimal() 
fused_alg89w.plot <- fused_alg89w.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))
fused_alg89w.plot <- fused_alg89w.plot + scale_x_continuous(limits = c(1, 18000))
fused_alg89w.plot

ggsave("fualg89w.pdf")

### An so on ask Timm which ones!

### Put them in a grid!

### Also beamer template for presentation DIW...

# Calculate the common x and y range 
(xrng = range(c(fA.nnd$brutto_zens_2011, vskt.w.active$brutto_zens_2011)))
(yrng = range(c(fA.nnd$gbja, vskt.w.active$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$brutto_zens_2011, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.w.active$brutto_zens_2011, vskt.w.active$gbja, lims=c(xrng, yrng), n=200)

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
  ggtitle("Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("differencew.pdf")

#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

##### Matching passive female population ######

#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

## I would rather write a function that automates the Nearest Neighbor Random Hot Deck Matching
## However: Aint't nobody got time for this 




#### Import SOEP passive #####

soep.w.passive <- import(paste(path, "soep_2012_f_passiv.dta" , sep = "/"), setclass = "data.table")
soep.w.passive <- clear.labels(soep.w.passive)

head(soep.w.passive)

#### Import VSKT ######

vskt.w.passive <- import(paste(path, "vskt_f_passiv.dta" , sep = "/"), setclass = "data.table")
vskt.w.passive <- clear.labels(vskt.w.passive)

head(vskt.w.passive)


### Now same as above! Also write function for this!


# Variables available in both datasets

(xp.vars <- intersect(names(soep.w.passive), names(vskt.w.passive)))

soep.w.passive <- soep.w.passive %>% 
  mutate(soep=1)

vskt.w.passive <- vskt.w.passive %>% 
  mutate(soep=0)

joint.p <- bind_rows(soep.w.passive, vskt.w.passive)

joint.p2 <- joint.p %>% 
  subset(em_rente==0) %>% 
  select(-em_rente)

rentew.plot <- joint.p2 %>% 
  ggplot() + 
  geom_density(aes(x=rente_total_2012, fill = factor(soep)), alpha = 0.4) 
rentew.plot <- rentew.plot + xlab("Rentenhöhe in €") + ylab("Dichte")
rentew.plot <- rentew.plot + ggtitle("Vergleich der (Alters)Rentenhöhe in 2012 in SOEP und VSKT") +theme_minimal() 
rentew.plot <- rentew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))
rentew.plot <- rentew.plot + scale_x_continuous(limits = c(1, 65000))

rentew.plot

ggsave("rentew.pdf")

agew.plot <- joint.p2 %>% 
  ggplot() + 
  geom_density(aes(x=gbja, fill = factor(soep)), alpha = 0.4) 
agew.plot <- agew.plot + xlab("Geburtsjahr") + ylab("Dichte")
agew.plot <- agew.plot + ggtitle("Vergleich der Geburtsjahrgänge der Rentner (inkl. EM-Rente) in SOEP und VSKT") +theme_minimal() 
agew.plot <- agew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

agew.plot

ggsave("agew.pdf")


worktimerentew.plot <- joint.p %>% 
  ggplot() + 
  geom_density(aes(x=exp_arbeit_20_bis2012, fill = factor(soep)), alpha = 0.4) 
worktimerentew.plot <- worktimerentew.plot + xlab("Arbeitserfahrung in Monaten der Rentner (ikl. EM-Rente") + ylab("Dichte")
worktimerentew.plot <- worktimerentew.plot + ggtitle("Vergleich der Arbeitserfahrung in SOEP und VSKT") +theme_minimal() 
worktimerentew.plot <- worktimerentew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

worktimerentew.plot

ggsave("wtimerentew.pdf")



unempexprentew.plot <- joint.p %>% 
  ggplot() + 
  geom_density(aes(x=exp_al_20_bis2012, fill = factor(soep)), alpha = 0.4) 
unempexprentew.plot <- unempexprentew.plot + xlab("Monate in Arbeitslosigkeit") + ylab("Dichte")
unempexprentew.plot <- unempexprentew.plot + ggtitle("Vergleich der Arbeitslosigkeitsdauer der Rentner in SOEP und VSKT") +theme_minimal() 
unempexprentew.plot <- unempexprentew.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),
                                                             values = c("gold","turquoise4"))
unempexprentew.plot <- unempexprentew.plot + scale_x_continuous(limits = c(1, 200)) 

unempexprentew.plot


ggsave("unemptimerentew.pdf")


pp <- cowplot::plot_grid(agew.plot, rentew.plot, unempexprentew.plot, worktimerentew.plot, nrow = 2, ncol = 2)
ggsave("grid3w.pdf", pp)



####### Identifying Matching Variables in Pension Sample ######


### standardizing coefficients

modelformulap <- educ~ age+spez_scheidung+rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012
all.vars(modelformulap)
seopp.stdz <- lapply(soep.w.passive[, all.vars(modelformulap)], scale) 

lm.p <- lm(educ~ age+ spez_scheidung + rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012,
           data=seopp.stdz)
summary(lm.p)

### Experience Arbeit und Experience AL sowie rente_total_2012
spearman2(educ~ age+ spez_scheidung + rente_total_2012+exp_arbeit_20_bis2012+exp_al_20_bis2012, p=2,
          data=seopp.stdz)

## doing the same for VSKT

spearman2(rente_total_2013~ rente_total_2012 + age_g + spez_scheidung + exp_arbeit_20_bis2012 + exp_al_20_bis2012,
          p=2, data=vskt.w.passive)

### here merely age and rente_total_2012


##### deciding for age and rente_total_2012 as matching variables
##### further using em_rente as group variable 



## Perform nearest neighbor distance hot deck with maximum norm as distance measure with 
## rank option = TRUE such that scale differences are accounted for 

soep.passive.vars <- setdiff(names(soep.w.passive), names(vskt.w.passive)) # available just in SOEP

vskt.passive.vars <- setdiff(names(vskt.w.passive), names(soep.w.passive)) # available just in VSKT

(Xp.mtc <- c("rente_total_2012", "exp_arbeit_20_bis2012", "age"))
(Xp2.mtc <- c("rente_total_2012", "age"))


### donation class em_rente define as factor first:

soep.w.passive <-  soep.w.passive %>% 
  mutate(em_rente = factor(em_rente)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung))


str(soep.w.passive$em_rente, soep.w.passive$spez_scheidung)


vskt.w.passive <- vskt.w.passive %>% 
  mutate(em_rente = factor(em_rente)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung))

str(vskt.w.passive$em_rente, vskt.w.passive$spez_scheidung)


### nearest neighbor distance matching using the L^{inf} norm and lpSolv constrained matching algorythm
group.v2 <- c("spez_scheidung")

nnd.hd.passive <- NND.hotdeck(data.rec=soep.w.passive, data.don=vskt.w.passive,
                              match.vars=Xp.mtc, 
                              don.class = group.v2,
                              dist.fun = "minimax",
                              rank = TRUE,
                              constrained = TRUE,
                              constr.alg = "lpSolve",
                              k=5)

fA.nnd.passive <- create.fused(data.rec=soep.w.passive, data.don=vskt.w.passive,
                               mtc.ids=nnd.hd.passive$mtc.ids,
                               z.vars=vskt.passive.vars)


summary(nnd.hd.passive$dist.rd)

### make boxplot with dist.rd
pdf('boxplotpassivew.pdf')
box <- boxplot(nnd.hd.passive$dist.rd, xlab="Supremumsnorm") 
title("Boxplot der Supremumsnormdistanzen zwischen SOEP and VSKT Rentnern")
dev.off()

save(fA.nnd.passive, file = "fusedpassivew.RDA")
save(nnd.hd.passive, file = "matchedpassivew.RDA")


### diagnostics for passive population ####
load("fusedpassivew.RDA")
load("matchedpassivew.RDA")
### prefereably put densities of VSKT and fused data set into one grid in order to compare
### of course it now makes sense to compare the non matching variables!

# main focus! after fusion the distributional structure should have prevailed!

fA.nnd.passive <- fA.nnd.passive %>% 
  mutate(vskt=0) %>% 
  mutate(spez_scheidung = factor(spez_scheidung))

vskt.w.passive <- vskt.w.passive %>% 
  mutate(vskt=1)



joint.post <- bind_rows(fA.nnd.passive, vskt.w.passive)



fused_rente13w.plot <- joint.post %>% 
  ggplot() + 
  geom_density(aes(x=rente_total_2013, fill = factor(vskt)), alpha = 0.4) 
fused_rente13w.plot <- fused_rente13w.plot + xlab("Rentenhöhe in €") + ylab("Dichte")
fused_rente13w.plot <- fused_rente13w.plot + ggtitle("Vergleich der Renten in 2013 in fused Data Set und VSKT") +theme_minimal() 
fused_rente13w.plot <- fused_rente13w.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))
fused_rente13w.plot <- fused_rente13w.plot + scale_x_continuous(limits = c(1, 30000))

fused_rente13w.plot

ggsave("furente13w.pdf")


#### Performing Kolmogorov-Smirnoff-Test for equality of Distributions

ks <- ks.test(fA.nnd.passive$rente_total_2013, vskt.w.passive$rente_total_2013, alternative = "two.sided")


### does not really work because there are so many zero values
#####




fused_inc00w.plot <- joint.post %>% 
  ggplot() + 
  geom_density(aes(x=brutto_zens_2000, fill = factor(vskt)), alpha = 0.4) 
fused_inc00w.plot <- fused_inc00w.plot + xlab("Einkommen in €") + ylab("Dichte")
fused_inc00w.plot <- fused_inc00w.plot + ggtitle("Vergleich der Einkommen in 2000 fused Data Set und VSKT") +theme_minimal() 
fused_inc00w.plot <- fused_inc00w.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))
fused_inc00w.plot <- fused_inc00w.plot + scale_x_continuous(limits = c(1, 67200))
fused_inc00w.plot
### does not really work because there are so many zero values

ggsave("fuincpassivew.pdf")


ks2 <- ks.test(fA.nnd.passive$brutto_zens_2000, vskt.w.passive$brutto_zens_2000, alternative = "two.sided")



### An so on ask Timm which ones!

### Put them in a grid!

### Also beamer template for presentation DIW...

# Calculate the common x and y range 
(xrng = range(c(fA.nnd.passive$rente_total_2013, vskt.w.passive$rente_total_2013)))
(yrng = range(c(fA.nnd.passive$gbja, vskt.w.passive$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$brutto_zens_2011, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.w.active$brutto_zens_2011, vskt.w.active$gbja, lims=c(xrng, yrng), n=200)

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
  ggtitle("Contour-Differenzen zwischen Fused Data und VSKT: Rentenhöhe 2013")

ggsave("differencepassivew.pdf")



####### Load all Fused Datasets and append them! ########

# Then save them as .RDA and .dta

load("fusedw.RDA")
load("fusedpassivew.RDA")


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
  


joint.women <- bind_rows(fA.nnd, fA.nnd.passive)

save(joint.women, file="joint_women.RDA")

write.dta(joint.women, file = "joint_women.dta")


### append both

load("joint_women.RDA")
load("joint_men.RDA")

joint.total <-  bind_rows(joint.women, joint.men)

save(joint.total, file="joint_total.RDA")

write.dta(joint.total, file = "joint_total.dta")



