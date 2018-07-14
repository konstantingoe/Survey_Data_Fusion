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

soep.men.active <- import(paste(path, "soep_2012_m_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.men.active <- clear.labels(soep.men.active)

head(soep.men.active)


#### Import VSKT active  ######

vskt.m.active <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
vskt.m.active <- clear.labels(vskt.m.active)

head(vskt.m.active)




### Now check for imprtant variables:

#### Define common variables ####

### density/histogram plots for X.vars --> marginal/joint density should be the same!

# Variables available in both datasets

(X.vars <- intersect(names(soep.men.active), names(vskt.m.active)))

soep.men.active <- soep.men.active %>% 
  mutate(soep=1)

vskt.m.active <- vskt.m.active %>% 
  mutate(soep=0)

joint <- bind_rows(soep.men.active, vskt.m.active)


anwartschaften.plot <- joint %>% 
    ggplot() + 
    geom_density(aes(x=rentenanspruch_2012, fill = factor(soep)), alpha = 0.4) 
anwartschaften.plot <- anwartschaften.plot + xlab("Rentenawartschaften in €") + ylab("Dichte")
anwartschaften.plot <- anwartschaften.plot + ggtitle("Vergleich der Rentenanwartschaften in 2012 in SOEP und VSKT") +theme_minimal() 
anwartschaften.plot <- anwartschaften.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

anwartschaften.plot
    
ggsave("anwartschaften.pdf")

birthyear.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=gbja, fill = factor(soep)), alpha = 0.4) 
birthyear.plot <- birthyear.plot + xlab("Geburtsjahr") + ylab("Dichte")
birthyear.plot <- birthyear.plot + ggtitle("Vergleich der Geburtsjahrgänge in SOEP und VSKT") +theme_minimal() 
birthyear.plot <- birthyear.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

birthyear.plot

ggsave("birthyear.pdf")


income.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=brutto_zens_2012, fill = factor(soep)), alpha = 0.4) 
income.plot <- income.plot + xlab("Arbeitseinkommen in €") + ylab("Dichte")
income.plot <- income.plot + ggtitle("Vergleich der 2012 Einkommen in SOEP und VSKT") +theme_minimal() 
income.plot <- income.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

income.plot

ggsave("income.pdf")


worktime.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=exp_arbeit_20_bis2012, fill = factor(soep)), alpha = 0.4) 
worktime.plot <- worktime.plot + xlab("Arbeitserfahrung in Monaten") + ylab("Dichte")
worktime.plot <- worktime.plot + ggtitle("Vergleich der Arbeitserfahrung in SOEP und VSKT") +theme_minimal() 
worktime.plot <- worktime.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

worktime.plot

ggsave("wtime.pdf")

unempben.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=alg_j_2012, fill = factor(soep)), alpha = 0.4) 
unempben.plot <- unempben.plot + xlab("Arbeitslosengeld in €") + ylab("Dichte")
unempben.plot <- unempben.plot + ggtitle("Vergleich Arbeitslosengeld in SOEP und VSKT") +theme_minimal() 
unempben.plot <- unempben.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"), 
                                                   values = c("gold","turquoise4"))
unempben.plot <- unempben.plot + scale_x_continuous(limits = c(10, 18000))
unempben.plot 

ggsave("unempben.pdf") 
  

unempexp.plot <- joint %>% 
  ggplot() + 
  geom_density(aes(x=exp_al_20_bis2012, fill = factor(soep)), alpha = 0.4) 
unempexp.plot <- unempexp.plot + xlab("Monate in Arbeitslosigkeit") + ylab("Dichte")
unempexp.plot <- unempexp.plot + ggtitle("Vergleich der Arbeitslosigkeitsdauer in SOEP und VSKT") +theme_minimal() 
unempexp.plot <- unempexp.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),
                                                   values = c("gold","turquoise4"))
unempexp.plot <- unempexp.plot + scale_x_continuous(limits = c(1, 200)) 

unempexp.plot


ggsave("unemptime.pdf")


p <- cowplot::plot_grid(birthyear.plot, anwartschaften.plot)
ggsave("grid1.pdf", p)


q <- cowplot::plot_grid(unempexp.plot, unempben.plot, worktime.plot, income.plot, ncol=2, nrow=2)
ggsave("grid2.pdf", q)

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


fused_inc89.plot <- joint2 %>% 
  ggplot() + 
  geom_density(aes(x=brutto_zens_1998, fill = factor(vskt)), alpha = 0.4) 
fused_inc89.plot <- fused_inc89.plot + xlab("Einkommen in €") + ylab("Dichte")
fused_inc89.plot <- fused_inc89.plot + ggtitle("Vergleich der Einkommen in 1989 in fused Data Set und VSKT") +theme_minimal() 
fused_inc89.plot <- fused_inc89.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))

fused_inc89.plot

ggsave("fuincome89.pdf")

fused_alg89.plot <- joint2 %>% 
  ggplot() + 
  geom_density(aes(x=alg_j_1989, fill = factor(vskt)), alpha = 0.4) 
fused_alg89.plot <- fused_alg89.plot + xlab("Arbeitslosengeld in €") + ylab("Dichte")
fused_alg89.plot <- fused_alg89.plot + ggtitle("Vergleich des Arbeitslosengeldes in 1989 in fused Data Set und VSKT") +theme_minimal() 
fused_alg89.plot <- fused_alg89.plot + scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))
fused_alg89.plot <- fused_alg89.plot + scale_x_continuous(limits = c(10, 18000))
fused_alg89.plot

ggsave("fualg89.pdf")

### An so on ask Timm which ones!

### Put them in a grid!

### Also beamer template for presentation DIW...

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
  ggtitle("Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("difference.pdf")


##### Matching passive male population ######

## I would rather write a function that automates the Nearest Neighbor Random Hot Deck Matching
## However: Aint't nobody got time for this 




#### Import SOEP passive #####

soep.m.passive <- import(paste(path, "soep_2012_m_passiv.dta" , sep = "/"), setclass = "data.table")
soep.m.passive <- clear.labels(soep.m.passive)

head(soep.m.passive)

#### Import VSKT ######

vskt.m.passive <- import(paste(path, "vskt_m_passiv.dta" , sep = "/"), setclass = "data.table")
vskt.m.passive <- clear.labels(vskt.m.passive)

head(vskt.m.passive)


### Now same as above! Also write function for this!


# Variables available in both datasets

(xp.vars <- intersect(names(soep.m.passive), names(vskt.m.passive)))

soep.m.passive <- soep.m.passive %>% 
  mutate(soep=1)

vskt.m.passive <- vskt.m.passive %>% 
  mutate(soep=0)

joint.p <- bind_rows(soep.m.passive, vskt.m.passive)

joint.p2 <- joint.p %>% 
  subset(em_rente==0) %>% 
  select(-em_rente)

rente.plot <- joint.p2 %>% 
  ggplot() + 
  geom_density(aes(x=rente_total_2012, fill = factor(soep)), alpha = 0.4) 
rente.plot <- rente.plot + xlab("Rentenhöhe in €") + ylab("Dichte")
rente.plot <- rente.plot + ggtitle("Vergleich der (Alters)Rentenhöhe in 2012 in SOEP und VSKT") +theme_minimal() 
rente.plot <- rente.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))
rente.plot <- rente.plot + scale_x_continuous(limits = c(1, 50000))

rente.plot

ggsave("rente.pdf")

age.plot <- joint.p2 %>% 
  ggplot() + 
  geom_density(aes(x=gbja, fill = factor(soep)), alpha = 0.4) 
age.plot <- age.plot + xlab("Geburtsjahr") + ylab("Dichte")
age.plot <- age.plot + ggtitle("Vergleich der Geburtsjahrgänge der Rentner (inkl. EM-Rente) in SOEP und VSKT") +theme_minimal() 
age.plot <- age.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

age.plot

ggsave("age.pdf")


worktimerente.plot <- joint.p %>% 
  ggplot() + 
  geom_density(aes(x=exp_arbeit_20_bis2012, fill = factor(soep)), alpha = 0.4) 
worktimerente.plot <- worktimerente.plot + xlab("Arbeitserfahrung in Monaten der Rentner (ikl. EM-Rente") + ylab("Dichte")
worktimerente.plot <- worktimerente.plot + ggtitle("Vergleich der Arbeitserfahrung in SOEP und VSKT") +theme_minimal() 
worktimerente.plot <- worktimerente.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4"))

worktimerente.plot

ggsave("wtimerente.pdf")



unempexprente.plot <- joint.p %>% 
  ggplot() + 
  geom_density(aes(x=exp_al_20_bis2012, fill = factor(soep)), alpha = 0.4) 
unempexprente.plot <- unempexprente.plot + xlab("Monate in Arbeitslosigkeit") + ylab("Dichte")
unempexprente.plot <- unempexprente.plot + ggtitle("Vergleich der Arbeitslosigkeitsdauer der Rentner in SOEP und VSKT") +theme_minimal() 
unempexprente.plot <- unempexprente.plot + scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),
                                                   values = c("gold","turquoise4"))
unempexprente.plot <- unempexprente.plot + scale_x_continuous(limits = c(1, 200)) 

unempexprente.plot


ggsave("unemptimerente.pdf")


pp <- cowplot::plot_grid(age.plot, rente.plot, unempexprente.plot, worktimerente.plot, nrow = 2, ncol = 2)
ggsave("grid3.pdf", p)



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

(X.mtc <- c("rente_total_2012", "exp_arbeit_20_bis2012", "age"))
(X2.mtc <- c("rente_total_2012", "age"))


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


nnd.hd <- NND.hotdeck(data.rec=soep.m.passive, data.don=vskt.m.passive,
                      match.vars=X.mtc, 
                      don.class = "em_rente",
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)
