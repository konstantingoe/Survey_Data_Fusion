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

#soep <- import(paste(path, "soep_2012_m_genau.dta" , sep = "/"), setclass = "data.table")

#soep <- clear.labels(soep)

#head(soep)

#### Import VSKT ######

vskt <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
vskt <- clear.labels(vskt)

head(vskt)

### bdp10301==1
### problem...we don't have the same population --> apparently there is selection regarding age for those who
### give reliable information on their pension entitlements



#### Clearly, setting bdp10301 to 1, meaning exact pension entitlements created a subset of 
#### observations that is not compatible with the VSKT population!

### Checking for full active population ####

#### Import SOEP - full active population #####

soep.men.full <- import(paste(path, "soep_2012_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.men.full <- clear.labels(soep.men.full)

head(soep.men.full)


### Now check for imprtant variables:

#### Define common variables ####

### density/histogram plots for X.vars --> marginal/joint density should be the same!

# Variables available in both datasets

(X.vars <- intersect(names(soep.men.full), names(vskt)))

soep.men.full <- soep.men.full %>% 
  mutate(soep=1)

vskt <- vskt %>% 
  mutate(soep=0)

joint <- bind_rows(soep.men.full, vskt)#


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


##
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

vskt <- vskt %>% 
  mutate(vskt=1)

joint2 <- bind_rows(fA.nnd, vskt)


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
(xrng = range(c(fA.nnd$brutto_zens_2011, vskt$brutto_zens_2011)))
(yrng = range(c(fA.nnd$gbja, vskt$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$brutto_zens_2011, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt$brutto_zens_2011, vskt$gbja, lims=c(xrng, yrng), n=200)

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

