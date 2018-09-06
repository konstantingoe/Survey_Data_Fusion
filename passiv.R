rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")


soep.mp <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")

vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")

(X.vars <- intersect(names(soep.mp), names(vskt.mp)))

soep.mp <- soep.mp %>% 
  mutate(soep=1) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(wgt=1500)

vskt.mp <- vskt.mp %>% 
  mutate(soep=0) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(wgt=weight)


joint <- bind_rows(soep.mp, vskt.mp)
joint <- joint %>% 
  mutate(soep = factor(soep, ordered = F))


(birthplot <- mydensplot(joint, "gbja", xname = "Geburtskohorte"))
ggsave("geburtsjahr_mp.pdf")

(birthplot.w <- mydensplot(joint, "gbja", xname = "Geburtskohorte (gewichtet)", weight = "wgt"))
ggsave("geburtsjahr_mp_weighted.pdf")


(birthdist <- mydistplot(joint, "gbja", xname = "Geburtskohorte")) 
ggsave("geburtsjahr_dist_mp.pdf")

plot.grdgbja <- cowplot::plot_grid(birthplot, birthplot.w, birthdist, ncol=2, nrow=2)
ggsave("grid_passiv1.pdf", plot.grdgbja)


(kstest.gbja <- ks.test(soep.mp$gbja, vskt.mp$gbja, alternative = "two.sided"))



(rentenplot <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_mp.pdf")

(rentenplot.w <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in € (gewichtet)", weight="wgt"))
ggsave("rente_mp_weighted.pdf")

(rentendist <- mydistplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_dist.pdf")

(kstest.rente <- ks.test(soep.mp$rente_2015_gesamt, vskt.mp$rente_2015_gesamt, alternative = "two.sided"))

plot.grdrente <- cowplot::plot_grid(rentenplot, rentenplot.w, rentendist, ncol=2, nrow=2)
ggsave("grid_passiv2.pdf", plot.grdrente)


#df.res <- data.frame(D = c(kstest.gbja$statistic, kstest.rente$statistic),
#                     `p value` = c(kstest.gbja$p.value, kstest.rente$p.value))
#stargazer(df.res)

#### checking multivariate correlation structure (usually)

### It might be worth adding a income indicator sth like 
### last observed income

### Pension, Birthyear and Working experience show the highest effect (sex as donation class)
spearman2(education~ sex+gbja+ exp_arbeit+ rente_2015_gesamt,
          p=2, data=soep.mp)

## doing the same for VSKT

spearman2(brutto_zens_1998~ sex+gbja+ exp_arbeit +rente_2015_gesamt,
          p=2, data=vskt.mp)


vskt.vars <- setdiff(names(vskt.mp), names(soep.mp)) # available just in VSKT
(Xp.mtc <- c("rente_2015_gesamt", "exp_arbeit"))
(Xp.mtc2 <- c("rente_2015_gesamt", "exp_arbeit", "gbja"))
(donclass1 <- c("gbja_cat","sex"))
(donclass2 <- "sex")



nnd.hd <- NND.hotdeck(data.rec=soep.mp, data.don=vskt.mp,
                      match.vars=Xp.mtc2, 
                      don.class = donclass2,
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)

fA.nnd <- create.fused(data.rec=soep.mp, data.don=vskt.mp,
                       mtc.ids=nnd.hd$mtc.ids,
                       z.vars=vskt.vars)


save(nnd.hd, file = "match_passiv_gesamt.RDA")
save(fA.nnd, file = "fused_passiv_gesamt.RDA")

##########################################################
##########################################################
##########################################################
                  ### diagnostics####
##########################################################
##########################################################
##########################################################


#load("match_passiv_gesamt.RDA")
#load("fused_passiv_gesamt.RDA")


summary(nnd.hd$dist.rd)

### make boxplot with dist.rd
pdf('boxplot.pdf')
box <- boxplot(nnd.hd$dist.rd, xlab="Supremumsnorm") 
title("Boxplot of distances between SOEP and VSKT cases")
dev.off()


fA.nnd <- fA.nnd %>% 
  mutate(vskt=0)

vskt.mp <- vskt.mp %>% 
  mutate(vskt=1)

joint.post <- bind_rows(fA.nnd, vskt.mp)

joint.post <- joint.post %>% 
  mutate(vskt=factor(vskt,ordered = F))

### Einkommen in 89
(fused_income89.plot <- mydensplot.post(joint.post, "rente_j_2015", xname = "Rentenhöhe 2015 in €", lmts =c(1, 40000)))
ggsave("fuincome_passive15.pdf")

(fused_income89.distplot <- mydistplot.post(joint.post, "rente_j_2015", xname = "Rentenhöhe 2015 in €"))
ggsave("fuincome_passive15dist.pdf")

ks.test(fA.nnd$rente_j_2015, vskt.mp$rente_j_2015, alternative = "two.sided")
###

### Arbeitserfahrung
(fused_exp_arbeit.plot <- mydensplot.post(joint.post, "exp_arbeit_20_bis2015", xname = "Arbeitserfahrung 2015 in Jahren"))
ggsave("exparb15_passive.pdf")

(fused_exp_arbeit.distplot <- mydistplot.post(joint.post, "exp_arbeit_20_bis2015", xname = "Arbeitserfahrung 2015 in Jahren"))
ggsave("exparb15_passivedist.pdf")

ks.test(fA.nnd$exp_arbeit_20_bis2015, vskt.mp$exp_arbeit_20_bis2015, alternative = "two.sided")
###


### Arbeitslosenzeit
(fused_exp_al.plot <- mydensplot.post(joint.post, "exp_al_20_bis2015", xname = "Arbeitslosenzeit 2015 in Jahren"))
ggsave("expal15_passive.pdf")

(fused_exp_al.distplot <- mydistplot.post(joint.post, "exp_al_20_bis2015", xname = "Arbeitslosenzeit 2015 in Jahren"))
ggsave("expal15_passivedist.pdf")

ks.test(fA.nnd$exp_al_20_bis2015, vskt.mp$exp_al_20_bis2015, alternative = "two.sided")
###

### Grid
plot.inc.exp <- cowplot::plot_grid(fused_income89.plot, fused_income89.distplot, fused_exp_arbeit.plot, fused_exp_arbeit.distplot, ncol=2, nrow=2)
ggsave("grid_passiv3.pdf", plot.inc.exp)

plot.alexp <- cowplot::plot_grid(fused_exp_al.plot, fused_exp_al.distplot, ncol=2, nrow=1)
ggsave("grid_passiv4.pdf", plot.alexp)
###


#### Arbeitszeit 2015 2d densityplot

# Calculate the common x and y range 
(xrng = range(c(fA.nnd$exp_arbeit_20_bis2015, vskt.mp$exp_arbeit_20_bis2015)))
(yrng = range(c(fA.nnd$gbja, vskt.mp$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$exp_arbeit_20_bis2015, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.mp$exp_arbeit_20_bis2015, vskt.mp$gbja, lims=c(xrng, yrng), n=200)

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
names(diff12.m) = c("Arbeitszeit","Geburtsjahr","z")

# Plot difference between densities
ggplot(diff12.m, aes(Arbeitszeit, Geburtsjahr, z=z, fill=z)) +
  geom_tile() +
  theme_classic() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
  scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  ggtitle("2015-Arbeitszeit Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("diffpassivearbeit_new15.pdf")

#### Experience Arbeitslosigkeit 2015 2d densityplot

# Calculate the common x and y range 
(xrng = range(c(fA.nnd$exp_al_20_bis2015, vskt.mp$exp_al_20_bis2015)))
(yrng = range(c(fA.nnd$gbja, vskt.mp$gbja)))

# Calculate the 2d density estimate over the common range
d1 = kde2d(fA.nnd$exp_al_20_bis2015, fA.nnd$gbja, lims=c(xrng, yrng), n=200)
d2 = kde2d(vskt.mp$exp_al_20_bis2015, vskt.mp$gbja, lims=c(xrng, yrng), n=200)

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
names(diff12.m) = c("Arbeitslosenzeit","Geburtsjahr","z")

# Plot difference between densities
ggplot(diff12.m, aes(Arbeitslosenzeit, Geburtsjahr, z=z, fill=z)) +
  geom_tile() +
  theme_classic() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
  scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  ggtitle("2015-Arbeitslosenzeit Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("differencepassive_new15.pdf")