rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")


soep.mp <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")

vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")

(X.vars <- intersect(names(soep.mp), names(vskt.mp)))

soep.mp <- soep.mp %>% 
  mutate(soep=1) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T))

vskt.mp <- vskt.mp %>% 
  mutate(soep=0) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T))


joint <- bind_rows(soep.mp, vskt.mp)
joint <- joint %>% 
  mutate(soep = factor(soep, ordered = F)) 

(birthplot <- mydensplot(joint, "gbja", xname = "Geburtskohorte"))
ggsave("geburtsjahr_mp.pdf")


(birthdist <- mydistplot(joint, "gbja", xname = "Geburtskohorte")) 
ggsave("geburtsjahr_dist_mp.pdf")



ks.test(soep.mp$gbja, vskt.mp$gbja, alternative = "two.sided")


(rentenplot <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_mp.pdf")
(rentendist <- mydistplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_dist.pdf")

ks.test(soep.mp$rente_2015_gesamt, vskt.mp$rente_2015_gesamt, alternative = "two.sided")

vskt.vars <- setdiff(names(vskt.mp), names(soep.mp)) # available just in VSKT


nnd.hd <- NND.hotdeck(data.rec=soep.mp, data.don=vskt.mp,
                      match.vars="rente_2015_gesamt", 
                      don.class = c("gbja_cat","sex"),
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)

fA.nnd <- create.fused(data.rec=soep.mp, data.don=vskt.mp,
                       mtc.ids=nnd.hd$mtc.ids,
                       z.vars=vskt.vars)


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

### Pension in 2015
fused_rente15.plot <- mydensplot.post(joint.post, "rente_j_2015", xname = "Rentenhöhe 2015 in €", lmts =c(1, 40000))
ks.test(fA.nnd$rente_j_2015, vskt.mp$rente_j_2015, alternative = "two.sided")

ggsave("furente15.pdf")


fused_exp_arbeit.plot <- mydensplot.post(joint.post, "exp_arbeit_20_bis2015", xname = "Arbeitserfahrung 2015 in Jahren")
ks.test(fA.nnd$exp_arbeit_20_bis2015, vskt.mp$exp_arbeit_20_bis2015, alternative = "two.sided")

ggsave("exparb15.pdf")


#### Experience Arbeit 2015 2d densityplot

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
names(diff12.m) = c("Arbeitserfahrung","Geburtsjahr","z")

# Plot difference between densities
ggplot(diff12.m, aes(Arbeitserfahrung, Geburtsjahr, z=z, fill=z)) +
  geom_tile() +
  theme_classic() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
  scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  ggtitle("2015-Arbeitserfahrung Contour-Differenzen zwischen Fused Data und VSKT")

ggsave("differencepassive_new15.pdf")
