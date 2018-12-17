rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")


soep.mp <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")

vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")


soep.mp <- soep.mp %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(divorced,ordered = F)) %>% 
  #mutate(age_g = factor(age_g,ordered = T)) %>% 
  mutate(age = 2015 -gbja) %>% 
  mutate(soep=1)
vskt.mp <- vskt.mp %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  #mutate(age_g = factor(age_g,ordered = T))
  mutate(expwork = exp_arbeit_20_bis2015) %>% 
  mutate(expunempl = exp_al_20_bis2015) %>% 
  mutate(unempben = alg_j_2015) %>% 
  mutate(age = 2015 -gbja) %>% 
  mutate(soep=0)


(X.vars <- intersect(names(soep.mp), names(vskt.mp)))

#choose subsection of variables on which to display descriptive statistics

vskt.tex <- select(vskt.mp, one_of(c("rente_2015_gesamt", "expunempl", "unempben", "exp_arbeit", "age", "brutto_zens_2015", "sex", "divorced")))
stargazer(vskt.tex, out = "descriptives_sapa.tex", title = "Chosen descriptive statistics of the passive SAPA sample in 2015 with historic information",
          digits = 0, notes = "Author's calculations based on SAPA 2002, 2003-2015 passive West German population", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "tablepassive", notes.align = "l", summary.logical=T)


#SOEP
forestSOEP <- randomForest(factor(education, ordered = T) ~ sex + age + rente_2015_gesamt + expunempl + expwork + unempben + divorced, data = soep.mp, importance = T, corr.bias = T)

pdf('forestSOEPpassiv.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = c("Random Forest Importance Plot", "für Variablenselektion im SOEP"))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT
forestVSKT <- randomForest(brutto_zens_2015 ~ sex + age + rente_2015_gesamt + expunempl + expwork + unempben + divorced, data = vskt.mp, importance = T, corr.bias = T)
pdf('forestVSKTpassiv.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = c("Random Forest Importance Plot" ,"für Variablenselektion in der VSKT"))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))

barplot(t(VI_FB/sum(VI_FB)))


soep.mp <- soep.mp %>% 
  mutate(weight=sum(vskt.mp$weight)/nrow(soep.mp))

joint <- bind_rows(soep.mp, vskt.mp)
joint <- joint %>% 
  mutate(soep = factor(soep, ordered = F))


(birthplot <- mydensplot(joint, "gbja", xname = "Geburtskohorte"))
ggsave("geburtsjahr_mp.pdf")

(birthplot.w <- mydensplot(joint, "gbja", xname = "Geburtskohorte (gewichtet)", weight = "weight"))
ggsave("geburtsjahr_mp_weighted.pdf")


#(birthdist <- mydistplot(joint, "gbja", xname = "Geburtskohorte")) 
#ggsave("geburtsjahr_dist_mp.pdf")

#plot.grdgbja <- cowplot::plot_grid(birthplot, birthplot.w, birthdist, ncol=2, nrow=2)
#ggsave("grid_passiv1.pdf", plot.grdgbja)


(kstest.gbja <- ks.test(soep.mp$gbja, vskt.mp$gbja, alternative = "two.sided"))



(rentenplot <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_mp.pdf")

(rentenplot.w <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in € (gewichtet)", weight="weight"))
ggsave("rente_mp_weighted.pdf")

#(rentendist <- mydistplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
#ggsave("rente_dist.pdf")

(expworkplot <- mydensplot(joint, "expwork", xname = "Arbeitserfahrung bis Renteneintritt"))
ggsave("expwork_mp.pdf")

(expworkplot.w <- mydensplot(joint, "expwork", xname = "Arbeitserfahrung bis Renteneintritt (gewichtet)", weight="weight"))
ggsave("expwork_weighted.pdf")

(kstest.rente <- ks.test(soep.mp$rente_2015_gesamt, vskt.mp$rente_2015_gesamt, alternative = "two.sided"))

plot.grdrente <- cowplot::plot_grid(birthplot, birthplot.w, expworkplot, expworkplot.w, rentenplot, rentenplot.w, ncol=2, nrow=3)
ggsave("grid_passiv.pdf", plot.grdrente)

#### checking multivariate correlation structure (usually)

### It might be worth adding a income indicator sth like 


Z.vars <- setdiff(names(vskt.mp), names(soep.mp)) # available just in VSKT
(X.mtc <- c("rente_2015_gesamt", "expwork", "gbja"))
(donclass <- c("divorced","sex"))

distancematch <- distancehd(A=soep.mp, B=vskt.mp, distfun = "minimax") 
randommatch <- randomhd(A=soep.mp, B=vskt.mp, distfun = "ANN", cutdon="rot", weight = "weight") 
randommatch_unwgt <- randomhd(A=soep.mp, B=vskt.mp, distfun = "ANN", cutdon="rot") 

xz.vars <- c(X.mtc, "brutto_zens_2013", "brutto_zens_2014", "brutto_zens_2015")
xz.varsl <- as.list(c(X.mtc, "brutto_zens_2013", "brutto_zens_2014", "brutto_zens_2015"))
names(xz.varsl) <- c("Rente 2015", "Arbeitserfahrung 2012", "Geburtsjahr", "Arbeitsentgeld 2013", "Arbeitsentgeld 2014", "Arbeitsentgeld 2015")

dist.hellinger <- lapply(xz.varsl, function(t) tryCatch({hellinger(select(
  distancematch, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

rand.hellinger <- lapply(xz.varsl, function(t) tryCatch({hellinger(select(
  randommatch, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

rand.hellinger_unwgt <- lapply(xz.varsl, function(t) tryCatch({hellinger(select(
  randommatch_unwgt, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))


kstest <- lapply(xz.varsl, function(t) ks.test(select(
  distancematch, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)
kstest2 <- lapply(xz.varsl, function(t) ks.test(select(
  randommatch, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)


hellingerdf <- ldply(rand.hellinger, data.frame, .id = "Variable")
names(hellingerdf) <- c("Variable", "Hellinger Distanz")
stargazer(hellingerdf, summary = F, title = "Hellinger Distanzen für die Mathching-Güte der passiven Bevölkerung", out = "hellinger_passiv.tex",
          notes = "Eigene Berechnungen auf Basis des SOEP v.32 und der VSKT; Für Hellinger Distanzen unter dem Wert 0.05 kann von Gleichheit der Verteilungen ausgegangen werden",
          notes.align = "l")

save(randommatch, file="passive_match_weighted.RDA")
write.dta(randommatch, file = "passive_match_weighted.dta")


#save(nnd.hd, file = "match_passiv_gesamt.RDA")
#save(fA.nnd, file = "fused_passiv_gesamt.RDA")

##########################################################
##########################################################
##########################################################
                  ### diagnostics####
##########################################################
##########################################################
##########################################################


load("match_passiv_gesamt.RDA")
load("fused_passiv_gesamt.RDA")


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



#### It might be worth to check the rank hot deck performance with weights...
#### that sounds really promising!

save(fA.nnd, file="passive_all.RDA")

write.dta(fA.nnd, file = "passive_all.dta")








