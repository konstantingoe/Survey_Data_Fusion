####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

# loading soep
soep.men.active <- import(paste(path, "soep_2012_m_aktiv.dta" , sep = "/"), setclass = "data.table") %>% 
  mutate(sex=0)

soep.women.active <- import(paste(path, "soep_2012_f_aktiv.dta" , sep = "/"), setclass = "data.table") %>% 
  mutate(sex=1)
soep.full <- bind_rows(soep.men.active, soep.women.active) %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung,ordered = F)) %>% 
  mutate(em_rente = factor(em_rente, ordered = F)) %>% 
  mutate(age_g = factor(age_g,ordered = T))
#remove missings
soep.full <- soep.full[complete.cases(soep.full), ]


rm(soep.men.active,soep.women.active)

# loading VSKT
vskt.men.active <- import(paste(path, "vskt_m_active.dta" , sep = "/"), setclass = "data.table") %>% 
  mutate(sex=0)

vskt.women.active <- import(paste(path, "vskt_f_active.dta" , sep = "/"), setclass = "data.table") %>% 
  mutate(sex=1)
vskt.full <- bind_rows(vskt.men.active, vskt.women.active) %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(spez_scheidung = factor(spez_scheidung,ordered = F)) %>% 
  mutate(em_rente = factor(em_rente_2012, ordered = F)) %>% 
  select(-em_rente_2012) %>% 
  mutate(age_g = factor(age_g,ordered = T))
#vskt.full <- vskt.full[complete.cases(vskt.full), ]
rm(vskt.men.active,vskt.women.active)

# Pre- Matching

(X.vars <- intersect(names(soep.full), names(vskt.full)))

# Random Forest for Matching Variable Selection

#SOEP
forestSOEP <- randomForest(factor(educ, ordered = T) ~ sex + age + rentenanspruch_2012 + exp_al_20_bis2012 + exp_arbeit_20_bis2012 + brutto_zens_2012 + spez_scheidung + alg_j_2012 + em_rente, data = soep.full, importance = T, corr.bias = T)

pdf('forestSOEP.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = c("Random Forest Importance Plot", "für Variablenselektion im SOEP"))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT
forestVSKT <- randomForest(brutto_zens_2013 ~ sex + age + rentenanspruch_2012 + exp_al_20_bis2012 + exp_arbeit_20_bis2012 + brutto_zens_2012 + spez_scheidung + alg_j_2012, data = vskt.full, importance = T, corr.bias = T)
pdf('forestVSKT.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = c("Random Forest Importance Plot", "für Variablenselektion in der VSKT"))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))

barplot(t(VI_FB/sum(VI_FB)))

# choose as matching variables jointly best
# as dontation class, dummy variables that are not important

X.mtc <- c("brutto_zens_2012","rentenanspruch_2012" , "exp_arbeit_20_bis2012", "age", "exp_al_20_bis2012")
donclass <- c("sex", "spez_scheidung", "em_rente")


# check Hellinger distance between Matching variables!

A.mtc <- select(soep.full, one_of(X.mtc))
B.mtc <- select(vskt.full, one_of(X.mtc))

#satisfyingly close / rentenanspruch_2012 a little above 0.05
dist <- sapply(as.list(X.mtc), function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

# visualize via Kernel-Densities:
soep.full <- soep.full %>% 
  mutate(soep=1)

vskt.full <- vskt.full %>% 
  mutate(soep=0)

joint <- bind_rows(soep.full, vskt.full)
joint <- joint %>% 
  mutate(soep = factor(soep, ordered = F)) 

####### Pension entitlements
anwartschaften.plot <- mydensplot(joint, "rentenanspruch_2012", xname="Rentenanwartschaften (2012) in EUR")
ggsave("anwartschaften.pdf")

####### Birthyear
birthyear.plot <- mydensplot(joint, "gbja", xname="Geburtsjahr")
ggsave("birthyear.pdf")

####### Income
income.plot <- mydensplot(joint, "brutto_zens_2012", xname = "Arbeitsentgeld (2012) in €", lmts = c(1, 70000))
ggsave("income.pdf")

####### Worktime
worktime.plot <- mydensplot(joint, "exp_arbeit_20_bis2012",xname = "Arbeitserfahrung in Monaten bis 2012")
ggsave("wtime.pdf")

####### Unemployment exprerience  
unempexp.plot <- mydensplot(joint, "exp_al_20_bis2012",xname = "Monate in Arbeitslosigkeit bis 2012", lmts = c(1,200))
ggsave("unemptime.pdf")

q <- cowplot::plot_grid(anwartschaften.plot, income.plot, birthyear.plot, worktime.plot, unempexp.plot, ncol=2, nrow=3)
ggsave("grid2.pdf", q)

soep.vars <- setdiff(names(soep.men.active), names(vskt.m.active)) # available just in SOEP

Z.vars <- setdiff(names(vskt.full), names(soep.full)) # available just in VSKT

### matching:

distancematch <- distancehd(A=soep.full,B=vskt.full, distfun = "minimax")
randommatch <- randomhd(A=soep.full,B=vskt.full, distfun = "ANN", cutdon = "min")

### ks-distance post matching
xz.vars <- c(X.mtc, "brutto_zens_2013" )
xz.varsl <- as.list(c(X.mtc, "brutto_zens_2013" ))
names(xz.varsl) <- c("Arbeitsentgeld 2012", "Rentenanwartschaften 2012", "Arbeitserfahrung 2012", "Alter", "Arbeitslosenzeit 2012", "Arbeitentgeld 2013")

kstest <- lapply(xz.varsl, function(t) ks.test(select(
  distancematch, one_of(xz.vars))[,t], select(
    vskt.full, one_of(xz.vars))[,t], 
  alternative = "two.sided"))
kstest2 <- lapply(xz.varsl, function(t) ks.test(select(
  randommatch, one_of(xz.vars))[,t], select(
    vskt.full, one_of(xz.vars))[,t], 
  alternative = "two.sided"))
# report hellinger distance, as it is already explained and merely all variables are not fully continuous
dist.hellinger <- lapply(xz.varsl, function(t) tryCatch({hellinger(select(
  distancematch, one_of(xz.vars))[,t], select(
    vskt.full, one_of(xz.vars))[,t], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

rand.hellinger <- lapply(xz.varsl, function(t) tryCatch({hellinger(select(
  randommatch, one_of(xz.vars))[,t], select(
    vskt.full, one_of(xz.vars))[,t], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

hellingerdf <- ldply(rand.hellinger, data.frame, .id = "Variable")
names(hellingerdf) <- c("Variable", "Hellinger Distanz")
stargazer(hellingerdf, summary = F, title = "Hellinger Distanzen für Mathching-Güte", out = "hellinger_aktiv.tex",
           notes = "Eigene Berechnungen auf Basis des SOEP v.32 und der VSKT; Für Hellinger Distanzen unter dem Wert 0.05 kann von Gleichheit der Verteilungen ausgegangen werden",
          notes.align = "l")
# graphical assesssment

randommatch <- randommatch %>% 
  mutate(vskt=0)

vskt.full <- vskt.full %>% 
  mutate(vskt=1)

joint2 <- bind_rows(randommatch, vskt.full)

joint2 <- joint2 %>% 
  mutate(vskt=factor(vskt, ordered = F))

##### Income 2013
inc2013.plot <- mydensplot.post(joint2, "brutto_zens_2013", xname = "Arbeitsentgeld 2013 in €", lmts = c(1,70000))
ggsave("income2013.pdf")

save(randommatch, file="active_match.RDA")

write.dta(randommatch, file = "active_match.dta")


