####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

#### First Passive ####
maxyear <- 2016
# loading soep
vskt.mp <- import(paste(path.new, "vskt_passiv_panel_m.dta" , sep = "/"), setclass = "data.table")
soep.mp <- import(paste(path.new, "soep_passive_m.dta" , sep = "/"), setclass = "data.table")

vskt.fp <- import(paste(path.new, "vskt_passiv_panel_f.dta" , sep = "/"), setclass = "data.table")
soep.fp <- import(paste(path.new, "soep_passive_f.dta" , sep = "/"), setclass = "data.table")

soep.mp$sex <- 0
soep.fp$sex <- 1
vskt.mp$sex <- 0
vskt.fp$sex <- 1

soep.mp <- bind_rows(soep.mp,soep.fp)
vskt.mp <- bind_rows(vskt.mp,vskt.fp)

rm(vskt.fp,soep.fp)


soep.mp <- soep.mp %>% 
  mutate(divorced = factor(divorce5,ordered = F)) %>% 
  mutate(age = maxyear -gbja,
         sex = factor(sex, ordered = F)) 
vskt.mp <- vskt.mp %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  mutate(expwork = exp_arbeit_20_bis2016) %>% 
  mutate(expunempl = exp_al_20_bis2016) %>% 
  mutate(age = maxyear -gbja) %>% 
  mutate(ltearnings = npv_2060_r_net,
         sex = factor(sex, ordered = F)) 


#select most interesting VSKT variables... can be remerged by case id 
vskt.mp <- select(vskt.mp, one_of(c("case", "gbja", "weight", "rente_2016_gesamt", "unempben", "expunempl", "divorced", "expwork", "ltearnings", "age","ja", "sex")))

#select jointly observed variables
(X.vars <- intersect(names(soep.mp), names(vskt.mp)))


#choose subsection of variables on which to display descriptive statistics
vskt.tex <- select(vskt.mp, one_of(c("rente_2016_gesamt", "expunempl", "unempben", "expwork", "age", "divorced", "ltearnings")))
vskt.tex <- vskt.tex %>% 
  mutate(everdivorced = as.numeric(as.numeric(divorced)==2,0,1)) %>% 
  mutate(ltearnings = round(ltearnings)) %>% 
  mutate(rente_2016_gesamt = round(rente_2016_gesamt)) %>% 
  mutate(unempben = round(unempben))

#Latex tables
names(vskt.tex) <- c("Pension entitl.", "Exp. unempl.","Unempl. benefit", "Work exp.", "Age", "divorced", "Lifetime earnings", "Ever divorced")
stargazer(vskt.tex, out = "descriptives_VSKT.tex", title = "Chosen descriptive statistics of the passive VSKT sample in 2015 with historic information",
          digits = 2, notes = "Author's calculations based on VSKT 2002, 2004-2015 passive West German population", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "tablepassive", notes.align = "l", summary.logical=T)


#### Random Forests #####

#SOEP classification tree for variable importance
forestSOEP <- randomForest(factor(education, ordered = T) ~ gbja + rente_2016_gesamt + expunempl + expwork + unempben + divorced + sex, data = soep.mp, importance = T, corr.bias = T)
pdf('forestSOEPpassiv.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = "", labels=c("Sex", "Ever divorced", "Unempl. benefit", "Exp. unempl.", "YoB", "Work exp.", "Pension entitl." ))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT regression tree for variable importance 
forestVSKT <- randomForest(ltearnings ~ gbja + rente_2016_gesamt + expunempl + expwork + unempben + divorced + sex, data = vskt.mp , importance = T, corr.bias = T)
pdf('forestVSKTpassiv.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = "", labels = c("Ever divorce", "Exp. unempl.", "YoB", "Unempl. benefit", "Sex", "Pension entitl.", "Work exp."))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))
barplot(t(VI_FB/sum(VI_FB)))

#### Matching variable quality ####
 
#### Deploy final use file ####

save(distancematch.passive2, file="passive_first_stage.RDA")
write.dta(distancematch.passive2, file = "passive_first_stage.dta")

#### Finished #####










