####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

#### First Passive ####
maxyear <- 2017
# loading soep
vskt.mp <- import(paste(path, "vskt_passiv_panel_m.dta" , sep = "/"), setclass = "data.table")
soep.mp <- import(paste(path, "soep_passive_m.dta" , sep = "/"), setclass = "data.table")

vskt.fp <- import(paste(path, "vskt_passiv_panel_f.dta" , sep = "/"), setclass = "data.table")
soep.fp <- import(paste(path, "soep_passive_f.dta" , sep = "/"), setclass = "data.table")

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
         sex = factor(sex, ordered = F),
         education = factor(education, ordered = T)) 
vskt.mp <- vskt.mp %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  mutate(age = maxyear -gbja) %>% 
  mutate(ltearnings = npv_2060_r_net,
         sex = factor(sex, ordered = F)) 

vskt.mp$expwork <- vskt.mp[,paste0("exp_arbeit_20_bis",maxyear)]
vskt.mp$expunempl <- vskt.mp[,paste0("exp_al_20_bis",maxyear)]

#select most interesting VSKT variables... can be remerged by case id 
vskt.mp <- select(vskt.mp, one_of(c("case", "gbja", "weight", paste0("rente_",maxyear,"_gesamt"), "unempben", "expunempl", "divorced", "expwork", "ltearnings", "age","ja", "sex")))

#select jointly observed variables
(X.vars <- intersect(names(soep.mp), names(vskt.mp)))


#choose subsection of variables on which to display descriptive statistics
vskt.tex <- select(vskt.mp, one_of(c(paste0("rente_",maxyear,"_gesamt"), "expunempl", "unempben", "expwork", "age", "divorced", "ltearnings")))
vskt.tex <- vskt.tex %>% 
  mutate(everdivorced = as.numeric(as.numeric(divorced)==2,0,1)) %>% 
  mutate(ltearnings = round(ltearnings)) %>% 
  mutate(unempben = round(unempben))

#Latex tables
names(vskt.tex) <- c("Pension entitl.", "Exp. unempl.","Unempl. benefit", "Work exp.", "Age", "divorced", "Lifetime earnings", "Ever divorced")
stargazer(vskt.tex, out = "descriptives_VSKT.tex", title = "Chosen descriptive statistics of the passive VSKT sample in 2015 with historic information",
          digits = 2, notes = "Author's calculations based on VSKT 2002, 2004-2015 passive West German population", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "tablepassive", notes.align = "l", summary.logical=T)


#### Random Forests #####

# formula object:

measurevars <- c("education","ltearnings")
groupvars  <- c("gbja", paste0("rente_",maxyear,"_gesamt"), "expunempl", "expwork", "unempben", "divorced", "sex")

formula1 <- as.formula(paste(measurevars[1], paste(groupvars, collapse=" + "), sep=" ~ "))
formula2 <- as.formula(paste(measurevars[2], paste(groupvars, collapse=" + "), sep=" ~ "))

#SOEP classification tree for variable importance

forestSOEP <- randomForest(formula1, data = soep.mp, importance = T, corr.bias = T)
pdf('forestSOEPpassiv.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = "", labels=c("Sex", "Ever divorced", "Unempl. benefit", "Exp. unempl.", "YoB", "Work exp.", "Pension entitl." ))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT regression tree for variable importance 
forestVSKT <- randomForest(formula2, data = vskt.mp , importance = T, corr.bias = T)
pdf('forestVSKTpassiv.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = "", labels = c("Ever divorce", "Exp. unempl.", "YoB", "Unempl. benefit", "Sex", "Pension entitl.", "Work exp."))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))
barplot(t(VI_FB/sum(VI_FB)))

#### Matching variable quality ####

#choose set of matchingvariables:	
#one could discuss whether uneployment benefits should be left out... keep it for now and check X_M quality	
X.mtc <- c(paste0("rente_",maxyear,"_gesamt"),"gbja" , "unempben", "expwork", "expunempl")	
names(X.mtc) <- c("Pension entitl.", "YoB", "Unempl. benef.", "Work exp." , "Exp. unempl.")	

# Hellinger Distance for matching variable quality	
A.mtc <- select(soep.mp, one_of(X.mtc))	
B.mtc <- select(vskt.mp, one_of(X.mtc))	

helldist <- unlist(nullToNA(sapply(X.mtc, function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))))	

#Latex table	
stargazer(helldist, out = "X_m_VSKT_SOEP.tex", title = "Potential set of matching variables with corresponding Hellinger distances",	
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2015 passive West German population", label = "X_m_application", notes.align = "l")	

#check unemployment shares:	
sum(A.mtc$expunempl>0)/nrow(A.mtc) # 73% in the passive SOEP population have never been unemployed	
sum(B.mtc$expunempl>0)/nrow(B.mtc) # 42% in the VSKT have never been unemployed	

sum(A.mtc$unempben>0)/ sum(A.mtc$expunempl>0) #only 65% of individualy with unemployment spells in the SOEP also report unemployment benefits	
sum(B.mtc$unempben>0) / sum(B.mtc$expunempl>0) #99% in the VSKT who experienced unemployment spells also report unemployment benefits	

#Experience in unemployment has too many zeros...integral not calculable	
#take only positive values and use those... note that we ommit the number of zeros, which could be potentially different	
temp1 <- filter(A.mtc, A.mtc$expunempl>0)	
temp2 <- filter(B.mtc, B.mtc$expunempl>0)	
temp3 <- hellinger(temp1$expunempl, temp2$expunempl, method = 1)	

##### Prepare for matching #####	

(X.mtc <- c(paste0("rente_",maxyear,"_gesamt"), "gbja", "expwork", "expunempl", "unempben")) #final X_M	
names(X.mtc) <- c("Pension entitl.", "YoB", "Work exp." , "Exp. unempl.", "Unempl.ben.")	

(Z.vars <- setdiff(names(vskt.mp), names(soep.mp))) #available just in VSKT	
(donclass <- c("divorced", "sex")) #donation classes	


##### Matching  #####	
# one request attempt distance, not random matching with Hungaran algorithm in order to	
# make sure that not too many of the same donors are assigned to the SOEP receivers 	

# randommatch undesired because it can't restrict donor being donated only once.

#randommatch1 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "Mahalanobis", cutdon="min", weight = "weight") 	
#randommatch2 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "minimax", cutdon="min", weight = "weight") 	
#randommatch3 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "Gower", cutdon="min", weight = "weight") 	

distancematch.passive1 <- distancehd(A=soep.mp,B=vskt.mp, distfun = "minimax")	
distancematch.passive2 <- distancehd(A=soep.mp,B=vskt.mp, distfun = "Gower")	
distancematch.passive3 <- distancehd(A=soep.mp,B=vskt.mp,distfun = "Mahalanobis")	
#------------------#	


xz.vars <- c(X.mtc, "ltearnings")	
xz.varsl <- as.list(c(X.mtc, "Lifetime earnings" = "ltearnings"))	

kstest1 <- sapply(xz.varsl, function(t) ks.test(select(	
  distancematch.passive1, one_of(xz.vars))[,t], select(	
    vskt.mp, one_of(xz.vars))[,t], 	
  alternative = "two.sided")$statistic)	

kstest2 <- sapply(xz.varsl, function(t) ks.test(select(	
  distancematch.passive2, one_of(xz.vars))[,t], select(	
    vskt.mp, one_of(xz.vars))[,t], 	
  alternative = "two.sided")$statistic)	

kstest3 <- sapply(xz.varsl, function(t) ks.test(select(	
  distancematch.passive3, one_of(xz.vars))[,t], select(	
    vskt.mp, one_of(xz.vars))[,t], 	
  alternative = "two.sided")$statistic)	

#kstest3 <- sapply(xz.varsl, function(t) ks.test(select(	
#  randommatch3, one_of(xz.vars))[,t], select(	
#    vskt.mp, one_of(xz.vars))[,t], 	
#  alternative = "two.sided")$statistic)	

kstestfinal <- round(rbind(kstest1, kstest2, kstest3),digits = 4)	
rownames(kstestfinal) <- c("Dist: Minimax", "Dist: Gower",  "Dist: Mahalanobis")	
ks.cutofflevel <- 1.224 * sqrt((nrow(soep.mp) + nrow(vskt.mp))/(nrow(soep.mp)*nrow(vskt.mp)))	
kstestfinal
#multivariate level 4 results:	

mvartest1 <- bd.test(select(distancematch.passive1, one_of(xz.vars)), select(vskt.mp, one_of(xz.vars)))$statistic	
mvartest2 <- bd.test(select(distancematch.passive2, one_of(xz.vars)), select(vskt.mp, one_of(xz.vars)))$statistic	
mvartest3 <- bd.test(select(distancematch.passive3, one_of(xz.vars)), select(vskt.mp, one_of(xz.vars)))$statistic	


bdtestfinal <- rbind(mvartest1, mvartest2, mvartest3)
rownames(bdtestfinal) <- c("Dist: Minimax", "Dist: Gower",  "Dist: Mahalanobis")	
bdtestfinal


### choose the one that has the smallest test statistic!!!!!
# last table	

stargazer(bdtestfinal, out = "applevel1.tex", title = "Ball divergence test after several hot deck matching routines of SOEP and VSKT",	
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2017 passive West German population. Displayed are Ball Divergence Statistics",	
          label = "lv4application", notes.align = "l", summary = F) 
#### Deploy final use file ####

save(distancematch.passive2, file="passive_first_stage.RDA")
write.dta(distancematch.passive2, file = "passive_first_stage.dta")

#### Finished #####


