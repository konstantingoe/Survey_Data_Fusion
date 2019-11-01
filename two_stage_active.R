####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

#### Active ####
maxyear <- 2012
# loading soep
vskt.ma <- import(paste(path.new, "vskt_m_active.dta" , sep = "/"), setclass = "data.table")
soep.ma <- import(paste(path.new, "soep_m_aktiv.dta" , sep = "/"), setclass = "data.table")

vskt.fa <- import(paste(path.new, "vskt_f_active.dta" , sep = "/"), setclass = "data.table")
soep.fa <- import(paste(path.new, "soep_f_aktiv.dta" , sep = "/"), setclass = "data.table")

soep.ma$sex <- 0
soep.fa$sex <- 1
vskt.ma$sex <- 0
vskt.fa$sex <- 1

soep.ma <- bind_rows(soep.ma,soep.fa)
vskt.ma <- bind_rows(vskt.ma,vskt.fa)

rm(vskt.fa,soep.fa)


soep.ma <- soep.ma %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  mutate(expwork = exp_arbeit_20_bis2012) %>% 
  mutate(expunempl = exp_al_20_bis2012) %>% 
  mutate(age = maxyear -gbja) %>% 
  mutate(entitlement = rentenanspruch_2012) %>% 
  mutate(income = brutto_zens_2012) %>% 
  mutate(unempben = alg_j_2012,
         sex = factor(sex, ordered = F)) %>% 
  select(-spez_scheidung, -rente_total_2012, -exp_arbeit_20_bis2012, -exp_al_20_bis2012, -rentenanspruch_2012, -brutto_zens_2012, -alg_j_2012)
vskt.ma <- vskt.ma %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  mutate(expwork = exp_arbeit_20_bis2012) %>% 
  mutate(expunempl = exp_al_20_bis2012) %>% 
  mutate(age = maxyear -gbja) %>% 
  mutate(entitlement = rentenanspruch_2012) %>% 
  mutate(income = brutto_zens_2012) %>% 
  mutate(unempben = alg_j_2012,
         sex = factor(sex, ordered = F)) %>% 
  select(-spez_scheidung, -em_rente, -rente_total_2012, -exp_arbeit_20_bis2012, -exp_al_20_bis2012, -rentenanspruch_2012, -brutto_zens_2012, -alg_j_2012)

soep.ma <- na.omit(soep.ma)
#select jointly observed variables
(X.vars <- intersect(names(soep.ma), names(vskt.ma)))

#### Random Forests #####

#SOEP classification tree for variable importance
forestSOEP <- randomForest(factor(educ, ordered = T) ~ age + entitlement + expunempl + expwork + unempben + income + divorced + sex, data = soep.ma, importance = T, corr.bias = T)
pdf('forestSOEPactive.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = "") # , labels=c("Ever divorced", "Unempl. benefit", "Exp. unempl.", "YoB", "Work exp.", "Pension entitl." ))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT regression tree for variable importance 
forestVSKT <- randomForest(ltearnings ~ age + entitlement + expunempl + expwork + unempben + income + divorced + sex, data = vskt.ma, importance = T, corr.bias = T, na.action = na.omit)
pdf('forestVSKTpassiv.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = "") #, labels = c("Ever divorce", "Exp. unempl.", "Unempl. benefit", "YoB", "Work exp.", "Pension entitl."))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))
barplot(t(VI_FB/sum(VI_FB)))

#### Matching variable quality ####

#choose set of matchingvariables:
#one could discuss whether uneployment benefits should be left out... keep it for now and check X_M quality
X.mtc <- c("entitlement","income" , "age", "expwork", "expunempl")

# Hellinger Distance for matching variable quality
A.mtc <- select(soep.ma, one_of(X.mtc))
B.mtc <- select(vskt.ma, one_of(X.mtc))

helldist <- unlist(nullToNA(sapply(X.mtc, function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))))

#Latex table
stargazer(helldist, out = "X_m_VSKT_SOEP.tex", title = "Potential set of matching variables with corresponding Hellinger distances",
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2015 passive West German population", label = "X_m_application", notes.align = "l")

#check unemployment shares:
sum(A.mtc$expunempl>0)/nrow(A.mtc) # 73% in the passive SOEP population have never been unemployed
sum(B.mtc$expunempl>0)/nrow(B.mtc) # 42% in the VSKT have never been unemployed

sum(soep.ma$unempben>0)/ sum(soep.ma$expunempl>0) #only 65% of individualy with unemployment spells in the SOEP also report unemployment benefits
sum(vskt.ma$unempben>0) / sum(vskt.ma$expunempl>0) #99% in the VSKT who experienced unemployment spells also report unemployment benefits

#Experience in unemployment has too many zeros...integral not calculable
#take only positive values and use those... note that we ommit the number of zeros, which could be potentially different
temp1 <- filter(A.mtc, A.mtc$expunempl>0)
temp2 <- filter(B.mtc, B.mtc$expunempl>0)
temp3 <- hellinger(temp1$expunempl, temp2$expunempl, method = 1)

##### Prepare for matching #####

(Z.vars <- setdiff(names(vskt.ma), names(soep.ma))) #available just in VSKT
(donclass <- c("sex", "divorced")) #donation classes


##### Matching  #####
### matching:

distancematch1 <- distancehd(A=soep.ma,B=vskt.ma, distfun = "minimax")

distancematch2 <- distancehd(A=soep.ma,B=vskt.ma,distfun = "Mahalanobis")

#randommatch <- randomhd(A=soep.ma,B=vskt.ma, distfun = "Gower", cutdon = "min")

xz.vars <- c(X.mtc, "ltearnings")
xz.varsl <- as.list(c(X.mtc, "Lifetime earnings" = "ltearnings"))

kstest1 <- sapply(xz.varsl, function(t) ks.test(select(
  distancematch1, one_of(xz.vars))[,t], select(
    vskt.ma, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)

kstest2 <- sapply(xz.varsl, function(t) ks.test(select(
  distancematch2, one_of(xz.vars))[,t], select(
    vskt.ma, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)

#kstest3 <- sapply(xz.varsl, function(t) ks.test(select(
#  randommatch, one_of(xz.vars))[,t], select(
#    vskt.ma, one_of(xz.vars))[,t], 
#  alternative = "two.sided")$statistic)

#kstestfinal <- round(rbind(kstest1, kstest2, kstest3),digits = 4)
#rownames(kstestfinal) <- c( "Minimax", "Mahalanobis", "Gower (random)")
#ks.cutofflevel <- 1.224 * sqrt((nrow(soep.ma) + nrow(vskt.ma))/(nrow(soep.ma)*nrow(vskt.ma)))

kstestfinal <- round(rbind(kstest1, kstest2),digits = 4)
rownames(kstestfinal) <- c( "Minimax", "Mahalanobis")
ks.cutofflevel <- 1.224 * sqrt((nrow(soep.ma) + nrow(vskt.ma))/(nrow(soep.ma)*nrow(vskt.ma)))

#multivariate level 4 results:

mvartest1 <- cramer.test(as.matrix(select(distancematch1, one_of(xz.vars))), as.matrix(select(vskt.ma, one_of(xz.vars))))
mvartest2 <- cramer.test(as.matrix(select(distancematch2, one_of(xz.vars))), as.matrix(select(vskt.ma, one_of(xz.vars))))
#mvartest3 <- cramer.test(as.matrix(select(randommatch, one_of(xz.vars))), as.matrix(select(vskt.ma, one_of(xz.vars))))

#save(mvartest1, file="applicramer1")
#save(mvartest2, file="applicramer2")
#save(mvartest3, file="applicramer3")

mvarfinal <- as.data.frame(c(mvartest1$statistic, mvartest2$statistic, mvartest3$statistic))
rownames(mvarfinal) <- c( "Minimax", "Mahalanobis", "Gower (random)")
colnames(mvarfinal) <- "Cramer"

kstestfinal <- bind_cols(kstestfinal, mvarfinal)
rownames(kstestfinal) <- c( "Minimax", "Mahalanobis", "Gower (random)") 

# last table

stargazer(kstestfinal, out = "applevel4.tex", title = "Kolmogorov-Smirnov distance after weighted random distance hot deck matching of SOEP and VSKT",
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2015 passive West German population. Displayed are KS distances. The correspoding critical value for equivalence of marginal distributions is $0.021$",
          label = "lv4application", notes.align = "l", summary = F)


#### Deploy final use file ####

save(distancematch1, file="active_first_stage.RDA")
write.dta(distancematch1, file = "active_first_stage.dta")

#### Finished #####



