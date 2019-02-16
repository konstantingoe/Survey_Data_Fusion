rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

##### Initiate #####

#load SOEP and VSKT
soep.mp <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")
vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")


soep.mp <- soep.mp %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(divorced,ordered = F)) %>% 
  mutate(age = 2015 -gbja) 
vskt.mp <- vskt.mp %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(spez_scheidung,ordered = F)) %>% 
  mutate(expwork = exp_arbeit_20_bis2015) %>% 
  mutate(expunempl = exp_al_20_bis2015) %>% 
  mutate(age = 2015 -gbja) %>% 
  mutate(ltearnings = npv_1760_r_net)

#select most interesting VSKT variables... can be remerged by case id 
vskt.mp <- select(vskt.mp, one_of(c("case","sex", "gbja", "weight", "rente_2015_gesamt", "unempben", "expunempl", "divorced", "expwork", "ltearnings", "age", "soep")))

#select jointly observed variables
(X.vars <- intersect(names(soep.mp), names(vskt.mp)))


#choose subsection of variables on which to display descriptive statistics
vskt.tex <- select(vskt.mp, one_of(c("rente_2015_gesamt", "expunempl", "unempben", "expwork", "age", "brutto_zens_2015", "sex", "divorced", "ltearnings")))
vskt.tex <- vskt.tex %>% 
  mutate(female = as.numeric(ifelse(as.numeric(sex)==2,0,1))) %>% 
  mutate(everdivorced = as.numeric(as.numeric(divorced)==2,0,1)) %>% 
  mutate(ltearnings = round(ltearnings)) %>% 
  mutate(rente_2015_gesamt = round(rente_2015_gesamt)) %>% 
  mutate(unempben = round(unempben))

#Latex tables
names(vskt.tex) <- c("Pension entitl.", "Exp. unempl.","Unempl. benefit", "Work exp.", "Age", "sex", "divorced", "Lifetime earnings", "Female", "Ever divorced")
stargazer(vskt.tex, out = "descriptives_VSKT.tex", title = "Chosen descriptive statistics of the passive VSKT sample in 2015 with historic information",
          digits = 2, notes = "Author's calculations based on VSKT 2002, 2004-2015 passive West German population", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "tablepassive", notes.align = "l", summary.logical=T)


#### Random Forests #####

#SOEP classification tree for variable importance
forestSOEP <- randomForest(factor(education, ordered = T) ~ sex + gbja + rente_2015_gesamt + expunempl + expwork + unempben + divorced, data = soep.mp, importance = T, corr.bias = T)
pdf('forestSOEPpassiv.pdf',height=4, width=6)
varImpPlot(forestSOEP,type=2, main = "", labels=c("Gender", "Ever divorced", "Unempl. benefit", "Exp. unempl.", "YoB", "Work exp.", "Pension entitl." ))
dev.off()
(VI_FA <- importance(forestSOEP, type=2, scale = F))
barplot(t(VI_FA/sum(VI_FA)))

#VSKT regression tree for variable importance 
forestVSKT <- randomForest(ltearnings ~ sex + gbja + rente_2015_gesamt + expunempl + expwork + unempben + divorced, data = vskt.mp, importance = T, corr.bias = T)
pdf('forestVSKTpassiv.pdf',height=4, width=6)
varImpPlot(forestVSKT,type=2, main = "", labels = c("Ever divorce", "Exp. unempl.", "Unempl. benefit", "YoB", "Gender", "Pension entitl.", "Work exp."))
dev.off()
(VI_FB <- importance(forestVSKT, type=2, scale = F))
barplot(t(VI_FB/sum(VI_FB)))

#### Matching variable quality ####

#choose set of matchingvariables:
#one could discuss whether uneployment benefits should be left out... keep it for now and check X_M quality
X.mtc <- c("rente_2015_gesamt","gbja" , "unempben", "expwork", "expunempl")
names(X.mtc) <- c("Pension entitl.", "YoB", "Unempl. benef.", "Work exp." , "Exp. unempl.")

# Hellinger Distance for matching variable quality
A.mtc <- select(soep.mp, one_of(X.mtc))
B.mtc <- select(vskt.mp, one_of(X.mtc))

helldist <- unlist(nullToNA(sapply(X.mtc, function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))))

#Latex table
stargazer(helldist, out = "X_m_VSKT_SOEP.tex", title = "Potential set of matching variables with corresponding Hellinger distances",
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2015 passive West German population", label = "X_m_application", notes.align = "l")

#check unemployment shares:
sum(A.mtc$expunempl>0)/nrow(A.mtc) # 75% in the passive SOEP population have never been unemployed
sum(B.mtc$expunempl>0)/nrow(B.mtc) # 48% in the VSKT have never been unemployed

sum(A.mtc$unempben>0)/ sum(A.mtc$expunempl>0) #only 55% of individualy with unemployment spells in the SOEP also report unemployment benefits
sum(B.mtc$unempben>0) / sum(B.mtc$expunempl>0) #97% in the VSKT who experienced unemployment spells also report unemployment benefits

#Experience in unemployment has too many zeros...integral not calculable
#take only positive values and use those... note that we ommit the number of zeros, which could be potentially different
temp1 <- filter(A.mtc, A.mtc$expunempl>0)
temp2 <- filter(B.mtc, B.mtc$expunempl>0)
temp3 <- hellinger(test1$expunempl, test2$expunempl, method = 1)

##### Prepare for matching #####

(X.mtc.final <- c("rente_2015_gesamt", "gbja", "expwork", "expunempl")) #final X_M
names(X.mtc.final) <- c("Pension entitl.", "YoB", "Work exp." , "Exp. unempl.")

(Z.vars <- setdiff(names(vskt.mp), names(soep.mp))) #available just in VSKT
(donclass <- c("divorced","sex")) #donation classes


##### Matching  #####
randommatch1 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "Mahalanobis", cutdon="min", weight = "weight") 
randommatch2 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "minimax", cutdon="min", weight = "weight") 
randommatch3 <- randomhd(A=soep.mp, B=vskt.mp, distfun = "Gower", cutdon="min", weight = "weight") 
#------------------#

#### Post-matching diagnostics: level 4 #####

xz.vars <- c(X.mtc.final, "ltearnings")
xz.varsl <- as.list(c(X.mtc.final, "Lifetime earnings" = "ltearnings"))

kstest1 <- sapply(xz.varsl, function(t) ks.test(select(
  randommatch1, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)

kstest2 <- sapply(xz.varsl, function(t) ks.test(select(
  randommatch2, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)

kstest3 <- sapply(xz.varsl, function(t) ks.test(select(
  randommatch3, one_of(xz.vars))[,t], select(
    vskt.mp, one_of(xz.vars))[,t], 
  alternative = "two.sided")$statistic)

kstestfinal <- round(bind_rows(kstest1, kstest2, kstest3),digits = 4)
rownames(kstestfinal) <- c("Mahalanobis", "Minimax", "Gower")
ks.cutofflevel <- 1.224 * sqrt((nrow(soep.mp) + nrow(vskt.mp))/(nrow(soep.mp)*nrow(vskt.mp)))

#multivariate level 4 results:

mvartest1 <- cramer.test(as.matrix(select(randommatch1, one_of(xz.vars))), as.matrix(select(vskt.mp, one_of(xz.vars))))
mvartest2 <- cramer.test(as.matrix(select(randommatch2, one_of(xz.vars))), as.matrix(select(vskt.mp, one_of(xz.vars))))
mvartest3 <- cramer.test(as.matrix(select(randommatch3, one_of(xz.vars))), as.matrix(select(vskt.mp, one_of(xz.vars))))

#save(mvartest1, file="applicramer1")
#save(mvartest2, file="applicramer2")
#save(mvartest3, file="applicramer3")

mvarfinal <- as.data.frame(c(mvartest1$statistic, mvartest2$statistic, mvartest3$statistic))
rownames(mvarfinal) <- c("Mahalanobis", "Minimax", "Gower")
colnames(mvarfinal) <- "Cramer"

kstestfinal <- bind_cols(kstestfinal, mvarfinal)
rownames(kstestfinal) <- c("Mahalanobis", "Minimax", "Gower")

# last table

stargazer(kstestfinal, out = "applevel4.tex", title = "Kolmogorov-Smirnov distance after weighted random distance hot deck matching of SOEP and VSKT",
          digits = 4, notes = "Author's calculations based on SOEP and VSKT 2002, 2004-2015 passive West German population. Displayed are KS distances. The correspoding critical value for equivalence of marginal distributions is $0.021$",
          label = "lv4application", notes.align = "l", summary = F)


#### Deploy final use file ####

save(randommatch1, file="passive_match_weighted.RDA")
write.dta(randommatch1, file = "passive_match_weighted.dta")

#### Finished #####



