####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")
 
set.seed(1234)

#load SOEP and VSKT
soep <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")
vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")

vskt.mp <- vskt.mp %>% 
  mutate(case = case/2+case^2)

# perform simulation on the basis of soep
# for this use soep twice delete in each set variables and keep X as common variables
# then take data set A and randomly drop nrow(soep)/nrow(vskt.mp) of the data... then match the full dataset B with 
# (X,Z) onto the smaller data set A with (X,Y).
# repeat this 1.000 times!

#### Sample Selection ####

soep <- soep %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(divorced, ordered = F)) %>% 
  select(-gbja_cat, - pwgt) #not useful here for retired population

# CIA assumption
CItest <- ci.test(x = "income", y = "education", z = c("sex","gbja", "rente_2015_gesamt","expunempl", "unempben" ,"expwork" , "divorced"), data = soep, test = "mi-cg")
# does not hold!

#### Create A and B ####

A <- select(soep, -income) #drop out income
B <- select(soep, -education) #drop out education

A <- A %>% 
  mutate(a=1)

B <- B %>% 
  mutate(a=0) %>% 
  mutate(persnrB = persnr)

#####  Graphically Checking densities #####
joint <- bind_rows(A, B)
joint <- joint %>% 
  mutate(a = factor(a, ordered = F))

#(birthplot <- mydensplot.sim(joint, "gbja", xname = "Geburtskohorte"))
#(kstest.gbja <- ks.test(A$gbja, B$gbja, alternative = "two.sided"))
#works perfectly cannot reject H0 that both come from the same distribution

#### Variable importance evaluation ####

#two-step procedure:

#1st step: check linear models

#for A
glm.modelA <- glm(education ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = A) 
summary(glm.modelA, correlation=F)
step.glm.modelA <- step(glm.modelA, trace=F)
step.glm.modelA$anova

eta.fcn(step.glm.modelA)

#for B

glm.modelB <- glm(income ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = B) 
summary(glm.modelB, correlation=F)
step.glm.modelB <- step(glm.modelB, trace=F)
step.glm.modelB$anova

eta.fcn(step.glm.modelB)

#2nd step: nonlinear regression tools

##### Fitting Random forest #####

#for A
forestA <- randomForest(factor(education, ordered = T) ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = A, importance = T, corr.bias = T)

pdf('forestA.pdf',height=4, width=6)
varImpPlot(forestA,type=2, main = "", labels=c("Gender", "Ever divorced", "Unempl. benefit", "Exp. unempl.", "YoB", "Work exp.", "Pension entitl."))
dev.off()

(VI_FA <- importance(forestA, type=2, scale = F))

barplot(t(VI_FA/sum(VI_FA)))

#tree <- getTree(forestA,1,labelVar=TRUE)
#d <- to.dendrogram(tree)
#str(d)
#plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))

#for B

forestB <- randomForest(income ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = B,importance = T, corr.bias = T)
pdf('forestB.pdf',height=4, width=6)
varImpPlot(forestB,type=2, main = "", label = c("Ever divorced", "Gender", "Exp. unempl.", "Unempl. benefit", "Yob", "Work exp.", "Pension entitl."))
dev.off()

(VI_FB <- importance(forestB, type=2, scale = F))

barplot(t(VI_FB/sum(VI_FB)))

# divorced is discarded in both variable selection models
# sex also has very low importance -> take both as don.classes 

soepdescr <- soep %>% 
  mutate(age = 2015 -gbja) %>% 
  select(-persnr, -gbja, -rentenbeginn) %>% 
  mutate(female = as.numeric(ifelse(as.numeric(sex)==2,0,1))) %>% 
  mutate(everdivorced = as.numeric(as.numeric(divorced)==2,0,1))

#Latex tables
names(soepdescr) <- c("sex", "Pension entitlements", "Education", "Income", "Unempl. benefit", "Exp. unempl.", "divorced", "Work exp.", "Age", "Female", "Ever divorced")
stargazer(soepdescr, out = "descriptives.tex", title = "Chosen descriptive statistics of the passive SOEP sample in 2015 with historic information",
          digits = 2, notes = "Author's calculations based on SOEP v.33 passive West German population in 2015.", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "descrtable", notes.align = "l", summary.logical=T)

##### Perform Matching ####
# define necessary string values
X.vars <- intersect(names(A), names(B))
Y.vars <- setdiff(names(A), names(B))
Z.vars <- setdiff(names(B), names(A))
X.mtc <- c("rente_2015_gesamt","gbja" , "unempben", "expwork", "expunempl")
donclass <- c("sex", "divorced")
xz.vars <- c(X.mtc, "income" )
xyz.vars <- c(X.mtc, Y.vars, "income" )

stargazer(X.mtc, summary = F, out = "matchingvariables.tex")

# Hellinger Distance for matching variable quality
A.mtc <- select(A, one_of(X.mtc))
B.mtc <- select(B, one_of(X.mtc))

dist <- sapply(as.list(X.mtc), function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

A <- A %>% 
  select(-a)

##### simulation starts here ######

fraction <- nrow(soep) / nrow(vskt.mp) 

#1000 repetitions: set this lower for test run!
rep <- 1000
A_k <- as.list.data.frame(replicate(rep, sample_frac(A, fraction, replace = F), simplify = F))
names(A_k) <- 1:rep

distfuns1 <- list("hungarian" = c("hungarian",1), "lpsolve" = c("lpSolve", 5))
distfuns2 <- list("mahalanobis distance" = "Mahalanobis","minimax distance" ="minimax", "gower distance" = "Gower")
randomfuns1 <- list("cutdon.rot" = "rot", "cutdon.min" = "min")
randomfuns2 <- list("mahalanobis distance" = "Mahalanobis","minimax distance" ="minimax", "gower distance" = "Gower", "ann distance" = "ANN")
rankvar <- "rente_2015_gesamt"  

distancematch <- montecarlofunc(routine = "distance",list1 = distfuns1, list2 = distfuns2, FUN = distancehd)

randommatch <- montecarlofunc(routine = "random", list1=randomfuns1, list2=randomfuns2, FUN = randomhd)

rankmatch <- montecarlofunc(routine = "rank", list1 = distfuns1, FUN = rankhd)

simlist <- list("distancematch" = distancematch , "randommatch" = randommatch, "rankmatch" = rankmatch)  

save(simlist, file= "simulation_fused.RDA")

#load("simulation_fused.RDA")

##### 4th level ######
ksB <- select(B, one_of(xz.vars))
xzlist <- list("pension" = xz.vars[1], "birthyear" = xz.vars[2], "unemplben" = xz.vars[3],
               "workexp" = xz.vars[4], "unempexp" = xz.vars[5], "income" = xz.vars[6])

#for distance just use out = "statistic" which is the ks.distance!
#and compare actual fit via kolmogorov-smirnov distance

ksfuseddist <- KS.match(routine = "distance", list1 = distfuns1, list2 = distfuns2, out = "statistic")
ksfusedrandom <- KS.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2, out = "statistic")
ksfusedrank <- KS.match(routine = "rank", list1 = distfuns1, out = "statistic")

kslist <- list("distanceks" = ksfuseddist , "randomks" = ksfusedrandom, "rankks" = ksfusedrank)  

distance.output <- ksoutput(routine = "distance",  list1 = distfuns1, list2 = distfuns2)
random.output <- ksoutput(routine = "random",  list1 = randomfuns1, list2 = randomfuns2)
rank.output <-  ksoutput(routine = "rank",  list1 = distfuns1)


#Power of test:
#divide 1 - number of rejected Null hypothesis by number of tests as average over routines

#draw p.value
ksdistp <- KS.match(routine = "distance", list1 = distfuns1, list2 = distfuns2, out = "p.value")
ksdistpower <- kspower(routine = "distance")

ksrandp <- KS.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2, out = "p.value")
ksrandpower <- kspower(routine = "random")

ksrankp <- KS.match(routine = "rank", list1 = distfuns1, out = "p.value")
ksrankpower <- kspower(routine = "rank")


#latex tables
stargazer(rbind(distance.output,ksdistpower), summary = F, title = "Mean over 100 Monte Carlo draws of Kolmogorov-Smirnov distance for Distance Hot Deck Matching routines",
          out = "ksdist.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)

stargazer(rbind(random.output,ksrandpower), summary = F, title = "Mean over k Monte Carlo draws of Kolmogorov-Smirnov distance for Random Distance Hot Deck Matching routines",
          out = "ksrand.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)

stargazer(rbind(rank.output,ksrankpower), summary = F, title = "Mean over k Monte Carlo draws of Kolmogorov-Smirnov distance for Rank Hot Deck Matching routines",
          out = "ksrank.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)

##### 3rd level ##### 
#Correlation matrices:

xyz.vars <- c(X.mtc, Y.vars, "income" )
corrmatfull <- cor(select(soep, one_of(xyz.vars)))

correlationsimdistance <- corr.match(routine = "distance", list1 = distfuns1, list2 = distfuns2)
correlationsimrandom <- corr.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
correlationsimrank <- corr.match(routine = "rank", list1 = distfuns1)
                    
correlationlist <- list("distancecorr" = correlationsimdistance , "randomcorr" = correlationsimrandom, "rankcorr" = correlationsimrank)  

corredist <- montecarlocorr(routine = "distance", list1 = distfuns1, list2 = distfuns2)
correrandom <- montecarlocorr(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
correrank <- montecarlocorr(routine = "rank", list1 = distfuns1)

corrlist <- list("distancecorr" = corredist , "randomcorr" = correrandom, "rankcorr" = correrank) 

# create output table
distcorroutput <- corroutput(routine = "distance", list1 = distfuns1, list2 = distfuns2)
randomcorroutput <- corroutput(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
rankcorrmeanoutput <- corroutput(routine = "rank", list1 = distfuns1)

# Power of test again
# divide 1 - number of rejected Null hypothesis by number of tests in each coefficient and the weighted average!
#draw p.value
corrdistpower <- corrpower(routine = "distance")
corrrandpower <- corrpower(routine = "random")
corrrankpower <- corrpower(routine = "rank")

# latex tables
stargazer(rbind(distcorroutput,corrdistpower), summary = F, title = "Mean over k Monte Carlo draws of Fischer's Correlation test for Distance Hot Deck Matching routines",
          out = "corrdist.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)

stargazer(rbind(randomcorroutput,corrrandpower), summary = F, title = "Mean over k Monte Carlo draws of Fischer's Correlation test for Random Distance Hot Deck Matching routines",
          out = "corrrand.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)

stargazer(rbind(rankcorrmeanoutput,corrrankpower), summary = F, title = "Mean over k Monte Carlo draws of Fischer's Correlation test for Rank Hot Deck Matching routines",
          out = "corrrank.tex", colnames = T, digits = 3, digits.extra = 3, flip = F, initial.zero = T, multicolumn = T, rownames =T, perl=T)


#### 2nd level ####

# if windows: 
# initiate parallel processing: 
#library(doParallel) 
no_cores <- detectCores() - 1  #minus 1 so that the machine can still perform pasic tasks
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores, type="PSOCK") # if not on windows put in here "FORK" or use the llply parallel==T version in the functions I provide
clusterExport(cl=cl, varlist=c("simlist", "soep", "xyz.vars", "factorsNumeric", "asNumeric", "mvartest", "distfuns1", "distfuns2", "xyz.match", "randomfuns1", "randomfuns2"),envir=environment())
clusterEvalQ(cl, list(library(cramer), library(dplyr), library(utils), library(stats), factorsNumeric, asNumeric, mvartest, xyz.match))


#Remark: Computational intensive 
#---------------------------------------------------------#
# if on a windows machine use this function: xyz.match.win
#---------------------------------------------------------#
# instead of xyz.match 

xyztestdist <- xyz.match(routine = "distance", list1 = distfuns1, list2 = distfuns2)
save(xyztestdist, file = "cramerdist.RDA")
xyztestrand <- xyz.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
save(xyztestrand, file = "cramerrand.RDA")
xyztestrank <- xyz.match(routine = "rank", list1 = distfuns1)
save(xyztestrank, file = "cramerrank.RDA")

stopCluster(cl)  


#data frame and latex tables:

xyztestdistdf <- xyztestdf(data =xyztestdist, routine = "distance", list1 = distfuns1, list2 = distfuns2)
names(xyztestdistdf) <- c("HUmahalanobis", "HUminimax", "HUgower","lpmahalanobis", "lpminimax", "lpgower")
stargazer(xyztestdistdf, title = "Multivariate cramer test for distributional equality after distance hot deck matching", out = "xyzdist.tex", omit.summary.stat = c("p25", "p75"))

xyztestranddf <- xyztestdf(data =xyztestrand, routine = "random", list1 = randomfuns1, list2 = randomfuns2)
names(xyztestranddf) <- c("rotmahalanobis", "rotminimax", "rotgower","rotANN", "minmahalanobis", "minminimax", "mingower", "minANN")
stargazer(xyztestranddf, title = "Multivariate cramer test for distributional equality after random distance hot deck matching", out = "xyzrand.tex",  omit.summary.stat = c("p25", "p75"))

xyztestrankdf <- xyztestdf(data =xyztestrank, routine = "rank", list1 = distfuns1)
names(xyztestrankdf) <- c("hungarian", "lpsolve")
stargazer(xyztestrankdf, title = "Multivariate cramer test for distributional equality after rank hot deck matching", out = "xyzrank.tex",  omit.summary.stat = c("p25", "p75"))


# finally construct testpower: as number of H0 / rep per routine

disttestpower <- t(rowSums(t(as.matrix(xyztestdistdf)) >pvalue) / rep)
randtestpower <- t(rowSums(t(as.matrix(xyztestranddf)) >pvalue) / rep)
ranktestpower <- t(rowSums(t(as.matrix(xyztestrankdf)) >pvalue) / rep)

#### Simulation finishes here ####


##### 1st level ####
# not used because not useful for application

#order by persnr and persnrB then create indicator variable which is one if two variables are different
arrange(fused32, persnr, persnrB)
eval <- fused32$persnr == fused32$persnrB
# choose smallest FALSE!
firstlevel32 <- table(eval)

