####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

set.seed(1234)


soep <- import(paste(path, "soep_passive_ges.dta" , sep = "/"), setclass = "data.table")
vskt.mp <- import(paste(path, "vskt_passiv_panel_ges.dta" , sep = "/"), setclass = "data.table")

# perform simulation on the basis of soep
# for this use soep twice delete in each set variables and keep X as common variables
# then take data set A and randomly drop 2/3 of the data... then match the full dataset B with 
# (X,Z) onto the smaller data set A with (X,Y).
# repeat this 10.000 times!

#possibly divide script into part that is fix and part that relies on simulation:


#### Sample Selection ####

soep <- soep %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(sex = factor(sex, ordered = F)) %>% 
  mutate(divorced = factor(divorced, ordered = F)) %>% 
  select(-gbja_cat, - pwgt) # not useful here for retired population

# CIA assumption
CItest <- ci.test(x = "income", y = "education", z = c("sex","gbja", "rente_2015_gesamt","expunempl", "unempben" ,"expwork" , "divorced"), data = soep, test = "mi-cg")
# does not hold!

#create A and B

A <- select(soep, -income)
B <- select(soep, -education)

A <- A %>% 
  mutate(a=1)

B <- B %>% 
  mutate(a=0) %>% 
  mutate(persnrB = persnr)


##### Checking densities #####
joint <- bind_rows(A, B)
joint <- joint %>% 
  mutate(a = factor(a, ordered = F))


#(birthplot <- mydensplot.sim(joint, "gbja", xname = "Geburtskohorte"))
#(kstest.gbja <- ks.test(A$gbja, B$gbja, alternative = "two.sided"))
#works perfectly cannot reject H0 that both come from the same distribution

# At this stage, since the population equality assumption is not a problem 
# anymore it might be sensical to fit a regression tree in order to check 
# which matching variables to use:

# two-step procedure:

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
varImpPlot(forestA,type=2, main = "")
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
varImpPlot(forestB,type=2, main = "")
dev.off()

sort(imp, decreasing = T) 

(VI_FB <- importance(forestB, type=2, scale = F))

barplot(t(VI_FB/sum(VI_FB)))

# divorced is discarded in both variable selection models
# sex also has very low importance -> take both as don.classes 
soepdescr <- soep %>% 
  mutate(age = 2016 -gbja) %>% 
  select(-persnr, -gbja, -rentenbeginn)
names(soepdescr) <- c("Gender", "Pension Entitlements", "Exp. unempl.","Unempl. Benefit", "Education", "Income", "Exp. empl.", "Divorced", "Age")
stargazer(soepdescr, out = "descriptives.tex", title = "Chosen descriptive statistics of the passive SOEP sample in 2016 with historic information",
          digits = 0, notes = "Author's calculations based on SOEP v.33 passive West German population in 2016.", summary.stat = c("n", "mean","sd", "median", "min", "max"), label = "descrtable", notes.align = "l")

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

# need function that draws random sample from A, 10.000 times and performs all sorts of matching on each of those samples and stores them in a list

# 3 repetitions:
rep <- 5
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


##### 4th level ######
ksB <- select(B, one_of(xz.vars))
xzlist <- list("pension" = xz.vars[1], "birthyear" = xz.vars[2], "unemplben" = xz.vars[3],
               "workexp" = xz.vars[4], "unempexp" = xz.vars[5], "income" = xz.vars[6])

# for distance just use out = "statistic" which is the ks.distance!
# and compare actual fit via kolmogorov-smirnov distance

ksfuseddist <- KS.match(routine = "distance", list1 = distfuns1, list2 = distfuns2, out = "statistic")
ksfusedrandom <- KS.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2, out = "statistic")
ksfusedrank <- KS.match(routine = "rank", list1 = distfuns1, out = "statistic")

kslist <- list("distanceks" = ksfuseddist , "randomks" = ksfusedrandom, "rankks" = ksfusedrank)  

summdist <- ksaggregate(routine = "distance",list1 = distfuns1, list2 = distfuns2)
summrand <- ksaggregate(routine = "random",list1 = randomfuns1, list2 = randomfuns2)
summrank <- ksaggregate(routine = "rank",list1 = distfuns1)

fullks <- rbind("summdist" = summdist, "summrand" = summrand, "summrank" = summrank)
routiname <-  c("hungarian mahalanobis distance mean", "hungarian minimax distance mean", "hungarian gower distance mean", 
                "lpSolve mahalanobis distance mean", "lpSolve minimax distance mean", "lpSolve gower distance mean",
                "cutrot mahalanobis random mean", "cutrot minimax random mean", "cutrot gower random mean", "cutrot ann random mean",
                "cutmin mahalanobis random mean", "cutmin minimax random mean", "cutmin gower random mean", "cutmin ann random mean",
                "hungarian rank mean", "lpSolve rank mean")
rownames(fullks) <- routiname
fullksmat <- t(fullks)
xtable(fullksmat, caption = "Mean over k Monte Carlo draws of Kolmogorov-Smirnov distance for several matching routines",
                          digits = 3, auto = T)

stargazer(fullksmat, summary = F, title = "Mean over k Monte Carlo draws of Kolmogorov-Smirnov distance for several matching routines",
                      out = "ks.tex", colnames = T, digits = 4, flip = F, initial.zero = T, multicolumn = T, rownames =T)


stargazer(summdist, summary = F, title = "Mean over k Monte Carlo draws of Kolmogorov-Smirnov distance for Hot Deck Distance Matching",
          colnames = T, digits = 4, flip = T, initial.zero = T, multicolumn = F,
          rownames =T, se = as.list(summdist[2,], summdist[4,],summdist[6,],summdist[8,],summdist[10,],summdist[12,]))
# find the number of rejected null hypotheses 



#######3rd level:
#### Correlation matrix #####

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

distcorrmean <- corraggregate(routine = "distance", list1 = distfuns1, list2 = distfuns2)
randomcorrmean <- corraggregate(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
rankcorrmean <- corraggregate(routine = "rank", list1 = distfuns1)

fullcorr <- rbind("distcorrmean" = distcorrmean, "randomcorrmean" = randomcorrmean, "rankcorrmean" = rankcorrmean)
rownames(fullcorr) <- routiname

fullcorrchoice <- t(select(fullcorr, contains("income")))

#also here find a way to aggregate and present

######2nd level:

#takes a long long time!
# but works great!

xyztestdist <- xyz.match(routine = "random", list1 = distfuns1, list2 = distfuns2)
xyztestrand <- xyz.match(routine = "random", list1 = randomfuns1, list2 = randomfuns2)
xyztestrank <- xyz.match(routine = "rank", list1 = distfuns1)


xyztestlist <- list("distancxyz" = xyztestdist , "randomxyz" = xyztestrand, "rankxyz" = xyztestrank)  

xyztestrankdf <- cbind(ldply(xyztestrank2$hungarian), ldply(xyztestrank2$lpsolve, .id = NULL))
names(xyztestrankdf) <- c("repetitions", "hungarian", "lpsolve")

stargazer(xyztestrankdf)
# done:
# then aggregate and report thats it

##### 1st evaluation level ####
#order by persnr and persnrB then create indicator variable which is one if two variables are different
arrange(fused32, persnr, persnrB)

eval <- fused32$persnr == fused32$persnrB
# choose smallest FALSE!
firstlevel32 <- table(eval)

