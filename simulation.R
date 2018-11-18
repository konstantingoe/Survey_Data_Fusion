####### Initiate #####
rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")

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
CItest <- ci.test(x = "income", y = "education", z = c("sex","gbja", "rente_2015_gesamt","expunempl", "unempben" ,"expwork" , "divorced"), data = soep)
# does not hold!

#create A and B

soepA <- select(soep, -income)
B <- select(soep, -education)

# choose fractions corresponding to fraction of SOEP vs. VSKT 
fraction <- nrow(soep) / nrow(vskt.mp) 
set.seed(1234)
A <- sample_frac(soepA, fraction, replace = F)
# having checked that for several draws the general structure for matching variable choice does not change

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

forestA <- randomForest(education ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = A)
varImpPlot(forestA,type=2)

(VI_FA <- importance(forestA, type=2, scale = F))

barplot(t(VI_FA/sum(VI_FA)))

#tree <- getTree(forestA,1,labelVar=TRUE)
#d <- to.dendrogram(tree)
#str(d)
#plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))

#for B


forestB <- randomForest(income ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = B)
varImpPlot(forestB,type=2)

(VI_FB <- importance(forestB, type=2, scale = F))

barplot(t(VI_FB/sum(VI_FB)))

# divorced is discarded in both variable selection models
# sex also has very low importance -> take both as don.classes 

##### Perform Matching ####
# define necessary string values
X.vars <- intersect(names(A), names(B))
Y.vars <- setdiff(names(A), names(B))
Z.vars <- setdiff(names(B), names(A))
X.mtc <- c("rente_2015_gesamt","gbja" , "unempben", "expwork", "expunempl")
donclass <- c("sex", "divorced")
xz.vars <- c(X.mtc, "income" )
xyz.vars <- c(X.mtc, Y.vars, "income" )


# Hellinger Distance for matching variable quality
A.mtc <- select(A, one_of(X.mtc))
B.mtc <- select(B, one_of(X.mtc))

dist <- sapply(as.list(X.mtc), function(y) tryCatch({hellinger(A.mtc[,y],B.mtc[,y], lower = 0, upper = Inf, method = 1) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))


##### simulation starts here ######

# need function that draws random sample from soepA, 10.000 times and perforems all sorts of matching on each of those samples and stores them in a list

# 3 repetitions:
rep <- 3
A_k <- as.list.data.frame(replicate(rep, sample_frac(soepA, fraction, replace = F), simplify = F))
names(A_k) <- 1:rep

distfuns1 <- list("hungarian" = c("hungarian",1), "lpsolve" = c("lpSolve", 5))
distfuns2 <- list("mahalanobis distance" = "Mahalanobis","minimax distance" ="minimax", "gower distance" = "Gower")
distancematch <- lapply(distfuns1, function(y) 
                    lapply(distfuns2, function(z) 
                      lapply(A_k, function(x) distancehd(x,
                             B, distfun = z, constr = y))))

randomfuns1 <- list("cutdon.rot" = "rot", "cutdon.min" = "min")
randomfuns2 <- list("mahalanobis distance" = "Mahalanobis","minimax distance" ="minimax", "gower distance" = "Gower", "ann distance" = "ANN")
randommatch <- lapply(randomfuns1, function(y) 
                lapply(randomfuns2, function(z) 
                  lapply(A_k, function(x) randomhd(x,
                    B, distfun = z, cutdon = y))))

rankvar <- "rente_2015_gesamt"  
rankmatch <- lapply(distfuns1, function(y) 
                lapply(A_k, function(x) rankhd(x,
                  B, constr = y)))


simlist <- list("distancematch" = distancematch , "randommatch" = randommatch, "rankmatch" = rankmatch)  

save(simlist, file= "simulation_fused.RDA")


##### 4th level ######
ksB <- select(B, one_of(xz.vars))
xzlist <- list("pension" = xz.vars[1], "birthyear" = xz.vars[2], "unemplben" = xz.vars[3],
               "workexp" = xz.vars[4], "unempexp" = xz.vars[5], "income" = xz.vars[6])

ksfuseddist <- setNames(lapply(seq_along(distfuns1), function(w)
            setNames(lapply(seq_along(distfuns2), function(s) 
              setNames(lapply(1:rep, function(r) 
                lapply(xzlist, function(t) ks.test(select(
                  simlist$distancematch[[w]][[s]][[r]], one_of(xz.vars))[,t], ksB[,t], 
                    alternative = "two.sided")$p.value)),
                      names(A_k))),names(distfuns2))),names(distfuns1))

ksfusedrandom <- setNames(lapply(seq_along(randomfuns1), function(w)
                  setNames(lapply(seq_along(randomfuns2), function(s) 
                    setNames(lapply(1:rep, function(r) 
                      lapply(xzlist, function(t) ks.test(select(
                        simlist$randommatch[[w]][[s]][[r]], one_of(xz.vars))[,t], ksB[,t], 
                          alternative = "two.sided")$p.value)),
                            names(A_k))),names(randomfuns2))),names(randomfuns1))

ksfusedrank <- setNames(lapply(seq_along(distfuns1), function(w)
                  setNames(lapply(1:rep, function(r) 
                    lapply(xzlist, function(t) ks.test(select(
                      simlist$rankmatch[[w]][[r]], one_of(xz.vars))[,t], ksB[,t], 
                        alternative = "two.sided")$p.value)),
                          names(A_k))),names(distfuns1))

kslist <- list("distanceks" = ksfuseddist , "randomks" = ksfusedrandom, "rankks" = ksfusedrank)  

ksdf <- setNames(lapply(seq_along(distfuns1), function(w)
          setNames(lapply(seq_along(distfuns2), function(z) 
            ldply(kslist$distanceks[[w]][[z]], data.frame, .id = "repetitions")), names(distfuns2))),names(distfuns1))

summarystat <- simSumm(data=df)

### figure out whats going wrong 
summ <- data.frame(lapply(select(ksdf$hungarian$`mahalanobis distance`, -repetitions), 
                 function(x) rbind(mean = mean(x))))

#### find way of displaying mean and variance of each of the ks values


#######3rd level:
#### Correlation matrix #####

xyz.vars <- c(X.mtc, Y.vars, "income" )
corrmatfull <- cor(select(soep, one_of(xyz.vars)))

correlationsimdistance <- setNames(lapply(seq_along(distfuns1), function(g) 
                            setNames(lapply(seq_along(distfuns2), function(s)
                              setNames(lapply(1:rep, function(z) corrtestmat(
                                corrmatfull, cor(select(simlist$distancematch[[g]][[s]][[z]], 
                                  one_of(xyz.vars))))),names(A_k))),names(distfuns2))), names(distfuns1))

correlationsimrandom <- setNames(lapply(seq_along(randomfuns1), function(g) 
                          setNames(lapply(seq_along(randomfuns2), function(s)
                            setNames(lapply(1:rep, function(z) corrtestmat(
                              corrmatfull, cor(select(simlist$randommatch[[g]][[s]][[z]], 
                                one_of(xyz.vars))))),names(A_k))),names(randomfuns2))), names(randomfuns1))

correlationsimrank <- setNames(lapply(seq_along(distfuns1), function(g) 
                        setNames(lapply(1:rep, function(z) corrtestmat(
                          corrmatfull, cor(select(simlist$rankmatch[[g]][[z]], 
                            one_of(xyz.vars))))),names(A_k))), names(distfuns1))
                    
correlationlist <- list("distancecorr" = correlationsimdistance , "randomcorr" = correlationsimrandom, "rankcorr" = correlationsimrank)  

#also here find a way to aggregate and present

######2nd level:

#takes a long long time!
# but works great!

xyztestdist <- setNames(lapply(seq_along(distfuns1), function(g) 
            setNames(lapply(seq_along(distfuns2), function(s)
              setNames(lapply(1:rep, function(z) mvartest(A=soep, 
                B=simlist$distancematch[[g]][[s]][[z]])$p.value),
                  names(A_k))),names(distfuns2))), names(distfuns1))

xyztestrand <- setNames(lapply(seq_along(randomfuns1), function(g) 
            setNames(lapply(seq_along(randomfuns2), function(s)
              setNames(lapply(1:rep, function(z) mvartest(A=soep, 
                B=simlist$randommatch[[g]][[s]][[z]])$p.value),
                  names(A_k))),names(randomfuns2))), names(randomfuns1))

xyztestrank <- setNames(lapply(seq_along(distfuns1), function(g) 
                setNames(lapply(1:rep, function(z) mvartest(A=soep, 
                  B=simlist$rankmatch[[g]][[z]])$p.value),
                    names(A_k))), names(distfuns1))


xyztestlist <- list("distancxyz" = xyztestdist , "randomxyz" = xyztestrand, "rankxyz" = xyztestrank)  

# done:
# then aggregate and report thats it

fused.1 <- fused.1 %>% 
  mutate(b=0)

B <- B %>% 
  mutate(b=1)

joint.post <- bind_rows(fused.1, B)

joint.post <- joint.post %>% 
  mutate(b=factor(b,ordered = F))

###### Post matching #####

######  4th validation level #####
# visually:
(birthplot.post <- mydensplot.post.sim(joint.post, "gbja", xname = "Income in €"))#,lmts =c(1, 25000)))

# statistically
# we can check both whether all X are marginally preserved, 
# marginals by ks.test

xz.vars <- c(X.mtc, "income" )

ksfused <- select(fused.1, one_of(xz.vars))
ksB <- select(B, one_of(xz.vars))

kstest <- rbind(xz.vars,sapply(as.list(xz.vars), function(x) ks.test(ksfused[,x], ksB[,x], alternative = "two.sided")$p.value))
rownames(kstest, do.NULL = TRUE, prefix = "row")
rownames(kstest) <- c("Variables","Kolmogorov-Smirnov p-value")

# and whether X,Z has been maintained 
# use generalized KS test <- Peacock test
#that's a lot of combinations! <- 15 tests!

combinationsmat <- combinations(n = 6, r = 2, v = xz.vars, set=T, repeats.allowed = F)

distequaltest <- vector(length = 15)
xznames <- vector(length = 15) 
for (i in seq_row(combinationsmat)){
  distequaltest[i] <-cramer.test(as.matrix(select(ksfused, one_of(c(combinationsmat[1,1:2])))), as.matrix(select(ksB, one_of(c(combinationsmat[1,1:2])))))$p.value
}
save(distequaltest, file = "xztest.RDA")
for (i in seq_row(combinationsmat)){
  xznames[i] <- paste(c(combinationsmat[i,1], "vs", combinationsmat[i,2]), collapse="  ")
}

distequalmat <- rbind(xznames, distequaltest)

#### 3rd evluation level ####

#### Correlation matrix approach #####
xyz.vars <- c(X.mtc, Y.vars, "income" )
soepcorr <- select(soep, one_of(xyz.vars))
fused1corr <- select(fused.1, one_of(xyz.vars))
corrmatfull <- round(cor(soepcorr),2)
corrmatfused <- round(cor(fused1corr),2)

corrtestmat <- matrix(NA, nrow=7, ncol=7)
for (i in 1:nrow(corrtestmat)) {
  for (j in 1:ncol(corrtestmat)) {
    corrtestmat[i,j] <- get.cocor.results(corrtest(corrmatfull[i,j],corrmatfused[i,j]))$fisher1925$p.value
  }
}
corrtestmat[upper.tri(corrtestmat, diag = T)] <- NA

# cannot reject the H0 that any of the correlation values are identical

##### 2nd evaluation level #####

# use cramer test to check f(x,y,z) has been preserved
# we cannot include dichotome variables

v <- factorsNumeric(select(soep, one_of(xyz.vars)))
w <- factorsNumeric(select(fused.1, one_of(xyz.vars)))

#takes a long long time!
# but works great!
xyztest <- cramer.test(as.matrix(v), as.matrix(w))


##### 1st evaluation level ####
#order by persnr and persnrB then create indicator variable which is one if two variables are different
arrange(fused32, persnr, persnrB)

eval <- fused32$persnr == fused32$persnrB
# choose smallest FALSE!
firstlevel32 <- table(eval)

