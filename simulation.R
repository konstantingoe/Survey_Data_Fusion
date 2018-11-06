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

# use Hellinger distance as a dissimilarity measure of the X

# todo: go into stata and reproduce rentenfile with income information as well...

#### Sample Selection ####

soep <- soep %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(sex = factor(sex, ordered = F))

# CIA assumption
CItest <- ci.test(x = "income", y = "education", z = c("sex","gbja", "rente_2015_gesamt","expunempl", "unempben" ,"expwork" , "divorced"), data = soep)


soepA <- select(soep, -income)
B <- select(soep, -education)

set.seed(1234)
A <- sample_frac(soepA, 0.3, replace = F)

A <- A %>% 
  mutate(a=1)

B <- B %>% 
  mutate(a=0) %>% 
  mutate(persnrB = persnr)

felix hats raus

(X.vars <- intersect(names(A), names(B)))
(Y.vars <- setdiff(names(A), names(B)))
(Z.vars <- setdiff(names(B), names(A)))

##### Checking densities #####
joint <- bind_rows(A, B)
joint <- joint %>% 
  mutate(a = factor(a, ordered = F))


(birthplot <- mydensplot.sim(joint, "gbja", xname = "Geburtskohorte"))
(kstest.gbja <- ks.test(A$gbja, B$gbja, alternative = "two.sided"))
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

barplot(t(VI_FA/sum(VI_F)))

#tree <- getTree(forestA,1,labelVar=TRUE)
#d <- to.dendrogram(tree)
#str(d)
#plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))

#for B


forestB <- randomForest(income ~ sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced , data = B)
varImpPlot(forestB,type=2)

(VI_FB <- importance(forestB, type=2, scale = F))

barplot(t(VI_FB/sum(VI_F)))

# divorced is discarded in both variable selection models

##### Perform Matching ####

X.mtc <- c("rente_2015_gesamt","gbja" , "unempben", "expwork", "expunempl")

# Hellinger Distance for matching variable quality

dist <- vector(length  = 5)
dist[1] <- hellinger(A$rente_2015_gesamt,B$rente_2015_gesamt, lower = 0, upper = Inf)
dist[2] <- hellinger(A$gbja,B$gbja)
dist[3] <- hellinger(A$unempben,B$unempben, lower = 0, upper = Inf)
dist[4] <- hellinger(A$expunempl,B$expunempl, lower = 0, upper = Inf)
dist[5] <- hellinger(A$expwork,B$expwork, lower = 0, upper = Inf)


donclass <- c("sex", "divorced")


#nearest neigbor distance hot deck
match.1 <- NND.hotdeck(data.rec=A, data.don=B,
                      match.vars=X.mtc, 
                      don.class = donclass,
                      dist.fun = "minimax",
                      rank = TRUE,
                      constrained = TRUE,
                      constr.alg = "lpSolve",
                      k=5)

fused.1 <- create.fused(data.rec=A, data.don=B,
                       mtc.ids=match.1$mtc.ids,
                       z.vars=Z.vars)

summary(match.1$dist.rd)
# distances are zero

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
(birthplot.post <- mydensplot.post.sim(joint.post, "income", xname = "Income in €"))#,lmts =c(1, 25000)))
# statistically
ks.test(fused.1$income, B$income, alternative = "two.sided")
ks.test(fused.1$gbja, B$gbja, alternative = "two.sided")

# we can check both whether all X are marginally preserved, 
# and whether X,Z has been maintained

# marginals by ks.test

xz.vars <- c(X.mtc, "income" )

ksfused <- select(fused.1, one_of(xz.vars))
ksB <- select(B, one_of(xz.vars))

for (i in 1:6) {
     ks.test(ksfused[,i], ksB[,i], alternative = "two.sided")$p.value
}


#### 3rd evluation level ####

lmjoint <- lm(income ~ education + sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced, 
              data = soep)
summary(lmjoint)
lmfused <- lm(income ~ education + sex + gbja + rente_2015_gesamt + expwork + expunempl + unempben + divorced, 
              data = fused.1)
summary(lmfused)

## not very nice to do this 500 times...need better correlation procedure


##### 2nd evaluation level #####

# use cramer test to check f(x,y,z) has been preserved

xyz.vars <- c(X.mtc, Y.vars, "income" )
# we cannot include dichotome variables

v <- factorsNumeric(select(soep, one_of(xyz.vars)))
w <- factorsNumeric(select(fused.1, one_of(xyz.vars)))

#takes a long long time!
# but works great!
cramer.test(as.matrix(v), as.matrix(w))



##### 1st evaluation level ####
#order by persnr and persnrB then create indicator variable which is one if two variables are different
arrange(fused.1, persnr, persnrB)

eval <- fused.1$persnr == fused.1$persnrB
# choose smallest FALSE!
table(eval)

