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

# todo: go into stata and reproduce rentenfile with income information as well...

#### Sample Selection ####

soep <- soep %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T)) %>% 
  mutate(sex = factor(sex, ordered = F))

soepA <- select(soep, -income)
B <- select(soep, -education)

set.seed(1234)
A <- sample_frac(soepA, 0.3, replace = F)

A <- A %>% 
  mutate(a=1)

B <- B %>% 
  mutate(a=0) %>% 
  mutate(persnrB = persnr)

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

glm.modelA <- glm(education ~ sex + gbja + rente_2015_gesamt + exp_arbeit, data = A) 
summary(glm.modelA, correlation=F)
step.glm.modelA <- step(glm.modelA, trace=F)
step.glm.modelA$anova

eta.fcn(step.glm.modelA)

#2nd step: nonlinear regression tools
##### Fitting Regression Tree #####

tree.A <- tree(education ~ sex + gbja + rente_2015_gesamt + exp_arbeit, 
               data = A)
summary(tree.A)
plot(prune.tree(tree.A))

# maybe also random forest...

 
# confirms the use of rente_2015_gesamt and gbja as matching variables!

##### Perform Matching ####

X.mtc <- c("rente_2015_gesamt","gbja")
donclass <- "sex"


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

### Income post matching
(birthplot.post <- mydensplot.post.sim(joint.post, "income", xname = "Income in €"))#,lmts =c(1, 25000)))
ks.test(fused.1$income, B$income, alternative = "two.sided")

# but marginal distributions not preserved!

# need a crosstable of where match ids correspond to original ids!

#order by persnr and persnrB then create indicator variable which is one if two variables are different
arrange(fused.1, persnr, persnrB)

eval <- fused.1$persnr == fused.1$persnrB
# choose smallest FALSE!
table(eval)

# find algorithm that maximizes TRUE!!
#write.dta(fused.1, file = "match_1.dta")



#### checking correlation structure ####

# also correlation structure not preserved!

# unlikely that the correlation structure has been preserved!
# but maybe control for random sample..
set.seed(1234)
soep.small <- sample_frac(soep, 0.3, replace = F)

lmjoint <- lm(income ~ education + sex + gbja + rente_2015_gesamt + exp_arbeit, 
              data = soep.small)
summary(lmjoint)
lmfused <- lm(income ~ education + sex + gbja + rente_2015_gesamt + exp_arbeit, 
              data = fused.1)
summary(lmfused)

# no still not

# another idea... compare regressing income on education in the hopefully
# properly matched (A U B) file with the later matchen (SOEP U VSKT).
# parameter estimates should be similar when CIA is supposed to hold
# for this look up how to test conditional independence in the full SOEP file!



