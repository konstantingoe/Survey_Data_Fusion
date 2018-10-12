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

soepA <- select(soep, -income)
B <- select(soep, -education)

set.seed(1234)
A <- sample_frac(soepA, 0.3, replace = F)

(X.vars <- intersect(names(A), names(B)))
(Y.vars <- setdiff(names(A), names(B)))
(Z.vars <- setdiff(names(B), names(A)))

A <- A %>% 
  mutate(a=1) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T))

B <- B %>% 
  mutate(a=0) %>% 
  mutate(gbja_cat = factor(gbja_cat, ordered = T))  

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

##### Fitting Regression Tree #####






##### Perform Matching ####

X.mtc <- c("","")

# another idea... compare regressing income on education in the hopefully
# properly matched (A U B) file with the later matchen (SOEP U VSKT).
# parameter estimates should be similar when CIA is supposed to hold
# for this look up how to test conditional independence in the full SOEP file!





