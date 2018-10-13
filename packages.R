# Preparing R -------------------------------------------------------------

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  broom,
  car,
  effects,
  foreign,
  ggeffects,
  haven,
  influence.ME,
  interplot,
  lme4,
  lmtest,
  lubridate,
  lm.beta,
  pastecs,
  data.table,
  plyr,
  plm,
  pglm,
  readstata13,
  readxl,
  rlang,
  sandwich,
  sjlabelled,
  sjmisc,
  sjPlot,
  sjstats,
  stargazer,
  strucchange,
  rio,
  tree,
  bnlearn,
  rpart,
  gridExtra,
  cowplot,
  randomForest,
  dplyr,
  StatMatch,
  lpSolve,
  Hmisc,
  MASS,
  reshape2,
  # those beneath always load last
  ggplot2,
  tidyverse)


#options(scipen = 100)
options(digits = 2)
