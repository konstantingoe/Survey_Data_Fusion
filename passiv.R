rm(list=ls())

source("packages.R")
source("functions.R")
source(".path.R")


soep.mp <- import(paste(path, "vskt_passiv_panel_3.dta" , sep = "/"), setclass = "data.table")

vskt.mp <- import(paste(path, "vskt_passiv_panel_2.dta" , sep = "/"), setclass = "data.table")

(X.vars <- intersect(names(soep.mp), names(vskt.mp)))

soep.mp <- soep.mp %>% 
  mutate(soep=1)

vskt.mp <- vskt.mp %>% 
  mutate(soep=0)

joint <- bind_rows(soep.mp, vskt.mp)
joint <- joint %>% 
  mutate(soep = factor(soep, ordered = F)) 

(birthplot <- mydensplot(joint, "gbja", xname = "Geburtskohorte"))
ggsave("geburtsjahr_mp.pdf")

ks.test(soep.mp$gbja, vskt.mp$gbja, alternative = "two.sided")


(rentenplot <- mydensplot(joint, "rente_2015_gesamt", xname = "Rente jährlich in €"))
ggsave("rente_mp.pdf")
ks.test(soep.mp$rente_2015_gesamt, vskt.mp$rente_2015_gesamt, alternative = "two.sided")



