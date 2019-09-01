clear all
set more off
set trace off
set maxvar 32000
cap log close


* Konsi homeoffice

*global do "/Users/kgoebler/Desktop/Masterthesis/do/"
global data "/Users/kgoebler/Desktop/Lebenseinkommen Projekt/data/"
global original_wide "/Users/kgoebler/Desktop/Lebenseinkommen Projekt/stata/"


* Konsi DIW
global do "S:\STUD\kgoebler\Lebenseinkommen\do\"
global data "S:\STUD\kgoebler\Lebenseinkommen\data\"
global grafik "S:\STUD\kgoebler\Lebenseinkommen\grafik\"
global original_long "V:\distribution\soep-core\soep.v34\stata"
global original_wide "V:\distribution\soep-core\soep.v34\stata\"
global original_wide_old "S:\data\soep30_de\stata\"
global log "S:\STUD\kgoebler\Lebenseinkommen\log\"

* Konsi Ext. Drive
global original_wide "V:\distribution\soep-core\soep.v34\stata\"
global original_wide_raw "V:\distribution\soep-core\soep.v34\stata\raw\"
global do "E:\Lebenseinkommen Projekt\do_new\"
global data "E:\Lebenseinkommen Projekt\"



qui ssc install kmatch, replace
qui ssc install kdens, replace
qui ssc install psmatch2, replace
