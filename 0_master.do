clear all
set more off
set trace off
set maxvar 32000
cap log close


* Konsi homeoffice

global do "/Users/kgoebler/Desktop/Masterthesis/do/"
global data "/Users/kgoebler/Desktop/Masterthesis/data/"
global original_wide "/Users/kgoebler/Desktop/Masterthesis/stata/"


* Konsi DIW
global do "S:\STUD\kgoebler\Lebenseinkommen\do\"
global data "S:\STUD\kgoebler\Lebenseinkommen\data\"
global grafik "S:\STUD\kgoebler\Lebenseinkommen\grafik\"
global original_long "S:\data\soep32_de_l\stata\"
global original_wide "S:\data\soep32_de\stata\"
global original_wide_old "S:\data\soep30_de\stata\"
global log "S:\STUD\kgoebler\Lebenseinkommen\log\"

qui ssc install kmatch, replace
qui ssc install kdens, replace
qui ssc install psmatch2, replace

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
* Programminformationen
*--------------------------------------------------------------------------------------------------------------------------------------------------------------
* geschrieben mit STATA 13
* benötitigte ado files Renten- und Pensionenberechnung
qui ssc install rowsort
qui ssc install xml_tab
qui ssc install outreg2

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
* data generation, imputation and GRV computation
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

do "${do}1_prepare_soep.do"


do "${do}1_pmm.do"

* check goodness of imputation
*do "${do}1a_pmm_pseudo.do"
