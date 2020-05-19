clear all
set more off

*global soep "D:\Original_data\SOEP\v34\"
*global data "D:\Data\matching_soep_vskt\data\"

use persnr spellnr spelltyp beginy endy using "${original_wide}biomarsy.dta", clear
sort persnr spellnr

* Only married/divorce 
keep if inrange(spelltyp,2,3) 

* Only  end of marriage observed after 1977
drop if endy < 1977 & spellty == 2
drop if beginy < 1977 & spellty == 3

* Marriage duratiom (5 and 10 years minimum)
gen duration = endy - beginy if spelltyp == 2 
gen divorce5 = 1 if duration[_n-1] >=5 & spelltyp == 3 & persnr == persnr[_n-1]
gen divorce10 = 1 if duration[_n-1] >=10 & spelltyp == 3 & persnr == persnr[_n-1]
recode divorce10 ( . = 0)  

keep if divorce5 == 1

bysort persnr: egen divorce = max(divorce10)
drop divorce10
ren divorce divorce10
duplicates drop persnr divorce5 divorce10, force
keep persnr divorce*
ren persnr pid
save "${data}divorce.dta", replace

