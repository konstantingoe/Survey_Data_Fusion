*Sampleauswahl VSKT jeweils passive Bevölkerung

clear all
set more off
cap log close 
*log using "${log}passive.log", replace

global maxyear = 2017
global maxyear2 = $maxyear + 1

********************************************************************************
********************************************************************************
*************************** VSKT ***********************************************
********************************************************************************
********************************************************************************

foreach gender in m f{
	use "${data}match_test_`gender'.dta", clear 
	 
	foreach var in spez_ost spez_aussiedler spez_knappe spez_heirat spez_handw spez_selbst spez_ddr {
		keep if `var' == 0
		drop `var'
	}

	keep hrf weight jahr_rente ja case gbja spez_scheidung exp_al_20_bis* rente_j_* ktsd3 exp_arbeit_20_bis* brutto_zens_* alg_j_* rente_total_* npv_2060_r_net

	* Erstellung der Variablen in welchem Jahr Renteneintritt mit vollständiger Kontoklärung

	* diejenigen Personen, die 1995 in Rente gegangen sind (Ausschliesslich Kohorte 1935)
	gen renteneintritt_1995 = 0
	replace renteneintritt_1995 = 1 if rente_j_1995 >0 

	*** alle anderen Renteneintritte pro Jahr ab 1996
	global r_1996 "& renteneintritt_1995==0"

	forval i=1997 (1) $maxyear {

	local j= `i' -1

	global r_`i' "${r_`j'} & renteneintritt_`j'==0"

	}
	 
	forval i=1996 (1) $maxyear {

	disp `i'

	gen renteneintritt_`i' = 0
	replace renteneintritt_`i' = 1 if rente_j_`i' >0 ${r_`i'} 

	}

	save "${data}vskt_passiv_panel_`gender'1.dta", replace
	 
	****

	* Erfolgreiche generierung von Renteneintrittsinformationen 

	* Die Frage ist jetzt: Sollen diejenigen Renteneintritte an die jeweiligen SOEP 
	* Jahre angespielt werden? Also VSKT in 1995 verrentete Personen (Kohorte 1935) 
	* an alle im SOEP 1995 verrenteten Personen minus alle die vor 1935 geboren wurden?

	* und das für alle Jahre bis 2015? in der VSKT sind das halt immer nur im Schnitt
	* 250 Fälle, das heisst im SOEP noch weniger und dann wird der Suchalgorithmus mit 
	* zu wenigen Fällen gespeist, eventuell.


	* Erstellen eines fiktiven panels

	use "${data}vskt_passiv_panel_`gender'1.dta", clear 

	* behalte nur passive population mit geklärten Konten
	* renteneintritt_jjjj ist die Variable die eindeutig die Geburtskohorten bei 
	* Renteneintritt verortet

	* rentenbeginn die Variable, die den Rentenbeginn einer Person datiert 

	global w_1996 "renteneintritt_1995==1"

	forval i=1997 / $maxyear2 {

	local j= `i' -1

	global w_`i' "${w_`j'} | renteneintritt_`j'==1"

	}

	keep if ${w_${maxyear2}}

	*** jetzt haben wir ein Datensatz geschaffen in dem alle verrenteten Personen bis 
	*** einschließlich 2017 vorhanden sind

	*** crossvalidation of rentenbeginn konsistent ist:
		gen rentenbeginn = .

	forval i=1995 (1) $maxyear {

		replace rentenbeginn = `i' if renteneintritt_`i'==1 

	}


	*** Variable erstellen, die für alle Personen den letzten bekannten Rentenwert beinhaltet
	*** Über diese wird dann gematcht

	egen rente_${maxyear}_gesamt = rowmax(rente_j_*)


	save "${data}vskt_passiv_panel_`gender'2.dta", replace

	recode gbja (1935/1940=40) (1941/1945=45) (1946/1950=50) (1951/1955=55) (1956/1960=60), gen(gbja_cat) 

	egen exp_arbeit = rowmax(exp_arbeit*)
	egen unempben = rowmax(alg_j_*)

	save "${data}vskt_passiv_panel_`gender'.dta", replace
}
********************************************************************************
********************************************************************************
*************************** SOEP ***********************************************
********************************************************************************
********************************************************************************
* ich will ab 1995 ein Dataframe, dass mir alle Renteneintritte ab Kohorte 1935
* subsumiert
global w_84  a
global w_85  b
global w_86  c
global w_87  d
global w_88  e
global w_89  f
global w_90  g
global w_91  h
global w_92  i
global w_93  j
global w_94  k
global w_95  l
global w_96  m
global w_97  n
global w_98  o
global w_99  p
global w_00  q
global w_01  r
global w_02  s
global w_03  t
global w_04  u
global w_05  v
global w_06  w
global w_07  x
global w_08  y
global w_09  z
global w_10  ba
global w_11  bb
global w_12  bc
global w_13  bd
global w_14  be
global w_15  bf
global w_16  bg
global w_17  bh

global j_96 95 
global j_97 96
global j_98 97
global j_99 98
global j_00 99
global j_01 00
global j_02 01
global j_03 02
global j_04 03
global j_05 04
global j_06 05
global j_07 06
global j_08 07
global j_09 08
global j_10 09
global j_11 10
global j_12 11
global j_13 12
global j_14 13
global j_15 14
global j_16 15
global j_17 16


use persnr gebjahr sex loc1989 immiyear using  "${original_wide}ppfad.dta", clear
		sort persnr
		keep if immiyear == -2
		keep if loc1989 == 2
		drop immiyear loc1989

		merge 1:1 persnr using "${original_wide}lpequiv", keepus(persnr igrv195) nogen keep(3)

		keep if gebjahr>1934 & gebjahr<=1935  & igrv195 >0

		gen renteneintritt_95 = 1

save "${data}soep_passive_95", replace


 local year = 1936
 
foreach var in 96 97 98 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17{

		use persnr gebjahr sex loc1989 immiyear using  "${original_wide}ppfad.dta", clear
		sort persnr
		keep if immiyear == -2
		keep if loc1989 == 2
		drop immiyear loc1989
	
		merge 1:1 persnr using "${original_wide}\${w_`var'}pequiv", keepus(persnr igrv1`var') nogen keep(3)

		keep if gebjahr>1934 & gebjahr<=`year'  & igrv1`var' >0 
		merge 1:1 persnr using "${data}soep_passive_${j_`var'}"

		*** wenn _merge==1 dann Renteneintritt
		*** wenn _merge==2 dann Dropout -> moeglicherweise alle dropouts vor dem vollendeten 65 Lebensjahr ausschliessen -> Timm fragen
		*** Wenn _merge==3 dann schon vorhanden -> ergo bereits in Rente 
		*** _merge==2 + _merge==3 ergibt bereits verrentete Population zum Beobachtungszeitpunkt


		gen renteneintritt_`var' = 0
		replace renteneintritt_`var' = 1 if _merge == 1 
		drop _merge
		
		
		
		save "${data}soep_passive_`var'", replace
		
				local year = `year' + 1

	}


	
	
foreach var in 95 96 97 98 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17{
	
	replace renteneintritt_`var' = 0 if renteneintritt_`var' == .
 
 }


egen rente_${maxyear}_gesamt = rowmax(igrv1*) 
	
global j_1984 84
global j_1985 85
global j_1986 86
global j_1987 87
global j_1988 88
global j_1989 89
global j_1990 90
global j_1991 91
global j_1992 92
global j_1993 93
global j_1994 94
global j_1995 95
global j_1996 96 
global j_1997 97
global j_1998 98
global j_1999 99
global j_2000 00
global j_2001 01
global j_2002 02
global j_2003 03
global j_2004 04
global j_2005 05
global j_2006 06
global j_2007 07
global j_2008 08
global j_2009 09
global j_2010 10
global j_2011 11
global j_2012 12
global j_2013 13
global j_2014 14
global j_2015 15
global j_2016 16
global j_2017 17


gen renteneintritt_ges = .
forval i=1995 (1) $maxyear {

	replace renteneintritt_ges = `i' if renteneintritt_${j_`i'} == 1

}

ren gebjahr gbja
recode sex (1=0) (2=1) //male = 0, female = 1
lab drop sex


*** how to weight? 

* get the weights from every year where renteneintritt is true!

merge 1:1 persnr using "${original_wide}phrf", keepus( persnr *phrf) keep(3) nogen
gen pwgt = 0

foreach var in 95 96 97 98 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17{
	
	replace pwgt = ${w_`var'}phrf if renteneintritt_`var'== 1 
}


drop *phrf 
*** weighting the soep does not really make sense 
* also note that the renteneintritt is not really renteneintritt it could also be survey entry!

recode gbja (1935/1940=40) (1941/1945=45) (1946/1950=50) (1951/1955=55) (1956/1960=60), gen(gbja_cat) 

save "${data}soep_passive_full_1", replace 

*** Working Experience / Education

use "${data}soep_passive_full_1", clear
*84 85 86 87 88 89 90 91 92 93 94 
foreach var in 95 96 97 98 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17{

merge 1:1 persnr using "${original_wide}\${w_`var'}pgen", keepus(persnr isced11_`var' exppt`var' expft`var' expue`var') keep(1 3) nogen

}

foreach i in 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17{
	merge 1:1 persnr using "${original_wide}/${w_`i'}pequiv", keepus(persnr i11110`wave' iself`wave' iunby`wave') keep(1 3) nogenerate
	gen earnings_`i' = max(0,i11110`i' - iself`i')

}


* so far so good... now gen a rowmax
order *, sequential

egen experienceft = rowmax(expft*)


* We loose 15 Observations

egen experiencept = rowmax(exppt*)

* Education 

egen education = rowmax(isced11*)

* Income
cap drop income
egen income = rowmax(earnings*)


* Unemployment benefit

egen unempben = rowmax(iunby*) 

* Experience in unemployment
cap drop expunempl
egen expunempl = rowmax(expue*) 
replace expunempl = expunempl*12

save "${data}soep_passive_full_div", replace 

use "${data}soep_passive_full_div", clear

ren persnr pid

preserve 
do "${divorcepath}2_divorce_info.do"
restore 

merge 1:1 pid using "${data}divorce.dta", keep(1 3) nogen

replace divorce5 = 0 if missing(divorce5)
replace divorce10 = 0 if missing(divorce10)


foreach x in experienceft experiencept education expunempl {

	 replace `x'=. if inlist(`x',-1,-3)
	 replace `x'=0 if `x'==-2
	 *tab1 `x'
	
}

drop isced11* expft* exppt* earnings* i11110* iself* iunby* expue*

gen exppt = 0.5 * experiencept 
egen expwork = rowtotal(experienceft exppt), missing
replace expwork = expwork*12


save "${data}soep_passive_full_2", replace 
****
use "${data}soep_passive_full_2", clear 

*Aufräumen
ren renteneintritt_ges rentenbeginn 


drop igrv* renteneintritt_* experienceft experiencept exppt

order pid sex gbja gbja_cat pwgt 

drop if expwork==.
drop if education ==.

save "${data}soep_passive_ges", replace 

global gender_0 m
global gender_1 f

forval i = 0/1{
	preserve
	keep if sex == `i'
	drop sex
	save "${data}soep_passive_${gender_`i'}", replace 
	restore
}

