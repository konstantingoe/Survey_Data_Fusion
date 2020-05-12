 *Sampleauswahl VSKT nur Männer
clear all
set more off
cap log close 
*log using "${log}prepare.log", replace

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
///////////////////   VSKT Active Sample ///////////////////////////////////////
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*


foreach gender in m f{
	use "${data}match_test_`gender'.dta", clear 

	foreach var in spez_ost spez_aussiedler spez_knappe spez_heirat spez_handw spez_selbst spez_ddr {
		keep if `var' == 0
		drop `var'
	}

	keep ztptrtbejj ja case gbja spez_scheidung exp_al_20_bis* ktsd3 exp_arbeit_20_bis* rentenanspruch_2012 brutto_zens_* alg_j_* psgr rente_total_* em_rente_2012 npv_20*_r_net
	keep ztptrtbejj psgr ja case gbja spez_scheidung exp_al_20_bis* exp_arbeit_20_bis* rentenanspruch_2012 brutto_zens_* alg_j_* rente_total_* em_rente_2012 npv_20*_r_net

	gen age=2012-gbja
	recode age (0/35=35) (36/45=45) (46/55=55) (56/102=56), gen(age_g) 
	 

	gen verrentet = 0
	replace verrentet = 1 if ztptrtbejj >0 & ztptrtbejj<2013

	tab verrentet if em_rente_2012>0
	sum em_rente_2012 if verrentet ==1 & em_rente_2012>0
	sum rente_total_2012 if verrentet==0 & rente_total_2012 >0


	gen em_rente=0
	replace em_rente=1 if em_rente_2012>0 

	 
	global match gbja spez_scheidung exp_al_20_bis2012 exp_arbeit_20_bis2012 rentenanspruch_2012 brutto_zens_2012 alg_j_2012 rente_total_2012 age_g 

	sum $match

	replace rentenanspruch_2012 = 0 if rentenanspruch_2012 <0 

	egen ltearnings = rowmax(npv_20*_r_net)

	keep $match verrentet em_rente ja case ltearnings  

	save "${data}vskt_`gender'.dta", replace

	// 1. Aktive Bevölkerung 

	use "${data}vskt_`gender'.dta", clear

	keep if verrentet==0 & em_rente==0

	drop verrentet

	save "${data}vskt_`gender'_active.dta", replace
}


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
///////////////////   SOEP Active Sample ///////////////////////////////////////
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

***Datensatz
set more off
clear

do "${divorcepath}2_divorce_info.do"
/*
*** Info fuer jemals geschieden sammeln ***;
use ${original_wide}biomarsy
gen x=0
replace x=1 if spelltyp==3 | spelltyp==5
egen divorced=max(x), by(pid)
bysort pid: gen n=_n
keep if n==1
sort pid
save "${data}divorced.dta", replace
clear
*/
************************************************************************************************************
***Reference 2013
use pid gebjahr sex loc1989 immiyear bdsampreg bcnetto bdnetto using  "${original_wide}ppfad.dta", clear
keep if (bdnetto >= 10 & bdnetto <19)
sort pid

merge 1:1 pid using ${original_wide}bdp, keepus(pid bdp10101 bdp10103 bdp10105 bdp10107 bdp102 bdp6501 bdp10301 bdp7701 bdp10302 bdp10303 bdp107 bdp10801 bdp10802 bdp10901 bdp10902 bdp10903) keep(3) nogenerate
merge 1:1 pid using ${original_wide}phrf, keepus(pid bcphrfaj bdphrfak) keep(3) nogenerate
ren pid persnr
merge 1:1 persnr using ${original_wide}biobirth, keepus(persnr sumkids) keep(1 3) nogenerate
ren persnr pid
merge 1:1 pid using "${data}divorce.dta", keep(1 3) nogenerate
replace divorce5 = 0 if missing(divorce5)
replace divorce10 = 0 if missing(divorce10)


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

gen obs = 0

*00 01 02 03 04 05 06 07 08 09 10

foreach wave in  11 12 13 {
	merge 1:1 pid using ${original_wide}/${w_`wave'}pequiv, keepus(pid i11110`wave' iself`wave' ioldy`wave' igrv1`wave' iciv1`wave' ivbl1`wave' icom1`wave' iwar1`wave' iguv1`wave' ison1`wave' iprv1`wave' igrv2`wave' iciv2`wave' ivbl2`wave' icom2`wave' iwar2`wave' iguv2`wave' ison2`wave' iprv2`wave' iwidy`wave' iunby`wave' d11107`wave') keep(1 3) nogenerate
	merge 1:1 pid using ${original_wide}/${w_`wave'}pgen, keepus(pid isced11_`wave' labgro`wave' emplst`wave' ${w_`wave'}vebzeit expft`wave' exppt`wave' expue`wave' stib`wave' betr`wave' oeffd`wave' nace`wave' ${w_`wave'}erwzeit ${w_`wave'}famstd  jobch`wave') keep(1 3) nogenerat 
	replace obs = obs + 1 if i11110`wave' !=.
	gen self_`wave' = 0
	gen civil_`wave' = 0
	gen liberal_`wave' = 0
	gen vz_`wave' = 0
	gen tz_`wave' = 0
	gen nace1_`wave' = 0
	gen nace2_`wave' = 0
	gen nace3_`wave' = 0
	gen nace4_`wave' = 0
	gen manager_`wave' = 0
	gen earnings_`wave' = max(0,i11110`wave' - iself`wave')
	replace iunby`wave' = 0 if iunby`wave' <0
	replace iself`wave' = 0 if iself`wave' <0	
	replace self_`wave' = 1 if stib`wave' >= 410 & stib`wave' <= 440 
	replace civil_`wave' = 1 if stib`wave' >= 610 & stib`wave' <= 640	
	replace liberal_`wave' = 1 if inlist(stib`wave',421, 422, 423)
    replace vz_`wave' = 1 if emplst`wave'==1
    replace tz_`wave' = 1 if emplst`wave'==2	
	replace nace1_`wave' = 1 if inlist(nace`wave',1,2)
	replace nace2_`wave' = 1 if inlist(nace`wave',5,10,11,14)
	replace nace3_`wave' = 1 if inlist(nace`wave',15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,40,41,45,96,97,100)
	replace nace4_`wave' = 1 if inlist(nace`wave',50,51,52,55,60,61,62,63,64,65,66,67,70,71,72,73,74,75,80,85,90,91,92,93,95,98,99)	
	replace manager_`wave' = 1 if inlist(stib`wave',550)
	replace betr`wave' = 1 if betr`wave'==11 
}

	
**** 2 Imputationen: 1. Schritt Imputation von BDP102 (-1), 2. Schritt: Imputation von BDP10302 

*Plausibilitaet

gen bdp102x=bdp102
replace bdp102x = 1 if bdp102 == -1 &  self_13 == 0 & civil_13 == 0 & ioldy13 == 0 & (expft13+ exppt13+ expue13 >=5)
replace bdp102x = 1 if bdp102 == -1 &  sex == 2 & sumkids >0 & sumkids!=. 
replace bdp102x = 2 if bdp102 == -1 &  sex == 2 & (sumkids == 0 | sumkids ==.) 
replace bdp102x = 3 if bdp102 == -1 &  ioldy13 > 0 
replace bdp102x = 2 if bdp102 == -1 &  (self_12 == 1 & self_13 == 1)
replace bdp102x = 2 if bdp102 == -1 &  (civil_12 == 1 & civil_13 == 1)
replace bdp102x = 2 if bdp102 == -1 &  sex == 1 & (expft13+ exppt13+ expue13 <5)

replace bdp102x = 3 if bdp102x == 2 & ioldy13 > 0 & ioldy13 < .   
replace bdp102x = 1 if bdp102x == 2 & (stib12>= 500 & stib12<=550) & (expft12+ exppt12+ expue12 >=5)
replace bdp102x = 1 if bdp102x == 2 & (stib13>= 500 & stib13<=550) & (expft13+ exppt13+ expue13 >=5)   
replace bdp102x = 1 if bdp102x == 2 & (stib12>=  15 & stib12<=250) & (expft12+ exppt12+ expue12 >=5)
replace bdp102x = 1 if bdp102x == 2 & (stib13>=  15 & stib13<=250) & (expft13+ exppt13+ expue13 >=5)
replace bdp102x = 1 if bdp102x == 2 & (inlist(stib12,-1,10,12))    & (expft12+ exppt12+ expue12 >=5)
replace bdp102x = 1 if bdp102x == 2 & (inlist(stib13,-1,10,12))    & (expft13+ exppt13+ expue13 >=5)
replace bdp102x = 1 if bdp102x == 2 & civil_12==0 & self_12==0 & sumkids>0 & sumkids<. & sex==2

replace bdp102x = 2 if bdp102x == 3 & (self_12==1 & self_13==1)
replace bdp102x = 2 if bdp102x == 3 & (civil_12==1 & civil_13==1)   
replace bdp102x = 1 if bdp102x == 3 & ioldy13==0 & stib13==13 & self_12 == 0 & civil_12 == 0 & (expft13+ exppt13+ expue13 >=5)
replace bdp102x = 1 if bdp102x == 3 & ioldy13==0 & stib13==13 & self_12 == 0 & civil_12 == 0 & sumkids >0 & sumkids <. & sex == 2
replace bdp102x = 2 if bdp102x == 3 & ioldy13==0 & stib13==13 & (self_12 == 1 | civil_12 == 1)
replace bdp102x = 1 if bdp102x == 3 & ioldy13==0  & self_12 == 0 & civil_12 == 0 & (expft13+ exppt13+ expue13 >=5)
replace bdp102x = 1 if bdp102x == 3 & ioldy13==0  & self_12 == 0 & civil_12 == 0 & sumkids >0 & sumkids <. & sex == 2
replace bdp102x = 2 if bdp102x == 3 & ioldy13 == 0

replace bdp102x = 3 if bdp102x == 1 & igrv113 > 0 & igrv113 < . & bdp10302 > 0 & bdp10302 <.
replace bdp102x = 2 if bdp102x == 1 & iciv113 > 0 & iciv113 < . & bdp10302 > 0 & bdp10302 <.
replace bdp102x = 3 if bdp102x == 1 & igrv113 > 0 & igrv113 < . & bdp10303==1
replace bdp102x = 2 if bdp102x == 1 & iciv113 > 0 & iciv113 < . & bdp10303==1
replace bdp102x = 2 if bdp102x == 1 & (expft13+exppt13+expue13)<5 & sumkids==0 
replace bdp102x = 3 if bdp102x == 1 & igrv113 > 0 & igrv113 < . & inlist(bdp10302,-1,-2)
replace bdp102x = 1 if bdp102x == 2 & expft13==. & bdp102==1  & gebjahr < 1988
replace bdp102x = 1 if inlist(pid,3149201,30695902,30831201)

**************************************************************************************************************************************;
*** Erwerbszeiten, und +2 bei Maennern aufgrund von Zeiten in Wehrdienst, bei Frauen inkl. Zahl der Kinder + 1 Jahr Erziehungszeit ***;
**************************************************************************************************************************************;
gen max_rente = .
replace max_rente = (((max(exppt13,0) + max(0,expft13)*2.5) + max(0,expue13))+2)*28.07 if bdsampreg==1 & sex==1 
replace max_rente = (((max(exppt13,0) + max(0,expft13)*2.5) + max(0,expue13))+2)*24.92 if bdsampreg==2 & sex==1
replace max_rente = (((max(exppt13,0) + max(0,expft13)*2.5) + max(0,expue13))+(sumkids*2))*28.07 if bdsampreg==1 & sex==2
replace max_rente = (((max(exppt13,0) + max(0,expft13)*2.5) + max(0,expue13))+(sumkids*2))*24.92 if bdsampreg==2 & sex==2
* gap aktive
gen gap=bdp10302-max_rente
gen relgap=gap/bdp10302*100
*** Korrektur nur wenn absolute Differenz < 300 Euro, relative Diff. > 30%, keine Missings auf Experience und exakter Wert angegeben ***;
replace bdp10302=-1 if (gap>300 & gap<.) & (relgap>30 & relgap<.) & (expft13~=-1 | expft13~=-3) & bdp10301==1
* gap passive
gen gapp=igrv113-(max_rente*12)
* Umbuchen auf , wenn Maxrente (2500x12) 𢥲schritten
gen help=igrv113-30000
replace icom113=icom113+help if help>0 & help!=.
replace igrv113=30000 if igrv113>30000 & igrv113!=.
drop help
**************************************************************************************************************************************;
*** Imputation betriebliche Altersvorsorge

gen bdp107x=bdp107
replace bdp107x=3 if (bdp10105>0 & bdp10105<.) | (bdp10107>0 & bdp10107<.)
replace bdp107x=3 if stib13==13 & bdp10901==1 & bdp107x==1 & bdp10902>0 & bdp10902<.
replace bdp107x=3 if icom113>0 & icom113<. 
replace bdp10107=bdp10902 if stib13==13 & bdp10901==1 & bdp107x==3 & (bdp10105==-2 & bdp10107==-2)
replace bdp107x=2 if bdp107==-1                               /*** Annahme generell keinen Betriebsrentenanspruch, auch deshalb weil sonst die Imputation Probleme bereitet ***/

gen bdp10902x=bdp10902
replace bdp107x=2 if bdp107x==1 & bdp10901==1 & bdp10902x==0  /***  Filter = ja, genauer Betrag aber Betriebsrente=0 --> Filter = nein ***/

*** Dummys bilden fuer die Art der Finanzierung der Betriebsrente ***
gen arbeitgeber=0
replace arbeitgeber=1 if  bdp10801==1 

gen arbeitnehmer=0
replace arbeitnehmer=1 if  bdp10801==2 

* asymmetrisches trimming
gen br_pa=bdp10902/(2013-gebjahr-20-expue13)
*sum br_pa if bdp10902>0 & bdp107x == 1 & bdp10901==1 & stib13 != 550 & stib13 != 540, d
_pctile br_pa if bdp10902>0 & bdp107x == 1 & bdp10901==1 & stib13 != 550 & stib13 != 540, p(98)
replace bdp10902x=. if br_pa>`r(r1)' & bdp107x == 1 & bdp10901==1 & stib13 != 550 & stib13 != 540

replace bdp10902x=. if bdp107x==1 & bdp10901~=1
replace bdp10902x=0 if inlist(bdp107x,2,3) 

**************************************************************************************************************************************;

gen west13=0
replace west13=1 if bdsampreg==1

gen migrant=0
replace migrant=1 if immiyear>-2 & immiyear<.

replace sumkids=0 if sex==1

gen bdp10302x=bdp10302
replace bdp10302x=. if bdp102x ==1 & bdp10301~=1
replace bdp10302x=0 if bdp102x ~=1 


*** numbers of years worked/ in sample since 2000
*** Zahl der Arbeitsplatzwechsel basierend auf jobch$$ ***

gen z1=0
gen z2=0
gen oe2=0
gen njob=0
foreach x in 11 12 13 {
	replace z1=z1+1 if i11110`x'<.
	replace z2=z2+1 if earnings_`x'<. & earnings_`x'>0
	replace oe2=oe2+1 if oeffd`x'==1
	replace njob=njob+1 if jobch`x'==4
	replace njob=5 if njob>5 & njob<.
}

gen everoeffd=0 
replace everoeffd=1 if oe2>0 & oe2<.


foreach x in bdp10302x expft13 exppt13 expue13 divorce5 divorce10 migrant isced11_13 gebjahr bdp10902x nace13 betr13 everoeffd {
 replace `x'=. if inlist(`x',-1,-3)
 replace `x'=0 if `x'==-2
 tab1 `x' 
}

save "${data}soep_1_m_f.dta", replace

***********************************************************************************************************************************************************************
use "${data}soep_1_m_f.dta", clear
mi set wide
mi register imp bdp10302x expft13 exppt13 expue13 isced11_13 bdp10902x betr13 bdp107x
mi register regular gebjahr vz_13 tz_13 divorce5 earnings_13 iself13 iunby13 liberal_13 sumkids bcphrfaj sex west migrant z1 z2 everoeffd bdp102x nace1_13 nace2_13 nace3_13 nace4_13 arbeitgeber arbeitnehmer njob manager_13

sum expft13 exppt13 expue13 west sex divorce5 migrant isced11_13 gebjahr i1111013 self_13 civil_13 sumkids bdphrfak pid bdp10302x bdp102x bdp10902x betr13 nace13 everoeffd earnings_13 iself13 iunby13 liberal_13 nace1_13 nace2_13 nace3_13 nace4_13 njob manager_13


* sample GRV
gen sample=0
replace sample=1 if bdp102x==1 
* sample company pens
gen sample1=0
replace sample1=1 if inlist(bdp107x,-1,1)
replace sample1=1 if bdp10902x>0 & bdp10902x<.


#delimit;
mi imp chained (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13)) expft13 
               (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13)) expue13 
			   (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13)) exppt13 
               (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13)) isced11_13 
			   (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13)) betr13
               (pmm, knn(5) omit(arbeitgeber arbeitnehmer njob manager_13 z1 z2) cond(if sample ==1)) bdp10302x 
= gebjahr divorce5 earnings_13 iself13 iunby13 liberal_13 sumkids bcphrfaj sex west migrant z1 z2 everoeffd vz_13 tz_13 nace1_13 nace2_13 nace3_13 nace4_13 arbeitgeber arbeitnehmer njob manager_13
, add(5) rseed(1234) noisily burnin(100) augment showevery(100) savetrace("${data}\impstats.dta", replace) force;

#delimit cr

save "${data}soep_2_m_f.dta", replace


*Matching W Männer, no immi

use "${data}soep_2_m_f.dta", clear

recode sex (1=0) (2=1) //male = 0, female = 1
lab drop sex

global gender_0 m
global gender_1 f

*Sampleauswahl SOEP --> Nur Männer/Frauen aus dem Westen ohne Migrationshintergrund, die gesetzlich versichert sind
forval i = 0/1{
 preserve	
	keep if sex == `i'
	drop sex
	keep if immiyear == -2
	drop immiyear
	keep if loc1989 == 2 & bdsampreg == 1
	drop loc1989 bdsampreg
	drop if civil_12==1
	drop if liberal_12 == 1
	drop if self_12 == 1
	drop if gebjahr <1935

	*** Age_groups erstellen

	gen age=2012-gebjahr

	recode age (0/35=35) (36/45=45) (46/55=55) (56/102=56), gen(age_g) 

	*** ren ISCED

	ren _1_isced11_13 educ
		gen educ_cat = .
		replace educ_cat = 1 if inlist(educ,0,1,2)
		replace educ_cat = 2 if educ == 3
		replace educ_cat = 3 if inlist(educ,4,5)
		replace educ_cat = 4 if inlist(educ,6,7,8)
		
	*** Familienvariablen, Haushaltsbrutto und Haushaltsnettoeinkommen
		
	drop bcnetto bdnetto bdp6501 bdp10103 bdp10105 bdp10107 bdp107 bdp10801 bdp10802 bdp10901  bdp10903 _mi_miss sample sample1
	*drop  spellnr spelltyp begin end beginy endy censor remark source x 
	drop obs iself* ioldy* iwidy* ivbl* icom* iprv* ison*
	drop betr* oeffd* *erwzeit *vebzeit nace* jobch* manager* vz* tz*  

	drop max_rente gap relgap gapp bdp107x bdp10902x arbeitgeber arbeitnehmer br_pa west13 bdp10302x z1 z2 oe2 njob everoeffd  
	drop *bdp10902* *betr* *bdp107* *isced*


	* Was auch immer hier getrieben wurde es war unfug:
	* Jetzt einmal neu und übersichtlich

	///////// Variablen aus der VSKT, die als Matching-Variablen fungieren können 
	/// gbja --> Geburtsjahr
	/// spez_scheidung --> Scheidung Ja/Nein
	/// exp_al_20_bis2002 -- exp_al_20_bis2015 --> Monate in Arbeitslosigkeit ohne 2003
	/// exp_arbeit_20_bis2002 -- exp_arbeit_20_bis2015 --> Monate in Arbeit ohne 2003
	/// rentenanspruch_2012 --> Rentenanwartschaften
	/// brutto_zens_1998 -- brutto_zens_2015  ( wir nehmen nur 2012) 
	/// --> Individelles Arbeitseinkommen bis zur Beitragsbemessungsgrenze der jeweiligen Jahre, bspw. 2012: 67.200 im Westen
	/// alg_j_1952 -- 2015 --> Arbeitslosengeld  
	/// rente_total_1998 -- 2015 --> jährliche Rentenbezüge 
	////////


	* Diese Variablen sind auch im SOEP zu finden insbesondere 2012 unser einziges Jahr,
	* zu dem es Rentenanwartschaftsinfos gibt
	// für das Data-Fusing sollten keine missings in den Matching Variablen enthalten sein


	// 1. Geburtsjahr im SOEP: gebjahr
	// ein einfacher rename reicht aus!
	ren gebjahr gbja

	// 2. Scheidungsindikator im SOEP: divorced
	// ein einfacher rename reicht aus!
	ren divorce5 spez_scheidung

	// 3. Monate in Arbeitslosigkeit: _1_expue13 -- Bezug auf das vorherige Jahr
	// Hier Jahreswerte in der VSKT allerdings Monatswerte:
	ren _1_expue13 exp_al_20_bis2012
	replace exp_al_20_bis2012 = exp_al_20_bis2012 *12

	// 4. Monate in Arbeit
	// auch hier wieder imputierte 2013 Werte nehmen:
	gen exppt13_p = 0.5*_1_exppt13 
	egen exp_arbeit_20_bis2012 = rowtotal(exppt13_p _1_expft13)
	replace exp_arbeit_20_bis2012 = exp_arbeit_20_bis2012*12

	// 5. Rentenanwartschaften
	// wurden oben imputiert: _1_bdp10302x 
	ren _1_bdp10302x rentenanspruch_2012

	// 6. Individelles Arbeitseinkommen bis zur Beitragsbemessungsgrenze
	// Im SOEP i1111012 --> earnings_12 oben erstellt bezieht sich nicht auf Vorjahr???


	gen brutto_zens_2012 = min(earnings_13, 67200)

	// 7. Arbeitslosengeld
	// im SOEP iunby13, da bezug auf das vorherige Jahr
	// eigentlich noch einschränkung wegen Kinder aber da hier nur Männer ist sumkids 0
	*replace alg_j_2012 = iunby12/0.67*.6 if d1110712 > 0 & d1110712<.

	gen alg_j_2012 = iunby13 

	// 8. Jährliche Rentenbezüge
	// im SOEP igrv13 für gesetzlich rentenversicherte

	ren igrv113 rente_total_2012
	replace rente_total_2012 = 0 if rente_total_2012<0

	/// alle relevanten Variablen harmonisiert und von NA bereinigt ///

	********************************************************************************
	********** Populationseinschränkungen für genaues data fusing ******************
	********************************************************************************

	* zunächst 3 Subpopulationen für Männer in 2012:

	* 1. Aktive mit genauen Rentenanwartschaftsangaben

	* 2. Aktive mit genauen und ungenauen Rentenanwartschaftsangaben 
	* hier müssen in einem Schritt nach dem data.fusing die matches verglichen werden

	* 3. Passive und sonstige Rentner

	/// Aktive Population / passive Population

	gen verrentet = 0
	replace verrentet = 1 if (rente_total_2012>0 & rente_total_2012<.) ///
						   | (iciv113>0 & iciv113<.) 				  ///
						   | (iguv113>0 & iguv113<.) 

	replace verrentet=1 if bdp102==3
	replace verrentet=1 if stib12==13	

	* Im SOEP können Erwerbsminderungsrenten nicht direkt erkannt werden, 
	* nur über die Altersabfrage

	gen em_rente = 0
	replace em_rente = 1 if verrentet==1 & age<60

	gen rente_ges = 0
	replace rente_ges = 1 if verrentet==1 | em_rente==1
											   
											   
	/// 1. Aktive Population mit genauen angaben auf der bdp10301

	gen active_genau = 0
	replace active_genau = 1 if bdp10301 ==1 & verrentet==0

	/// 2. Aktive Population mit genauen und ungenauen Angaben: verrentet==0

	/// 3. Passive Population: verrentet==1

	/// Summary über die matching variablen
	global match gbja spez_scheidung exp_al_20_bis2012 exp_arbeit_20_bis2012 rentenanspruch_2012 brutto_zens_2012 alg_j_2012 rente_total_2012 age_g 

	sum $match

	*** Cleaning:

	drop *11 *13
	drop *112 *212
	drop migrant i1111012 iunby12 labgro12 stib12 emplst12 expft12 exppt12 expue12 self_12 civil_12 liberal_12 earnings_12
	drop bdp7701 bdp10101 bdp102 bdp10302 bdp10303 bcphrfaj bdphrfak bdp102x bdp10301 _*_* expp* sumkids 

	save "${data}soep_2012_${gender_`i'}.dta", replace

	*** für 1. : Aktive Bevölkerung 

	use "${data}soep_2012_${gender_`i'}.dta", clear

	keep if verrentet == 0	
	keep $match pid educ educ_cat divorce10
	 
	save "${data}soep_${gender_`i'}_aktiv.dta", replace
 restore
}








