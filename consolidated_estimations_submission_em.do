
* Descriptive regressions for the energy paper as submitted to Energy Economics
*******************************************************************************
*******************************************************************************

clear all
cd "" /* insert correct path */
use consolidated_data_submission_em.dta      /* load the data file  */

* global options 
	* keep if inv=="Production"       /* do regressions for production        */
	* keep if inv=="Fin. Consumption" /* do regressions for final production  */
	keep if inv=="Fin. Production"    /* do regressions for final consumption */

	global controls // s_int inc_ppp /* optionally include additional controls */

* Generate the dummies: 
	gen p_gdp      = p_dum * inc_ppp
	gen eu_p_dum   = p_dum * eu_dum
	gen eu15_p_dum = p_dum * eu15_dum
	gen eeu_p_dum  = p_dum * eeu_dum

	egen group_id  = group(group)

	label define p_label 0 "1997-2007" 1 "2007-2014"
	label values p_dum p_label

	replace group="ROW"   if group == "noecd"
	replace group="ROW"   if group == "row"
	replace group="BRICS" if group == "brics"
	replace group="EEU"   if group == "eeu"
	replace group="EU-15" if group == "eu_15"
	replace group="OECD"  if group == "oecd"

	label values p_dum 
	gen oecd_dum=0
	replace oecd_dum=1  if group == "OECD"
	gen brics_dum=0
	replace brics_dum=1 if group == "BRICS"
	gen row_dum=0
	replace row_dum=1   if group == "ROW"

	gen oecd_p_dum = p_dum * oecd_dum


	*******************************************
	*** Regression energy efficiency factor ***
	*******************************************
	
*********************
* REG EU (column 1) *
*********************

	reg mv_int $controls p_dum eu_dum eu_p_dum, robust 
	estimates store est1
	
	************************
	* 1st period
	* non-EU in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU in 1st period:
	di _b[_cons]+_b[eu_dum]
	estadd scalar eu1 = _b[_cons]+_b[eu_dum]
	test _cons+eu_dum=0
	estadd scalar eu1_p = r(p)	
	* EU vs non-EU in 1st period:
	di _b[eu_dum]
	estadd scalar eubase1=_b[eu_dum]
	test eu_dum
	estadd scalar eubase1_p = r(p)	
	************************
	* 2nd period	
	* non-EU in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu_dum]+_b[eu_p_dum] 
	estadd scalar eu2=_b[_cons]+_b[p_dum]+_b[eu_dum]+_b[eu_p_dum] 
	test _cons+p_dum+eu_dum+eu_p_dum=0
	estadd scalar eu2_p = r(p)
	* EU vs non-EU in 2nd period:
	di _b[eu_dum]+_b[eu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eubase2=_b[eu_dum]+_b[eu_p_dum]
	test eu_dum+eu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eubase2_p=r(p)	
	************************
	* 1st vs 2nd period
	* non-EU
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU
	di _b[p_dum]+_b[eu_p_dum]
	estadd scalar eu12 = _b[p_dum]+_b[eu_p_dum]
	test p_dum+eu_p_dum=0
	estadd scalar eu12_p = r(p)

	* Optionally print the individual column: 
	* esttab est1, stats(N base1 eu1 eubase1 base2 eu2 eubase2 base12 eu12 base1_p eu1_p eubase1_p base2_p eu2_p eubase2_p base12_p eu12_p)
	*exit
	
	
*******************************
* REG EU15 and EEU (column 2) *
*******************************
	reg mv_int $controls p_dum eu15_dum eeu_dum eu15_p_dum eeu_p_dum, robust 
	estimates store est2
	************************
	* 1st period
	* non-EU in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eeu_dum=0
	estadd scalar eeu1_p = r(p)		
	* EU15 vs non-EU in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs non-EU in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	************************
	* 2nd period	
	* non-EU in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* EU15 vs non-EU in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs non-EU in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)	
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	
	************************
	* 1st vs 2nd period
	* non-EU
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	
	************************
	* 1st-2nd period, EU15 vs EEU (DID)
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	

    * Optionally, print an individual column: 
	* esttab est2, stats(N base1 eu1 eu151 eeu1 eubase1 eu15base1 eeubase1 base2 eu2 eu152 eeu2 eubase2 eu15base2 eeubase2 base12 eu12 eu1512 eeu12 base1_p eu1_p eu151_p eeu1_p eubase1_p eu15base1_p eeubase1_p base2_p eu2_p eu152_p eeu2_p eubase2_p eu15base2_p  eeubase2_p base12_p eu12_p eu1512_p eeu12_p)

	
*******************************************
* REG EU15 and EEU no outliers (column 3) *
*******************************************
	reg mv_int $controls p_dum eu15_dum eeu_dum eu15_p_dum eeu_p_dum if reg!="che", robust 
	estimates store est2_nooutliers
	************************
	* 1st period
	* non-EU in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eeu_dum=0
	estadd scalar eeu1_p = r(p)		
	* EU15 vs non-EU in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs non-EU in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	************************
	* 2nd period	
	* non-EU in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* EU15 vs non-EU in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs non-EU in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)	
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	
	************************
	* 1st vs 2nd period
	* non-EU
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	
		************************
	* 1st-2nd period, EU15 vs EEU (DID)
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	

	
		
****************************************
* REG EU15 and EEU and OECD (column 4) *
****************************************

	reg mv_int $controls p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum, robust
	estimates store est3
	*test eu15_p_dum=oecd_p_dum // cannot reject that EU15 and OECD are equal
	************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eeu_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)
	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
	
		
****************************************************
* REG EU15 and EEU and OECD no outliers (column 5) *
****************************************************	
	
	reg mv_int $controls p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum if reg!="che", robust 
	*test oecd_p_dum=eu15_p_dum
	estimates store est3_nooutliers
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eeu_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
	
	
******************************
* Plug all columns together: *
******************************
	
	esttab est1 est2 est2_nooutliers est3 est3_nooutliers, ///
	star(* 0.1 ** 0.05 *** 0.01) b(3) t(3) r2 ///
	stats(N r2 ///
	base1 eu1 eu151 eeu1 oecd1 ///
	eubase1 eu15base1 eeubase1 oecdbase1 eeueu151 oecdeu151 oecdeeu1 ///
	base2 eu2 eu152 eeu2 oecd2 ///
	eubase2 eu15base2 eeubase2 oecdbase2 eeueu152 oecdeu152 oecdeeu2 ///
	base12 eu12 eu1512 eeu12 oecd12 ///
	eeueu15did oecdeu15did oecdeeudid ///
	///
	base1_p eu1_p eu151_p eeu1_p oecd1_p ///
	eubase1_p eu15base1_p eeubase1_p oecdbase1_p eeueu151_p oecdeu151_p oecdeeu1_p ///
	base2_p eu2_p eu152_p eeu2_p oecd2_p ///
	eubase2_p eu15base2_p eeubase2_p oecdbase2_p eeueu152_p oecdeu152_p oecdeeu2_p ///
	base12_p eu12_p eu1512_p eeu12_p oecd12_p ///
	eeueu15did_p oecdeu15did_p oecdeeudid_p)


****************************************
* Create the fingures in the appendix: *
****************************************
    
	graph twoway (lfitci mv_int p_dum) (scatter mv_int p_dum, mcolor(dknavy)), legend(off) ytitle(Average annual %-change in sectoral energy intensity) by(, legend(off)) xlabel(minmax) xsize(15) ysize(10)  by(group, rows(1))  scheme(s2manual) 
	exit
	
	* graph without Switzerland (i.e. outlier in the OECD group)
	graph twoway (lfitci mv_int p_dum) (scatter mv_int p_dum, mcolor(dknavy)) if reg!="che", legend(off) ytitle(Average annual %-change in sectoral energy intensity) by(, legend(off)) xlabel(minmax) xsize(15) ysize(10)  by(group, rows(1))  scheme(s2manual) 


****************************************
* Regressions for the energy mix:      *
****************************************

	global depvar dav_shr_abs_ren 
	
	foreach var in dav_shr_abs_fos dav_shr_abs_nuc dav_shr_abs_rsn dav_shr_abs_hyd dav_shr_abs_wnd dav_shr_abs_sol dav_shr_abs_orn dav_shr_abs_nor dav_shr_abs_ren {
		replace `var'=`var'*100
		label var `var' "Average annual change in the share (in %) of egy commodity in energy mix"
	}


*******************************
* renewables
*******************************

drop if reg=="cyp"

	reg dav_shr_abs_ren p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum, robust
	estimates store est_ren_oecd

	************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
		
	
*******************************
* non-renewables
*******************************	
	
	reg dav_shr_abs_nor  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_nor_oecd
	/*
	test eu15_p_dum=oecd_p_dum // cannot reject that EU15 and OECD are equal
	test eeu_p_dum=oecd_p_dum
	*/
	
	************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eeu_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
	
	

*******************************
* fossil
*******************************

	reg dav_shr_abs_fos  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_fos_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)
	
	
	
*******************************
* nuclear
*******************************

	reg dav_shr_abs_nuc  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_nuc_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)
	
	
		
	
	
*******************************
* other non-renewable
*******************************

	reg dav_shr_abs_rsn  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_rsn_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)
	
		
	
*******************************
* hydro
*******************************

	reg dav_shr_abs_hyd p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_hyd_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
	
	

	
*******************************
* wind
*******************************

	reg dav_shr_abs_wnd  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_wnd_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)
	
	
	
	
*******************************
* solar
*******************************

	reg dav_shr_abs_sol  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_sol_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)
		
	
*******************************
* other renewable
*******************************

	reg dav_shr_abs_orn  p_dum eu15_dum eeu_dum oecd_dum eu15_p_dum eeu_p_dum oecd_p_dum , robust
	estimates store est_orn_oecd
		************************
	* 1st period
	* base in 1st period:
	di _b[_cons]
	estadd scalar base1 = _b[_cons]
	test _cons
	estadd scalar base1_p = r(p)
	* EU15 in 1st period:
	di _b[_cons]+_b[eu15_dum]
	estadd scalar eu151 = _b[_cons]+_b[eu15_dum]
	test _cons+eu15_dum=0
	estadd scalar eu151_p = r(p)	
	* EEU in 1st period:
	di _b[_cons]+_b[eeu_dum]
	estadd scalar eeu1 = _b[_cons]+_b[eeu_dum]
	test _cons+eu15_dum=0
	estadd scalar eeu1_p = r(p)	
	* OECD in 1st period:
	di _b[_cons]+_b[oecd_dum]
	estadd scalar oecd1 = _b[_cons]+_b[oecd_dum]
	test _cons+oecd_dum=0
	estadd scalar oecd1_p = r(p)		
	* EU15 vs base in 1st period:
	di _b[eu15_dum]
	estadd scalar eu15base1=_b[eu15_dum]
	test eu15_dum
	estadd scalar eu15base1_p = r(p)	
	* EEU vs base in 1st period:
	di _b[eeu_dum]
	estadd scalar eeubase1=_b[eeu_dum]
	test eeu_dum
	estadd scalar eeubase1_p = r(p)	
	* EEU vs EU15 in 1st period:
	di _b[eeu_dum]-_b[eu15_dum]
	estadd scalar eeueu151=_b[eeu_dum]-_b[eu15_dum]
	test eeu_dum=eu15_dum
	estadd scalar eeueu151_p = r(p)	

	* OECD vs base in 1st period:
	di _b[oecd_dum]
	estadd scalar oecdbase1=_b[oecd_dum]
	test oecd_dum
	estadd scalar oecdbase1_p = r(p)
	* OECD vs EU-15 in 1st period:
	di _b[oecd_dum]-_b[eu15_dum]
	estadd scalar oecdeu151=_b[oecd_dum]-_b[eu15_dum]
	test oecd_dum=eu15_dum
	estadd scalar oecdeu151_p = r(p)
	* OECD vs EEU in 1st period:
	di _b[oecd_dum]-_b[eeu_dum]
	estadd scalar oecdeeu1=_b[oecd_dum]-_b[eeu_dum]
	test oecd_dum=eeu_dum
	estadd scalar oecdeeu1_p = r(p)	
		
	
	************************
	* 2nd period	
	* base in 2nd period:
	di _b[_cons]+_b[p_dum]
	estadd scalar base2=_b[_cons]+_b[p_dum]
	test _cons+p_dum=0
	estadd scalar base2_p = r(p)
	* EU15 in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	estadd scalar eu152=_b[_cons]+_b[p_dum]+_b[eu15_dum]+_b[eu15_p_dum] 
	test _cons+p_dum+eu15_dum+eu15_p_dum=0
	estadd scalar eu152_p = r(p)
	* EEU in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	estadd scalar eeu2=_b[_cons]+_b[p_dum]+_b[eeu_dum]+_b[eeu_p_dum] 
	test _cons+p_dum+eeu_dum+eeu_p_dum=0
	estadd scalar eeu2_p = r(p)
	* OECD in 2nd period:
	di _b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	estadd scalar oecd2=_b[_cons]+_b[p_dum]+_b[oecd_dum]+_b[oecd_p_dum] 
	test _cons+p_dum+oecd_dum+oecd_p_dum=0
	estadd scalar oecd2_p = r(p)
	* EU15 vs base in 2nd period:
	di _b[eu15_dum]+_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2=_b[eu15_dum]+_b[eu15_p_dum]
	test eu15_dum+eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eu15base2_p=r(p)	
	* EEU vs base in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2=_b[eeu_dum]+_b[eeu_p_dum]
	test eeu_dum+eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar eeubase2_p=r(p)
	* EEU vs EU15 in 2nd period:
	di _b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152=_b[eeu_dum]+_b[eeu_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test eeu_dum+eeu_p_dum=eu15_dum+eu15_p_dum // diff EU vs. non-EU 2nd period
	estadd scalar eeueu152_p=r(p)	

	* OECD vs base in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum] // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2=_b[oecd_dum]+_b[oecd_p_dum]
	test oecd_dum+oecd_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdbase2_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum] 
	estadd scalar oecdeu152=_b[oecd_dum]+_b[oecd_p_dum]-_b[eu15_dum]-_b[eu15_p_dum]
	test oecd_dum+oecd_p_dum-eu15_dum-eu15_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeu152_p=r(p)	
	* OECD vs EU15 in 2nd period:
	di _b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum] 
	estadd scalar oecdeeu2=_b[oecd_dum]+_b[oecd_p_dum]-_b[eeu_dum]-_b[eeu_p_dum]
	test oecd_dum+oecd_p_dum-eeu_dum-eeu_p_dum=0 // diff EU vs. non-EU 2nd period
	estadd scalar oecdeeu2_p=r(p)			
	
	************************
	* 1st vs 2nd period
	* base
	di _b[p_dum]
	estadd scalar base12 = _b[p_dum]
	test p_dum=0
	estadd scalar base12_p = r(p)
	* EU15
	di _b[p_dum]+_b[eu15_p_dum]
	estadd scalar eu1512 = _b[p_dum]+_b[eu15_p_dum]
	test p_dum+eu15_p_dum=0
	estadd scalar eu1512_p = r(p)
	* EEU
	di _b[p_dum]+_b[eeu_p_dum]
	estadd scalar eeu12 = _b[p_dum]+_b[eeu_p_dum]
	test p_dum+eeu_p_dum=0
	estadd scalar eeu12_p = r(p)
	* OECD
	di _b[p_dum]+_b[oecd_p_dum]
	estadd scalar oecd12 = _b[p_dum]+_b[oecd_p_dum]
	test p_dum+oecd_p_dum=0
	estadd scalar oecd12_p = r(p)

	
	************************
	* 1st-2nd period, EU vs OECD (DID)
	* EU15 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eu15_p_dum]
	estadd scalar oecdeu15did = _b[oecd_p_dum]-_b[eu15_p_dum]
	test oecd_p_dum=eu15_p_dum
	estadd scalar oecdeu15did_p = r(p)
	* EEU 2-1 vs. OECD 2-1
	di _b[oecd_p_dum]-_b[eeu_p_dum]
	estadd scalar oecdeeudid = _b[oecd_p_dum]-_b[eeu_p_dum]
	test oecd_p_dum=eeu_p_dum
	estadd scalar oecdeeudid_p = r(p)	
	* EEU 2-1 vs. EU15 2-1
	di _b[eeu_p_dum]-_b[eu15_p_dum]
	estadd scalar eeueu15did = _b[eeu_p_dum]-_b[eu15_p_dum]
	test eu15_p_dum=eeu_p_dum
	estadd scalar eeueu15did_p = r(p)	
		
	
		esttab  est_fos_oecd  est_nuc_oecd est_rsn_oecd  est_hyd_oecd est_wnd_oecd  est_sol_oecd est_orn_oecd, star(* 0.1 ** 0.05 *** 0.01) b(3) t(3) r2 stats(N r2 ///
	base1 eu151 eeu1 oecd1 ///
	eu15base1 eeubase1 oecdbase1 eeueu151 oecdeu151 oecdeeu1 ///
	base2 eu152 eeu2 oecd2 ///
	eu15base2 eeubase2 oecdbase2 eeueu152 oecdeu152 oecdeeu2 ///
	base12 eu1512 eeu12 oecd12 ///
	eeueu15did oecdeu15did oecdeeudid ///
	///
	base1_p eu151_p eeu1_p oecd1_p ///
	eu15base1_p eeubase1_p oecdbase1_p eeueu151_p oecdeu151_p oecdeeu1_p ///
	base2_p eu152_p eeu2_p oecd2_p ///
	eu15base2_p eeubase2_p oecdbase2_p eeueu152_p oecdeu152_p oecdeeu2_p ///
	base12_p eu1512_p eeu12_p oecd12_p ///
	eeueu15did_p oecdeu15did_p oecdeeudid_p), // using commodity_table_fc.tex

exit












