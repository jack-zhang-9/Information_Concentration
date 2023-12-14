*********************************************************************************
*
* Large foreign IPOs analysis                  
*
*********************************************************************************

***************************
*Prepare for Data
***************************
cd "D:\张知遥\武汉大学\刘淼RA\Info_concentration"
*Import master data, do some data filtering
import delimited info_con_master_yr.csv, clear
drop if at == . /*drop if total asset is missing*/
*drop if sic2 >= 60 & sic2 <= 69 /*drop financial institutions*/
drop if fyear >= 2021 /*our sample period ends at 2020*/
egen ind_group = group(ind)
/*
*A gvkey-year would have multiple obs if it has multiple permnos (3% of sample)
*Keep the permno with higher trading volume in each year
duplicates tag permno fyear, gen(tab)
tab tab
bysort gvkey fyear: egen high_trade = max(yr_vol)
keep if yr_vol == high_trade
drop tab high_trade
*/
/* There are three duplicates for permno 17977 and fyear 2020 */
*drop if permno == 17977 & fyear == 2020 & _n ~= 29068

*drop if fyear < 2008
*drop if global == 1
*drop if global_del2 == 1
drop if trivial_del == 1 /**************************************************************************************************/

*replace missing analysts/conference outcome variables to 0
foreach varname of varlist analyst_count ccp_count ec_count ec_no_question ccp_no_question  ec_word_count ccp_word_count ec_no_analyst  ccp_no_analyst forecast_count hedge_share{
	replace `varname' = 0 if `varname'==.
}

*transforming/creating variables
gen lmve = log(mve_f)
gen lanalyst_count = log(analyst_count + 1)
gen lat = log(at+1)
gen lat2 = lat^2
gen age2 = age^2
*gen ltrading_vol = log(trading_vol + 1)
replace xrd = 0 if xrd == .
replace xsga = 0 if xsga == .
gen intangible = (xrd + xsga)/at
gen lintangible = log(intangible + 1)
gen lat_bl = lat*bl
gen lat_inst_ownership = lat*inst_ownership
gen lat_intangible = lat*intangible
gen age_bl = age*bl
gen age_inst_ownership = age*inst_ownership
gen age_intangible = age*intangible
gen lforecast_count = log(forecast_count+1)
replace forecast_count = forecast_count/100
replace inst_ownership=0 if inst_ownership==.
replace inst_ownership=1 if inst_ownership > 1 /*Check these - data errors?*/

// gen ebit3_at = ebit3/at
// gen m_at = log(mve_f/at)
// gen ebit_at = ebit/at
// tostring sic, replace
*gen sic1 = substr(sic, 1, 1)
*gen sic2 = substr(sic, 1, 2)

winsor2 lanalyst_count analyst_count lforecast_count forecast_count ec_count ccp_count ec_no_question ec_no_analyst  ec_word_count sum_trading_turnover avg_trading_turnover global_share2 lat lat2 age age2 bl intangible inst_ownership hedge_share, replace cuts(0 99)


***************************
*Diff-in-Diff (2007-2020)
*This should be our main sample periods - consistent with Bartik IV models
*All CapitalIQ variables available 
***************************

* Robustness test: increase 10 and decrease 10 IPOs
/*
import delimited ipo_shocks1.csv, clear
**************
*Orignial
pctile pct = cutoff, nq(100)
keep if cutoff >= pct[90]
*********
*robustness test : 2008 - 2020 robustness
// keep if fyear >= 2008
// pctile pct = cutoff, nq(100)
// keep if cutoff >= pct[90]
***********
* robustness test:  + 10 = 72 / - 10 = 52
// egen rank = rank(-cutoff)
// keep if rank <= 72
******************
bysort ind: egen post_year1 = min(fyear)
keep ind post_year1
duplicates drop ind post_year1, force
drop if missing(ind)
save ipo_shocks.dta, replace
*/

merge m:1 ind using ipo_shocks.dta 
drop _merge
gen post2 = 0
// gen fyear1 = substr(datadate_drift,1,4)
// destring fyear1, replace
replace post2 = 1 if fyear>=post_year1
drop if missing(fyear)

keep if fyear >= 1990 & fyear <= 2020


***************

reghdfe analyst_count post lat lat2 age age2 bl intangible inst_ownership if market_level == 2|market_level == 3, absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("analyst_count") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, Small & Medium) alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes replace label 

forvalues i = 3(-1)1{
		reghdfe analyst_count post lat lat2 age age2 bl intangible inst_ownership if market_level == `i', absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("analyst_count") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, `i') alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes append label 
	}
	
	
foreach varname of varlist forecast_count avg_trading_turnover{

		reghdfe `varname' post lat lat2 age age2 bl intangible inst_ownership if market_level == 2|market_level == 3, absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("`varname'") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, Small & Medium) alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes append label 


	forvalues i = 3(-1)1{
		reghdfe `varname' post lat lat2 age age2 bl intangible inst_ownership if market_level == `i', absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("`varname'") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, `i') alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes append label 
	}
}
rm "foregin_ipo.txt"

******************************************************
*Hedge fund shares
******************************************************
replace hedge_share = hedge_share*100
reghdfe hedge_share post2 lat lat2 age age2 bl intangible inst_ownership if market_level == 2|market_level == 3, absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("hedge_share") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, Small & Medium) alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes replace label 

forvalues i = 3(-1)1{
		reghdfe hedge_share post2 lat lat2 age age2 bl intangible inst_ownership if market_level == `i', absorb(permno fyear) cluster(permno)

		outreg2 using "foregin_ipo.xls", excel adjr  ///
	nor2 addstat(Adjusted R-squared, e(r2_a), Obs2, e(N) + e(num_singletons)) ctitle("hedge_share") addtext(Firm F.E., Yes, Year F.E., Yes,  Cluster, Firm, Sample, `i') alpha(0.01, 0.05, 0.10) symbol(***, **, *) bdec(3) sdec(2) nocons nonotes append label 
	}
******************************************************
*Parallel Trends (2007-2020) pre=5, posst=5
******************************************************
drop if ind == ""
* difference btw current year and the event year 
gen eventtime = (fyear - post_year1)

* look at the distribution of max difference for each firm 
egen max_eventtime = max(eventtime), by(permno)
tab max_eventtime
sum eventtime

/*
max_eventtime = 5 has 10329 observations, max_eventtime = 4 has 3692 observations, max_eventtime = 3 has 2741 observation, 
which probably impies that many firms enter the market after the shock.
*/


* keep if event time difference is within the range 
 *keep if eventtime>=-`start' & eventtime<=`end'  /**************************************************************************/
local start 5
local end 5
gen within = 1 if eventtime>= -`start' & eventtime<=`end'
keep if eventtime ==. | within == 1 

* make sure there is enough after periods 
 *keep if max_eventtime>=`end'

local start 5
local end 5
forvalues i = -`start'(1)-1{
	local j = -`i'
	gen lead`j' = eventtime == `i'
}

forvalues i = 0(1)`end'{
	gen lag`i' = eventtime == `i'
}

/**************************************************************************************** nomarlly we omit the minus one year indicator? */

*Number of Analyst
eststo clear 
eststo trend: reghdfe analyst_count lead3-lead1 o.lag0 lag1-lag5 lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
coefplot trend, omitted baselevels levels(90) vertical drop(_cons lat lat2 age age2 bl intangible inst_ownership) yline(0) ciopts(recast(rarea) fcolor(navy*.15) lcolor(navy*.15 navy*.15 navy*.15) lwidth(thin) lpattern(solid solid solid)) xlabel(1 "-3" 2 "-2" 3 "-1" 4 "0" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5")  xtitle("Event Year") ytitle("analyst_count") xline(4) graphregion(color(white))

	graph export "analyst_count_parallel.png", replace
	
*Number of forecast
eststo trend2: reghdfe forecast_count lead3-lead1 o.lag0 lag1-lag5 lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
coefplot trend2, omitted baselevels levels(90) vertical drop(_cons lat lat2 age age2 bl intangible inst_ownership) yline(0) ciopts(recast(rarea) fcolor(navy*.15) lcolor(navy*.15 navy*.15 navy*.15) lwidth(thin) lpattern(solid solid solid)) xlabel(1 "-3" 2 "-2" 3 "-1" 4 "0" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5")  xtitle("Event Year") ytitle("forecast_count") xline(4) graphregion(color(white))
	graph export "forecast_count_parallel.png", replace

* Pre
eststo clear 
eststo trend: reghdfe analyst_count lead5-lead1 o.lag0 lag1-lag5 lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
coefplot trend, omitted baselevels levels(90) vertical drop(_cons lat lat2 age age2 bl intangible inst_ownership) yline(0) ciopts(recast(rarea) fcolor(navy*.15) lcolor(navy*.15 navy*.15 navy*.15) lwidth(thin) lpattern(solid solid solid)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5")  xtitle("Event Year") ytitle("analyst_count") xline(6) graphregion(color(white))

eststo trend2: reghdfe forecast_count lead5-lead1 o.lag0 lag1-lag5 lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
coefplot trend2, omitted baselevels levels(90) vertical drop(_cons lat lat2 age age2 bl intangible inst_ownership) yline(0) ciopts(recast(rarea) fcolor(navy*.15) lcolor(navy*.15 navy*.15 navy*.15) lwidth(thin) lpattern(solid solid solid)) xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5")  xtitle("Event Year") ytitle("forecast_count") xline(6) graphregion(color(white))

/************************************************************************************ Where does lead5 comes from?*/

reghdfe analyst_count lead5-lead2 lag0-lag5 lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[6-`x',1]
matrix res[6-`x',3]=temp[6-`x',5]
matrix res[6-`x',4]=temp[6-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+5,1]
matrix res[`x'+6,3]=temp[`x'+5,5]
matrix res[`x'+6,4]=temp[`x'+5,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("TWFE estimates with cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/TWFE/with covariates.png", replace
restore
	 
reghdfe analyst_count lead5-lead2 lag0-lag5 if market_level == 2 | market_level == 3, a(permno fyear)  cluster(permno) 
matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[6-`x',1]
matrix res[6-`x',3]=temp[6-`x',5]
matrix res[6-`x',4]=temp[6-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+5,1]
matrix res[`x'+6,3]=temp[`x'+5,5]
matrix res[`x'+6,4]=temp[`x'+5,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("TWFE estimates w/o cov") xtitle("Relative time to foreign IPO ") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/TWFE/without covariates.png", replace
restore


***********************
*** Borusyak et al. ***
*********************** 

did_imputation analyst_count permno fyear post_year1 if market_level == 2 | market_level == 3, fe(permno fyear) horizons(0 1 2 3 4 5) pretrends(5) cluster(permno) autosample
/*
 Number of obs     =     32,198
------------------------------------------------------------------------------
analyst_co~t |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        tau0 |   .0689443   .0564178     1.22   0.222    -.0416326    .1795211
        tau1 |   .0155354   .0729761     0.21   0.831    -.1274951    .1585658
        tau2 |  -.0611103   .0857367    -0.71   0.476    -.2291511    .1069305
        tau3 |  -.1102143   .0956236    -1.15   0.249    -.2976331    .0772045
        tau4 |  -.1915157   .1051281    -1.82   0.068     -.397563    .0145316
        tau5 |  -.2134903   .1199298    -1.78   0.075    -.4485484    .0215679
        pre1 |  -1.658654   1.985035    -0.84   0.403    -5.549251    2.231943
        pre2 |  -1.749824   1.986105    -0.88   0.378    -5.642519    2.142871
        pre3 |   -1.74759   1.987624    -0.88   0.379    -5.643261    2.148081
        pre4 |  -1.736848    1.98957    -0.87   0.383    -5.636334    2.162638
        pre5 |  -1.671611   1.991706    -0.84   0.401    -5.575282    2.232061
------------------------------------------------------------------------------
*/

did_imputation analyst_count permno fyear post_year1 if market_level == 2 | market_level == 3, fe(permno fyear) horizons(0 1 2 3 4 5) pretrends(5) nose autosample
/*

                                                Number of obs     =     32,198
------------------------------------------------------------------------------
analyst_co~t |      Coef.
-------------+----------------------------------------------------------------
        tau0 |   .0689443
        tau1 |   .0155354
        tau2 |  -.0611103
        tau3 |  -.1102143
        tau4 |  -.1915157
        tau5 |  -.2134903
        pre1 |  -1.658654
        pre2 |  -1.749824
        pre3 |   -1.74759
        pre4 |  -1.736848
        pre5 |  -1.671611
------------------------------------------------------------------------------
*/

did_imputation analyst_count permno fyear post_year1 if market_level == 2 | market_level == 3, fe(permno fyear) horizons(0 1 2 3 4 5) pretrends(3) cluster(permno) autosample

/*
Number of obs     =     32,198
------------------------------------------------------------------------------
analyst_co~t |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        tau0 |   .0689443   .0564178     1.22   0.222    -.0416326    .1795211
        tau1 |   .0155353   .0729761     0.21   0.831    -.1274951    .1585658
        tau2 |  -.0611103   .0857367    -0.71   0.476    -.2291511    .1069305
        tau3 |  -.1102143   .0956236    -1.15   0.249    -.2976331    .0772044
        tau4 |  -.1915157   .1051281    -1.82   0.068     -.397563    .0145316
        tau5 |  -.2134903   .1199298    -1.78   0.075    -.4485485    .0215679
        pre1 |   .0469767   .0782651     0.60   0.548      -.10642    .2003734
        pre2 |  -.0428571   .0663562    -0.65   0.518    -.1729129    .0871988
        pre3 |  -.0405881   .0515918    -0.79   0.431    -.1417061    .0605299
------------------------------------------------------------------------------
*/

matrix temp=r(table)'
matrix res=J(9,4,0)
forvalues x = 1/3 {
matrix res[4-`x',1]=-`x'
matrix res[4-`x',2]=temp[`x'+6,1]
matrix res[4-`x',3]=temp[`x'+6,5]
matrix res[4-`x',4]=temp[`x'+6,6]
}
forvalues x = 0/5 {
matrix res[`x'+4,1]=`x'
matrix res[`x'+4,2]=temp[`x'+1,1]
matrix res[`x'+4,3]=temp[`x'+1,5]
matrix res[`x'+4,4]=temp[`x'+1,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Borusyak, Jaravel & Spiess w/o cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") xlabel(-5(5)5) ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4)) 
graph export "graph/drop/did imputation/without covariates.png", replace
restore



******************************
*** Callaway and Sant'Anna ***
******************************

replace post_year1 = 0 if post_year1 == .
/*
 Groups that are never treated should be coded as Zero
*/
csdid analyst_count if market_level == 2 | market_level == 3, ivar(permno) time(fyear) gvar(post_year1) notyet method(dripw) long2
/*
defalut method drimp could not work
*/

estat event
/*
ATT by Periods Before and After treatment
Event Study:Dynamic effects
------------------------------------------------------------------------------
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     Pre_avg |  -.0063997   .0224151    -0.29   0.775    -.0503324    .0375331
    Post_avg |  -.0882782   .0704549    -1.25   0.210    -.2263673    .0498108
         Tm4 |  -.0952177   .0507654    -1.88   0.061     -.194716    .0042807
         Tm3 |  -.0341441   .0489854    -0.70   0.486    -.1301537    .0618655
         Tm2 |   .0178092   .0430593     0.41   0.679    -.0665854    .1022038
         Tm1 |   .0859539   .0439004     1.96   0.050    -.0000892    .1719971
         Tp0 |   .0359787   .0402698     0.89   0.372    -.0429486     .114906
         Tp1 |   .0407109   .0629896     0.65   0.518    -.0827463    .1641682
         Tp2 |  -.0776187   .0844881    -0.92   0.358    -.2432122    .0879749
         Tp3 |  -.0710296   .0946373    -0.75   0.453    -.2565153    .1144561
         Tp4 |  -.2419278   .1158853    -2.09   0.037    -.4690589   -.0147968
         Tp5 |   -.215783   .1290428    -1.67   0.094    -.4687022    .0371363
------------------------------------------------------------------------------
*/
matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[8-`x',1]
matrix res[6-`x',3]=temp[8-`x',5]
matrix res[6-`x',4]=temp[8-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+7,1]
matrix res[`x'+6,3]=temp[`x'+7,5]
matrix res[`x'+6,4]=temp[`x'+7,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Callaway & Sant'Anna w/o cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/csdid/control group not yet.png", replace
restore

csdid analyst_count if market_level == 2 | market_level == 3, ivar(permno) time(fyear) gvar(post_year1) method(dripw) long2
estat event
/*
ATT by Periods Before and After treatment
Event Study:Dynamic effects
------------------------------------------------------------------------------
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     Pre_avg |  -.0168431   .0251091    -0.67   0.502    -.0660561    .0323699
    Post_avg |  -.0984722   .0716064    -1.38   0.169    -.2388182    .0418738
         Tm4 |  -.0952177   .0507654    -1.88   0.061     -.194716    .0042807
         Tm3 |  -.0427192   .0488606    -0.87   0.382    -.1384842    .0530458
         Tm2 |  -.0082951   .0444221    -0.19   0.852    -.0953608    .0787706
         Tm1 |   .0788597   .0451207     1.75   0.081    -.0095752    .1672945
         Tp0 |   .0300417   .0419352     0.72   0.474    -.0521498    .1122332
         Tp1 |   .0177774    .064515     0.28   0.783    -.1086697    .1442245
         Tp2 |  -.0919028   .0855564    -1.07   0.283    -.2595903    .0757846
         Tp3 |  -.0890387   .0966523    -0.92   0.357    -.2784737    .1003964
         Tp4 |  -.2419278   .1158853    -2.09   0.037    -.4690589   -.0147968
         Tp5 |   -.215783   .1290428    -1.67   0.094    -.4687022    .0371363
------------------------------------------------------------------------------
*/
matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[8-`x',1]
matrix res[6-`x',3]=temp[8-`x',5]
matrix res[6-`x',4]=temp[8-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+7,1]
matrix res[`x'+6,3]=temp[`x'+7,5]
matrix res[`x'+6,4]=temp[`x'+7,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Callaway & Sant'Anna w/o cov never-treated") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/csdid/control group never treated.png", replace
restore


csdid analyst_count lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, ivar(permno) time(fyear) gvar(post_year1) notyet long2 method(dripw)
estat event

/*
ATT by Periods Before and After treatment
Event Study:Dynamic effects
------------------------------------------------------------------------------
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     Pre_avg |    -.19585   .1614672    -1.21   0.225    -.5123198    .1206198
    Post_avg |   .1042146   .1002987     1.04   0.299    -.0923673    .3007965
         Tm4 |  -.1321907   .3933322    -0.34   0.737    -.9031077    .6387263
         Tm3 |  -.2965863   .2258369    -1.31   0.189    -.7392184    .1460458
         Tm2 |  -.3194366   .1170821    -2.73   0.006    -.5489133     -.08996
         Tm1 |  -.0351864   .0686688    -0.51   0.608    -.1697748    .0994019
         Tp0 |   .1154052    .068403     1.69   0.092    -.0186621    .2494726
         Tp1 |   .1400739    .088553     1.58   0.114    -.0334868    .3136346
         Tp2 |    .102169   .1203273     0.85   0.396    -.1336681    .3380061
         Tp3 |    .113541   .1252086     0.91   0.365    -.1318633    .3589453
         Tp4 |   .1012927   .2243004     0.45   0.652    -.3383281    .5409134
         Tp5 |    .052806   .1830114     0.29   0.773    -.3058897    .4115018
------------------------------------------------------------------------------
*/
matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[8-`x',1]
matrix res[6-`x',3]=temp[8-`x',5]
matrix res[6-`x',4]=temp[8-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+7,1]
matrix res[`x'+6,3]=temp[`x'+7,5]
matrix res[`x'+6,4]=temp[`x'+7,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Callaway & Sant'Anna with cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") 
graph export "graph/drop/csdid/with covariates.png", replace
restore

csdid analyst_count lat lat2 age age2 bl intangible inst_ownership if market_level == 2 | market_level == 3, ivar(permno) time(fyear) gvar(post_year1) long2 method(dripw)
estat event
/*
Event Study:Dynamic effects
------------------------------------------------------------------------------
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     Pre_avg |  -.1936222    .163606    -1.18   0.237     -.514284    .1270396
    Post_avg |   .0939224   .1022284     0.92   0.358    -.1064416    .2942865
         Tm4 |  -.1321907   .3933322    -0.34   0.737    -.9031077    .6387263
         Tm3 |  -.3015919   .2293776    -1.31   0.189    -.7511638      .14798
         Tm2 |  -.2954621   .1211177    -2.44   0.015    -.5328483   -.0580758
         Tm1 |  -.0452442   .0708279    -0.64   0.523    -.1840642    .0935759
         Tp0 |   .0909856   .0713499     1.28   0.202    -.0488577    .2308289
         Tp1 |   .1309967   .0924782     1.42   0.157    -.0502573    .3122506
         Tp2 |   .0836547   .1228891     0.68   0.496    -.1572036     .324513
         Tp3 |   .1037989   .1279068     0.81   0.417    -.1468938    .3544917
         Tp4 |   .1012927   .2243004     0.45   0.652    -.3383281    .5409134
         Tp5 |    .052806   .1830114     0.29   0.773    -.3058897    .4115018
------------------------------------------------------------------------------
*/

matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[8-`x',1]
matrix res[6-`x',3]=temp[8-`x',5]
matrix res[6-`x',4]=temp[8-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+7,1]
matrix res[`x'+6,3]=temp[`x'+7,5]
matrix res[`x'+6,4]=temp[`x'+7,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Callaway & Sant'Anna with cov ") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect")

graph export "graph/drop/csdid/with covariates never-treated.png", replace
restore
/*
 Remark 1. When Panel estimators are used, asymptotic and Wbootstrap Standard errors are already clustered at the panel level.  When using cluster, one is effectively requesting a
                            two-way cluster estimation.
*/

replace post_year1 = . if post_year1 == 0

*****************************************
*** DeChaisemartin and D'Haultfeuille ***
*****************************************

preserve

rename ind industry

twowayfeweights analyst_count permno fyear post2 if market_level == 2 | market_level == 3, type(feTR) test_random_weights(fyear)

/*

Under the common trends assumption, beta estimates a weighted sum of 13892 ATTs. 
11281 ATTs receive a positive weight, and 2611 receive a negative weight.
The sum of the positive weights is equal to 1.0730617.
The sum of the negative weights is equal to -.07306171.
beta is compatible with a DGP where the average of those ATTs is equal to 0,
while their standard deviation is equal to ..
beta is compatible with a DGP where those ATTs all are of a different sign than beta,
while their standard deviation is equal to ..

Regression of variables possibly correlated with the treatment effect on the weights

B[1,4]
              Coef           SE       t-stat  Correlation
fyear    1.0281778    .09574378    10.738847     .1312533
*/

twowayfeweights analyst_count permno fyear post2 if market_level == 2 | market_level == 3, type(feTR) test_random_weights(fyear) controls(lat lat2 age age2 bl intangible inst_ownership)
/*
Under the common trends assumption, beta estimates a weighted sum of 13828 ATTs. 
11074 ATTs receive a positive weight, and 2754 receive a negative weight.
The sum of the positive weights is equal to 1.0888841.
The sum of the negative weights is equal to -.08888412.
beta is compatible with a DGP where the average of those ATTs is equal to 0,
while their standard deviation is equal to ..
beta is compatible with a DGP where those ATTs all are of a different sign than beta,
while their standard deviation is equal to ..

Regression of variables possibly correlated with the treatment effect on the weights

B[1,4]
              Coef           SE       t-stat  Correlation
fyear    .76850223    .09106654    8.4389089    .10226432
*/


did_multiplegt analyst_count permno fyear post2 if market_level == 2 | market_level == 3, ///
robust_dynamic longdiff_placebo dynamic(5) placebo(5) breps(100) seed(1) cluster(permno) 
/*
longdiff_placebo: this option can be used when the robust_dynamic option is specified.  When this option is specified, the lth placebo compares first-time switchers' and not-yet switchers' outcomes evolution, from the last period before first-time switchers' treatment changes to the l+1th period before that change (this comparison goes from the future towards the past, to be consistent with event-study regressions where everything is relative to the period prior to the treatment change). Thus, the lth placebo assesses if parallel trends holds over l+1 periods, the number of periods overc which parallel trends has to hold for the lth dynamic effect to be unbiased.  When this option is not specified, the lth placebo compares first-time switchers' and not-yet switchers' outcome evolution, from the l+1th to the lth period before first-time switchers' treatment changes.  
*/

did_multiplegt analyst_count permno fyear post2 if market_level == 2 | market_level == 3, ///
robust_dynamic longdiff_placebo dynamic(5) placebo(5) breps(200) seed(1) cluster(permno) control()
/*
 The DID_M and DID_l estimators with control variables are similar to those without controls, except that the first-difference of the outcome is replaced by residuals from regressions of the
    first-difference of the outcome on the first-differences of the controls and time fixed effects.
*/
did_multiplegt analyst_count permno fyear post2 if market_level == 2 | market_level == 3, ///
robust_dynamic longdiff_placebo dynamic(5) placebo(5) breps(200) seed(1) cluster(permno) trends_lin()
/* with unit—specific trend */



restore








***********************
*** Sun and Abraham ***
***********************

gen never_treated = (post_year1 == .)

eventstudyweights analyst_count lead2 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
rel_time(eventtime)
mat list e(weights)

eventstudyweights analyst_count lead5-lead1 lag0-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
 rel_time(eventtime) control_cohort(never_treated) covariates(lat lat2 age age2 bl intangible inst_ownership)
mat list e(weights)

eventstudyinteract analyst_count lead5-lead1 lag0-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
control_cohort(never_treated) vce(cluster permno)


/*
(obs=25,139)

IW estimates for dynamic effects                Number of obs     =     35,942
Absorbing 2 HDFE groups                         F( 141,   6056)   =       8.07
                                                Prob > F          =     0.0000
                                                R-squared         =     0.7519
                                                Adj R-squared     =     0.6999
                                                Root MSE          =     1.8007
                             (Std. Err. adjusted for 6,057 clusters in permno)
------------------------------------------------------------------------------
             |               Robust
analyst_co~t |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       lead5 |   .4197274   .1775415     2.36   0.018     .0716828     .767772
       lead4 |    .223613    .190422     1.17   0.240    -.1496819    .5969078
       lead3 |   .2307039   .1738226     1.33   0.184    -.1100502     .571458
       lead2 |   .1053172   .1971127     0.53   0.593    -.2810938    .4917281
       lead1 |   .1737483   .2030149     0.86   0.392    -.2242331    .5717296
        lag0 |   .1303409   .1995987     0.65   0.514    -.2609437    .5216255
        lag1 |   .0951805   .1835361     0.52   0.604    -.2646155    .4549765
        lag2 |  -.0873976   .2201633    -0.40   0.691    -.5189961    .3442009
        lag3 |  -.0747115   .2232727    -0.33   0.738    -.5124053    .3629824
        lag4 |  -.0507684   .2071096    -0.25   0.806    -.4567769    .3552402
        lag5 |  -.1040639   .1929401    -0.54   0.590    -.4822953    .2741674
------------------------------------------------------------------------------
*/



eventstudyinteract analyst_count lead5-lead2 lag0-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
control_cohort(never_treated) vce(cluster permno)

matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[6-`x',1]
matrix res[6-`x',3]=temp[6-`x',5]
matrix res[6-`x',4]=temp[6-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+5,1]
matrix res[`x'+6,3]=temp[`x'+5,5]
matrix res[`x'+6,4]=temp[`x'+5,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Sun and Abraham w/o cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/eventstudyinteract/without covariates.png", replace
restore

/*
(obs=25,139)

IW estimates for dynamic effects                Number of obs     =     35,942
Absorbing 2 HDFE groups                         F( 138,   6056)   =       2.19
                                                Prob > F          =     0.0000
                                                R-squared         =     0.7518
                                                Adj R-squared     =     0.6998
                                                Root MSE          =     1.8010
                             (Std. Err. adjusted for 6,057 clusters in permno)
------------------------------------------------------------------------------
             |               Robust
analyst_co~t |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       lead5 |    -.04066   .1010214    -0.40   0.687    -.2386978    .1573779
       lead4 |   -.103589   .0850146    -1.22   0.223    -.2702479    .0630698
       lead3 |  -.0984843   .0690349    -1.43   0.154    -.2338173    .0368487
       lead2 |  -.0949654   .0499268    -1.90   0.057    -.1928397     .002909
        lag0 |  -.0385236   .0467052    -0.82   0.410    -.1300825    .0530352
        lag1 |  -.0579434   .0652888    -0.89   0.375    -.1859326    .0700459
        lag2 |   -.140306   .0779622    -1.80   0.072    -.2931396    .0125276
        lag3 |  -.1050966   .0876659    -1.20   0.231    -.2769529    .0667597
        lag4 |  -.1127908    .095616    -1.18   0.238    -.3002321    .0746505
        lag5 |   -.186741   .1067101    -1.75   0.080    -.3959307    .0224488
------------------------------------------------------------------------------
*/



eventstudyinteract analyst_count lead5-lead1 lag1-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
control_cohort(never_treated) vce(cluster permno)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2])

/*
(obs=25,139)

IW estimates for dynamic effects                Number of obs     =     35,942
Absorbing 2 HDFE groups                         F( 138,   6056)   =       2.22
                                                Prob > F          =     0.0000
                                                R-squared         =     0.7518
                                                Adj R-squared     =     0.6998
                                                Root MSE          =     1.8010
                             (Std. Err. adjusted for 6,057 clusters in permno)
------------------------------------------------------------------------------
             |               Robust
analyst_co~t |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       lead5 |   .0491131   .1120727     0.44   0.661    -.1705892    .2688154
       lead4 |  -.0303496   .0978085    -0.31   0.756     -.222089    .1613898
       lead3 |  -.0261186   .0815092    -0.32   0.749    -.1859057    .1336685
       lead2 |  -.0478299   .0664695    -0.72   0.472    -.1781337    .0824738
       lead1 |   .0362395   .0490067     0.74   0.460    -.0598311      .13231
        lag1 |    -.03083   .0494239    -0.62   0.533    -.1277185    .0660585
        lag2 |  -.1241015   .0657124    -1.89   0.059    -.2529211    .0047181
        lag3 |  -.0942447   .0784179    -1.20   0.229    -.2479716    .0594822
        lag4 |  -.1016008   .0880629    -1.15   0.249    -.2742355    .0710339
        lag5 |  -.1760202   .0981756    -1.79   0.073    -.3684792    .0164389
------------------------------------------------------------------------------
*/



eventstudyinteract analyst_count lead5-lead1 lag1-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
control_cohort(never_treated) covariates(lat lat2 age age2 bl intangible inst_ownership) vce(cluster permno)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2])

/*
IW estimates for dynamic effects                Number of obs     =     35,807
Absorbing 2 HDFE groups                         F( 145,   6043)   =      12.59
                                                Prob > F          =     0.0000
                                                R-squared         =     0.8034
                                                Adj R-squared     =     0.7621
                                                Root MSE          =     1.6020
                             (Std. Err. adjusted for 6,044 clusters in permno)
------------------------------------------------------------------------------
             |               Robust
analyst_co~t |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       lead5 |    .047946   .1017784     0.47   0.638     -.151576     .247468
       lead4 |  -.0021163   .0892031    -0.02   0.981    -.1769861    .1727536
       lead3 |   .0060132    .074058     0.08   0.935    -.1391669    .1511933
       lead2 |  -.0494097   .0601402    -0.82   0.411    -.1673059    .0684865
       lead1 |   .0150195   .0449312     0.33   0.738    -.0730616    .1031006
        lag1 |    .018599   .0462011     0.40   0.687    -.0719716    .1091696
        lag2 |  -.1384409   .0599716    -2.31   0.021    -.2560067   -.0208752
        lag3 |  -.1214322   .0694125    -1.75   0.080    -.2575055    .0146411
        lag4 |  -.1682108   .0773581    -2.17   0.030    -.3198603   -.0165613
        lag5 |  -.2061342   .0845061    -2.44   0.015    -.3717963   -.0404722
------------------------------------------------------------------------------
*/

eventstudyinteract analyst_count lead5-lead2 lag0-lag5 if market_level == 2 | market_level == 3, absorb(i.fyear i.permno) cohort(post_year1) ///
control_cohort(never_treated) covariates(lat lat2 age age2 bl intangible inst_ownership) vce(cluster permno)

matrix temp=r(table)'
matrix res=J(11,4,0)
matrix res[5,1]=-1
forvalues x = 2/5 {
matrix res[6-`x',1]=-`x'
matrix res[6-`x',2]=temp[6-`x',1]
matrix res[6-`x',3]=temp[6-`x',5]
matrix res[6-`x',4]=temp[6-`x',6]
}
forvalues x = 0/5 {
matrix res[`x'+6,1]=`x'
matrix res[`x'+6,2]=temp[`x'+5,1]
matrix res[`x'+6,3]=temp[`x'+5,5]
matrix res[`x'+6,4]=temp[`x'+5,6]
}

preserve
svmat res
twoway (scatter res2 res1, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line res2 res1, lcolor(navy)) (rcap res4 res3 res1, lcolor(maroon)), ///
	 title("Sun and Abraham with cov") xtitle("Relative time to foreign IPO") ///
	 ytitle("Effect") ylabel(-0.5(.1)0.4) yscale(range(-0.5 0.4))
graph export "graph/drop/eventstudyinteract/with covariates.png", replace
restore
/*
(obs=25,035)

IW estimates for dynamic effects                Number of obs     =     35,807
Absorbing 2 HDFE groups                         F( 145,   6043)   =      12.64
                                                Prob > F          =     0.0000
                                                R-squared         =     0.8034
                                                Adj R-squared     =     0.7621
                                                Root MSE          =     1.6020
                             (Std. Err. adjusted for 6,044 clusters in permno)
------------------------------------------------------------------------------
             |               Robust
analyst_co~t |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       lead5 |  -.0230393   .0927144    -0.25   0.804    -.2047925     .158714
       lead4 |  -.0493289   .0781261    -0.63   0.528    -.2024839     .103826
       lead3 |  -.0453231   .0635239    -0.71   0.476    -.1698527    .0792064
       lead2 |   -.078412   .0464265    -1.69   0.091    -.1694244    .0126004
        lag0 |  -.0237065   .0429347    -0.55   0.581    -.1078739    .0604609
        lag1 |   .0040613   .0589912     0.07   0.945    -.1115824    .1197051
        lag2 |  -.1370514   .0696556    -1.97   0.049    -.2736011   -.0005016
        lag3 |  -.1197514   .0770373    -1.55   0.120    -.2707719    .0312691
        lag4 |  -.1663551   .0826661    -2.01   0.044    -.3284101   -.0043001
        lag5 |  -.2016963   .0906968    -2.22   0.026    -.3794945   -.0238981
------------------------------------------------------------------------------

*/
