/*
FRT/Debt regressions, including IMF programme dummies and democracy level
October 2016
*/

/* 1. Clean the slate. */
clear
set more off

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/paper/"

use "analysis/frt08_16_v2.dta"

* Create interactions
gen l_frt2015xl_pub_gen = l_frt_2015 * l_pubdebtgdp_gen
gen d_frt_2015xd_pubdebtgdp_gen = d_frt_2015 * d_pubdebtgdp_gen

gen l_frt2015xd_pubdebtgdp_gen = l_frt_2015 * d_pubdebtgdp_gen
gen d_frt_2015xl_pub_gen = d_frt_2015 * l_pubdebtgdp_gen

* Subset sample to OECD/non-Japan (other countries lack FRED bond yield data)
keep if country != "Russian Federation" & country != "South Africa" & country != "Japan"

gen imf_program = 0
replace imf_program = 1 if country == "Iceland" & year == 2008
replace imf_program = 1 if country == "Greece" & year == 2010
replace imf_program = 1 if country == "Ireland" & year == 2010
replace imf_program = 1 if country == "Portugal" & year == 2011
replace imf_program = 1 if country == "Korea, Republic of" & year == 1997
replace imf_program = 1 if country == "Mexico" & year == 1995

gen imf_under_program = 0
replace imf_under_program = 1 if country == "Iceland" & year >= 2008 & year <= 2011
replace imf_under_program = 1 if country == "Greece" & year >= 2010
replace imf_under_program = 1 if country == "Ireland" & year >= 2010 & year <= 2013
replace imf_under_program = 1 if country == "Portugal" & year == 2011 & year <= 2014

sort imf_code year
by imf_code: gen imf_program_lag = imf_program[_n-1]

sort imf_code year
by imf_code: gen imf_under_program_lag = imf_under_program[_n-1]

/* ECM Models */

/************** Spreads ********************/
* Spreads non-interactive
xtreg d_bond_spread_fred l_bond_spread_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_uds d_uds imf_program_lag ///
	if country!="United States", cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_1_imf.dta", detail(all) replace table(nonInteractSpread, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

* Spreads interactive
xtreg d_bond_spread_fred l_bond_spread_fred
	l_frt_2015 d_frt_2015 ///
	l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_frt2015xl_pub_gen d_frt_2015xd_pubdebtgdp_gen ///
	l_frt2015xd_pubdebtgdp_gen d_frt_2015xl_pub_gen ///
	l_infl d_infl ///
	l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth ///
	l_us3mrate d_us3mrate l_vix d_vix ///
	l_uds d_uds imf_program_lag ///
	if country!="United States", ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

	test l_frt_2015 l_pubdebtgdp_gen l_frt2015xl_pub_gen



/************** Volatility ********************/
* Coefficient of variation non-interactive
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l ///
	d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_uds d_uds imf_program_lag, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_3_imf.dta", detail(all) replace table(frt1, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())


* Coefficient of variation interactive
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_frt2015xl_pub_gen d_frt_2015xd_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_cgdpgrowth d_cgdpgrowth l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate ///
	l_vix d_vix ///
	l_uds d_uds imf_program_lag, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_4_imf.dta", detail(all) replace table(frt2, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())
