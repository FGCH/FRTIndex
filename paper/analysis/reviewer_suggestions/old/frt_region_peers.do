/*
FRT/Debt regressions, including Regional peer weights
August 2016
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

gen l_frt2015xl_reg_peer = l_frt_2015 * l_sp_wght_region_d_bnd_sprd_frd
gen d_frt_2015xl_reg_peer = d_frt_2015 * d_sp_wght_region_d_bnd_sprd_frd

* Subset sample to OECD/non-Japan (other countries lack FRED bond yield data)
keep if country != "Russian Federation" & country != "South Africa" & country != "Japan"

/* ECM Models */

/************** Spreads ********************/
* Spreads non-interactive
xtreg d_bond_spread_fred l_bond_spread_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_sp_wght_region_d_bnd_sprd_frd d_sp_wght_region_d_bnd_sprd_frd ///
	if country!="United States", cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_1_region_peer.dta", detail(all) replace table(nonInteractSpread, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

* Spreads interactive
xtreg d_bond_spread_fred l_bond_spread_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_frt2015xl_pub_gen d_frt_2015xd_pubdebtgdp_gen l_infl d_infl ///
	l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth ///
	l_us3mrate d_us3mrate l_vix d_vix ///
	l_sp_wght_region_d_bnd_sprd_frd d_sp_wght_region_d_bnd_sprd_frd ///
	if country!="United States", ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_2_region_peer.dta", detail(all) replace table(nonInteractCov, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

/************** Volatility ********************/
* Coefficient of variation non-interactive
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l ///
	d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_sp_wght_region_d_lt_ratcv_frd d_sp_wght_region_d_lt_ratcv_frd, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_3_region_peer.dta", detail(all) replace table(frt1, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())


* Coefficient of variation interactive
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt_2015 d_frt_2015 l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_frt2015xl_pub_gen d_frt_2015xd_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate ///
	l_vix d_vix ///
	l_sp_wght_region_d_lt_ratcv_frd d_sp_wght_region_d_lt_ratcv_frd, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

regsave using "tables/reviewer_suggestions/FRT_4_region_peer.dta", detail(all) replace table(frt2, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())
