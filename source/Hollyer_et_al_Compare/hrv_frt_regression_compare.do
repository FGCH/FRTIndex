///////////////////////
// Compare HRV and FRT affect on sovereign bonds
// Christopher Gandrud
// MIT License
///////////////////////

clear

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/"

// Load data
use "/git_repositories/FRTIndex/paper/analysis/frt08_16_v2.dta"

* Create interactions
gen l_hrvxl_pub_gen = l_hrv_mean * l_pubdebtgdp_gen
gen d_hrvxd_pubdebtgdp_gen = d_hrv_mean * d_pubdebtgdp_gen

* Subset sample to OECD/non-Japan (other countries lack FRED bond yield data)
keep if country != "Russian Federation" & country != "South Africa" & country != "Japan"

gen imf_program = 0
*replace imf_program = 1 if country == "Iceland" & year == 2008
replace imf_program = 1 if country == "Greece" & year == 2010
replace imf_program = 1 if country == "Ireland" & year == 2010
replace imf_program = 1 if country == "Portugal" & year == 2011
replace imf_program = 1 if country == "Korea, Republic of" & year == 1997
replace imf_program = 1 if country == "Mexico" & year == 1995

gen imf_under_program = 0
*replace imf_under_program = 1 if country == "Iceland" & year >= 2008 & year <= 2011
replace imf_under_program = 1 if country == "Greece" & year >= 2010
replace imf_under_program = 1 if country == "Ireland" & year >= 2010 & year <= 2013
replace imf_under_program = 1 if country == "Portugal" & year == 2011 & year <= 2014

sort imf_code year
by imf_code: gen imf_program_lag = imf_program[_n-1]

sort imf_code year
by imf_code: gen imf_under_program_lag = imf_under_program[_n-1]

//////// Examine Change in long-term rate spread
// HRV
xtreg d_bond_spread_fred l_bond_spread_fred l_hrv_mean d_hrv_mean l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_uds d_uds ///
	if country!="United States", cluster(imf_code) i(imf_code) fe vsquish noomit

    regsave using "tables/HRV_1.dta", detail(all) replace ///
    	table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg d_bond_spread_fred l_bond_spread_fred l_hrv_mean d_hrv_mean l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_hrvxl_pub_gen d_hrvxd_pubdebtgdp_gen l_infl d_infl ///
	l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth ///
	 l_us3mrate d_us3mrate l_vix d_vix ///
	 l_uds d_uds ///
	 if country!="United States", ///
	 cluster(imf_code) i(imf_code) fe vsquish noomit    
    
    regsave using "tables/HRV_2.dta", detail(all) replace ///
    	table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


//////// Examine Spread volatility
// HRV
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_hrv_mean d_hrv_mean l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l ///
	d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix ///
	l_uds d_uds, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit    
    
    regsave using "tables/HRV_3.dta", detail(all) replace ///
    	table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_hrv_mean d_hrv_mean l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_hrvxl_pub_gen d_hrvxd_pubdebtgdp_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate ///
	l_vix d_vix l_uds d_uds, cluster(imf_code) i(imf_code) fe vsquish noomit
	
	regsave using "tables/HRV_4.dta", detail(all) replace ///
		table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


////// Residuals
regress l_hrv_mean l_frt_2015
predict l_frt_residuals, residuals

regress d_hrv_mean d_frt
predict d_frt_residuals, residuals

// Create residual interactions
gen l_frt_residxlpub = l_frt_residuals * l_pubdebtgdp_gen
gen d_frt_residxdpub = d_frt_residuals * d_pubdebtgdp_gen

xtreg d_bond_spread_fred l_bond_spread_fred l_frt_residuals d_frt_residuals ///
	l_pubdebtgdp_gen d_pubdebtgdp_gen l_frt_residxlpub d_frt_residxdpub l_inf d_infl ///
	l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth ///
	 l_us3mrate d_us3mrate l_vix d_vix l_uds d_uds if country != "United States", ///
	 cluster(imf_code) i(imf_code) fe vsquish noomit
	
	regsave using "tables/HRV_5.dta", detail(all) replace ///
		table(residuals, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
