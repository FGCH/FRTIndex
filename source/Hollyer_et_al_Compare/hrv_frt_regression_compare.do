///////////////////////
// Compare HRV and FRT affect on sovereign bonds
// Christopher Gandrud
// MIT License
///////////////////////

clear

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/"

// Load data
use "/git_repositories/FRTIndex/paper/analysis/frt04_16_v1.dta"

// Create interactions
gen l_hrvxl_pub = l_hrv_mean * l_pubdebtgdp
gen d_hrvxd_pub = d_hrv_mean * d_pubdebtgdp

//////// Examine Change in long-term rate spread
//FRT
xtreg d_bond_spread_fred l_bond_spread_fred l_frt d_frt l_pubdebtgdp d_pubdebtgdp l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit
xtreg d_bond_spread_fred l_bond_spread_fred l_frt d_frt l_pubdebtgdp d_pubdebtgdp l_frtxl_pub d_frtxd_pub l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit

// HRV
xtreg d_bond_spread_fred l_bond_spread_fred l_hrv_mean d_hrv_mean l_pubdebtgdp d_pubdebtgdp l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit
    regsave using "tables/HRV_1.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg d_bond_spread_fred l_bond_spread_fred l_hrv_mean d_hrv_mean l_pubdebtgdp d_pubdebtgdp l_hrvxl_pub d_hrvxd_pub l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit
    regsave using "tables/HRV_2.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


//////// Examine Spread volatility
// FRT
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt d_frt l_pubdebtgdp d_pubdebtgdp l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix, cluster(imf_code) i(imf_code) fe vsquish noomit
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt d_frt l_pubdebtgdp d_pubdebtgdp l_frtxl_pub d_frtxd_pub l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix, cluster(imf_code) i(imf_code) fe vsquish noomit

// HRV
xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_hrv_mean d_hrv_mean l_pubdebtgdp d_pubdebtgdp l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix, cluster(imf_code) i(imf_code) fe vsquish noomit
    regsave using "tables/HRV_3.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_hrv_mean d_hrv_mean l_pubdebtgdp d_pubdebtgdp l_hrvxl_pub d_hrvxd_pub l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix, cluster(imf_code) i(imf_code) fe vsquish noomit
    regsave using "tables/HRV_4.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


////// Residuals
regress l_hrv_mean l_frt
predict l_frt_residuals, residuals

regress d_hrv_mean d_frt
predict d_frt_residuals, residuals

// Create residual interactions
gen l_frt_residxlpub = l_frt_residuals * l_pubdebtgdp
gen d_frt_residxdpub = d_frt_residuals * d_pubdebtgdp

xtreg d_bond_spread_fred l_bond_spread_fred l_frt_residuals d_frt_residuals l_pubdebtgdp d_pubdebtgdp l_frt_residxlpub d_frt_residxdpub l_inf d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit
	regsave using "tables/HRV_5.dta", detail(all) replace table(residuals, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
