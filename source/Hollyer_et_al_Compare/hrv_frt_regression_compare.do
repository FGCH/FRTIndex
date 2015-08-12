///////////////////////
// Compare HRV and FRT affect on sovereign bonds
// Christopher Gandrud
// MIT License
///////////////////////

clear

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/"

// Load data
insheet using "frt_hrv_bond.csv", comma

// Create interactions
gen lhrvxlpub = lhrv_mean * lpubdebtgdp
gen dhrvxdpub = dhrv_mean * dpubdebtgdp

//////// Examine Change in long-term rate spread
//FRT
xtreg dnewspread lnewspread lfrt dfrt lpubdebtgdp dpubdebtgdp linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit
xtreg dnewspread lnewspread lfrt dfrt lpubdebtgdp dpubdebtgdp lfrtxlpub dfrtxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dnewspread lnewspread lhrv_mean dhrv_mean lpubdebtgdp dpubdebtgdp linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV_1.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg dnewspread lnewspread lhrv_mean dhrv_mean lpubdebtgdp dpubdebtgdp lhrvxlpub dhrvxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV_2.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


//////// Examine Spread volatility
// FRT
xtreg dratecov lltratecov lfrt dfrt lpubdebtgdp dpubdebtgdp linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
xtreg dratecov lltratecov lfrt dfrt lpubdebtgdp dpubdebtgdp lfrtxlpub dfrtxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dratecov lltratecov lhrv_mean dhrv_mean lpubdebtgdp dpubdebtgdp linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV_3.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

xtreg dratecov lltratecov lhrv_mean dhrv_mean lpubdebtgdp dpubdebtgdp lhrvxlpub dhrvxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV_4.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


////// Residuals
regress lhrv_mean lfrt
predict lfrt_residuals, residuals

regress dhrv_mean dfrt
predict dfrt_residuals, residuals

// Create residual interactions
gen lfrt_residxlpub = lfrt_residuals * lpubdebtgdp
gen dfrt_residxdpub = dfrt_residuals * dpubdebtgdp

xtreg dnewspread lnewspread lfrt_residuals dfrt_residuals lpubdebtgdp dpubdebtgdp lfrt_residxlpub dfrt_residxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit
	regsave using "tables/HRV_5.dta", detail(all) replace table(residuals, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
