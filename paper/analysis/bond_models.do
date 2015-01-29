///////////////////////
// Compare transparencies' affects on sovereign bonds
// 29 January 2015
// MIT License
///////////////////////

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/paper/"

// Load data
use "analysis/frt_hrv_obi_bond.dta"

//////// Examine Change in long-term rate
// FRT
xtreg dltrate lltrate lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/FRT1.dta", detail(all) replace table(LongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// HRV
xtreg dltrate lltrate lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV1.dta", detail(all) replace table(LongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// OBI
xtreg dltrate lltrate lobi_filled dobi_filled lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

//////// Examine Change in long-term rate spread
//FRT
xtreg dltspreadus lltspreadus lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/FRT2.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// HRV
xtreg dltspreadus lltspreadus lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV2.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// OBI
xtreg dltspreadus lltspreadus lobi_filled dobi_filled  lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit

//////// Examine Spread volatility
// FRT
xtreg dratecov lltratecov lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/FRT3.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// FRT with country growth and Eurozone
xtreg dratecov lltratecov lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix lcountry_growth dcountry_growth eurozone, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/FRT4.dta", detail(all) replace table(Volatility_morevars, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


// HRV
xtreg dratecov lltratecov lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV3.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// OBI
xtreg dratecov lltratecov lobi_filled dobi_filled lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
