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

//////// Examine Change in long-term rate
// FRT
xtreg dnewspread lnewspread dfrt lfrt lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dnewspread lnewspread dhrv_mean lhrv_mean , cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV1.dta", detail(all) replace table(LongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

//////// Examine Change in long-term rate spread
//FRT
xtreg dltspreadus lltspreadus lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dltspreadus lltspreadus lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV2.dta", detail(all) replace table(ChangeLongRunRate, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


//////// Examine Spread volatility
// FRT
xtreg dratecov lltratecov lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dratecov lltratecov lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    regsave using "tables/HRV3.dta", detail(all) replace table(Volatility, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
