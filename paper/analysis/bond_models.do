///////////////////////
// Compare transparencies' affects on sovereign bonds
// 30 January 2015
// MIT License
///////////////////////

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/paper/"

// Load data
use "analysis/frt_hrv_obi_bond.dta", clear

// Log transform FRT
gen lfrt_log = log(lfrt + 2.5) // 2.5 keeps the values above 0
gen dfrt_log = log(dfrt + 2.5)

//////// Examine Change in long-term rate //////////////////////////////////////
// FRT
xtreg dltrate lltrate lfrt dfrt lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/FRT1.dta", detail(all) replace table(LongRunRate, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// HRV
xtreg dltrate lltrate lhrv_mean dhrv_mean ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth ///
    doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/HRV1.dta", detail(all) replace table(LongRunRate, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// OBI
xtreg dltrate lltrate lobi_filled dobi_filled ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth ///
    doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

//////// Examine Change in long-term rate spread ///////////////////////////////
//FRT
xtreg dltspreadus lltspreadus lfrt dfrt lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/FRT2.dta", detail(all) replace ///
        table(ChangeLongRunRate, order(regvars r2) format(%5.2f) ///
        paren(stderr) asterisk())

// HRV
xtreg dltspreadus lltspreadus lhrv_mean dhrv_mean ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth ///
    doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/HRV2.dta", detail(all) replace ///
        table(ChangeLongRunRate, order(regvars r2) format(%5.2f) ///
        paren(stderr) asterisk())

// OBI
xtreg dltspreadus lltspreadus lobi_filled dobi_filled  ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate ///
    loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) ///
    fe vsquish noomit

//////// Examine Spread volatility /////////////////////////////////////////////
// FRT --- Basic
xtreg dratecov lltratecov lfrt dfrt lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix lcountry_growth dcountry_growth, cluster(ccode1) i(ccode1) ///
    fe vsquish noomit
    
    regsave using "tables/FRT3.dta", detail(all) replace table(Volatility_basic, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())
        
// FRT --- no Canada
xtreg dratecov lltratecov lfrt dfrt lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix lcountry_growth dcountry_growth if country != "Canada", ///
    cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/FRT4.dta", detail(all) replace table(Volatility_noca, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// FRT with country growth and Eurozone
xtreg dratecov lltratecov lfrt dfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix lcountry_growth dcountry_growth eurozone, cluster(ccode1) i(ccode1) ///
    fe vsquish noomit
    
    regsave using "tables/FRT5.dta", detail(all) replace ///
        table(Volatility_morevars, order(regvars r2) format(%5.2f) ///
        paren(stderr) asterisk())
        
// FRT on a log scale
/// without domestic growth and eurozone
xtreg dratecov lltratecov lfrt_log dfrt_log lpubdebtgdp ///
	dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
	dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
	
	regsave using "tables/log_FRT1.dta", detail(all) replace ///
        table(Volatility_log, order(regvars r2) format(%5.2f) ///
        paren(stderr) asterisk())
        
// with no Canada
xtreg dratecov lltratecov lfrt_log dfrt_log lpubdebtgdp ///
    dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix ///
    dvix if country != "Canada", cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/log_FRT2.dta", detail(all) replace ///
        table(Volatility_log_no_ca, order(regvars r2) format(%5.2f) ///
        paren(stderr) asterisk())


// HRV
xtreg dratecov lltratecov lhrv_mean dhrv_mean lstrucbalgdp dstrucbalgdp ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth ///
    doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
    
    regsave using "tables/HRV3.dta", detail(all) replace table(Volatility, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// OBI
xtreg dratecov lltratecov lobi_filled dobi_filled lstrucbalgdp dstrucbalgdp ///
    lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth ///
    doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
