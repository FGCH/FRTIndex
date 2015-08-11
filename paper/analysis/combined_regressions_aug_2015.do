/*
Debt regressions
*/

/* 1. Clean the slate. */
clear
set more off

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/paper/"

insheet using "analysis/frt0526.csv", comma

/* ECM Models */

/* Non-Interactive Models */

* Spreads

xtreg dnewspread lnewspread dfrt lfrt lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

regsave using "tables/FRT_noninteract_1.dta", detail(all) replace table(nonInteractSpread, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())

* Coefficient of variation
xtreg dratecov lltratecov dfrt lfrt lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

regsave using "tables/FRT_noninteract_2.dta", detail(all) replace table(nonInteractCov, ///
        order(regvars r2) format(%5.2f) paren(stderr) asterisk())


/* Interactive Models */

* Spreads
xtreg dnewspread lnewspread dfrt dpubdebtgdp dfrtxdpub lfrt lpubdebtgdp lfrtxlpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

xtreg dnewspread lnewspread lfrt lpubdebtgdp lfrtxlpub dfrt dpubdebtgdp dfrtxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

xtreg dnewspread lnewspread lfrt lpubdebtgdp lfrtxlpub dfrt dpubdebtgdp dfrtxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

* Coefficient of variation
xtreg dratecov lltratecov dfrt dpubdebtgdp dfrtxdpub lfrt lpubdebtgdp lfrtxlpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

xtreg dratecov lltratecov lfrt lpubdebtgdp lfrtxlpub dfrt dpubdebtgdp dfrtxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
