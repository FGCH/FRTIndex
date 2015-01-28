///////////////////////
// Compare HRV and FRT affect on sovereign bonds
// Christopher Gandrud
// 28 January 2015
// MIT License
///////////////////////

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/'

// Load data
use "frt_hrv_bond.dta"

//////// Examine Change in long-term rate
// FRT
xtreg dltrate lltrate dfrt lfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dltrate lltrate dhrv_mean lhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

//////// Examine Change in long-term rate spread
//FRT
xtreg dltspreadus lltspreadus dfrt lfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dltspreadus lltspreadus dhrv_mean lhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix if ccode1~=2, cluster(ccode1) i(ccode1) fe vsquish noomit


//////// Examine Spread volatility
// FRT
xtreg dratecov lltratecov dfrt lfrt lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

// HRV
xtreg dratecov lltratecov dhrv_mean lhrv_mean lstrucbalgdp dstrucbalgdp lpubdebtgdp dpubdebtgdp linfl dinfl lus3mrate dus3mrate loecdgrowth doecdgrowth lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit
