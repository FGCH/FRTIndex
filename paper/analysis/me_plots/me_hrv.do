///////////////////////
// Marginal effects for HRV levels on bond spreads
// Christopher Gandrud
// MIT License
///////////////////////

clear

set more off

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/"

// Load data
insheet using "source/Hollyer_et_al_Compare/frt_hrv_bond.csv", comma

// Create interactions
gen lhrvxlpub = lhrv_mean * lpubdebtgdp
gen dhrvxdpub = dhrv_mean * dpubdebtgdp


/* 3. Run standard regression. */

xtreg dnewspread lhrv_mean lpubdebtgdp lhrvxlpub lnewspread  dhrv_mean dpubdebtgdp dhrvxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit

/* 4. Gather parameters, initialize matrix for runs */
quietly summarize lpubdebtgdp if e(sample), detail /* Change x2 to variable that you want to appear on X-axis */
local min=r(min)
local max=r(max)
local cen10=r(p10)
local cen20=r(p20)
local cen30=r(p30)
local cen40=r(p40)
local cen50=r(p50)
local cen60=r(p60)
local cen70=r(p70)
local cen80=r(p80)
local cen90=r(p90)
local numparams=11  /*Set number of coefficients to be graphed HERE */
local inc=(`max'-`min')/(`numparams'-1)
local iter=0
matrix foo2 = 0,0,0,0
local order=1  /* Replace this number with a number that will tell Stata which IV's coefs. you */
               /* want to graph -- first, second, third, etc.  In this example, we want to graph */
               /* x1's coefficients as a function of x2, and x1 is the first independent variable */
               /* listed in the regress command, so we would enter a 1. */

/* 5. Calculate coefficients for x1 across range of x2, store in matrix foo2 */
while `iter'<`numparams' {
   gen lpubdebtgdpa=lpubdebtgdp-`min'-(`inc'*`iter')  /* Alter these four lines to fit your model. */
   summarize lpubdebtgdpa
   gen x1x2a=lhrv_mean*lpubdebtgdpa

xtreg dnewspread lhrv_mean dhrv_mean x1x2a lnewspread lpubdebtgdp dpubdebtgdp dhrvxdpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix if country != "United States", cluster(ccode1) i(ccode1) fe vsquish noomit

matrix betas=e(b)                /* Stop alterations. */
   scalar lfrtcoef=betas[1,`order']
   matrix ses=e(V)
   scalar lfrtse=sqrt(ses[`order',`order'])
   local obs=e(N)                   /* Calculate 95% confidence intervals.  Assumes t-tests for signif.; */
   scalar ci95=invnorm(0.975) /* if using procedure that produces z-tests, use invnorm(0.975) */
   local xval = `min'+(`inc'*`iter')
   matrix foo = lfrtcoef-ci95*lfrtse, lfrtcoef, lfrtcoef+ci95*lfrtse, `xval'
   matrix foo2 = foo2 \ foo
   drop lpubdebtgdpa x1x2a
   local iter=`iter'+1
   }

/* 6. Convert matrix into data */
matrix points=foo2[2..(`numparams'+1),1..4]
svmat points

/* 7. Produce graph if lpubdebtgdp is continuous, or if lpubdebtgdp is ordinal but */
/* fractional values are not conceptually inconceivable (e.g. some */
/* continuous quantity for which only ordinal measures are available). */
/* Change titles, labels, etc., to fit your particular needs */
twoway (connect points1 points2 points3 points4, mcolor(navy maroon navy)/*
*/ clcolor(black maroon black) lpattern(dash solid dash) lwidth(medium medthick medium) msize(small small small) msym(i D i))/*
*/(histogram lpubdebtgdp if e(sample), bin(50) yaxis(2) blcolor(gray) bfcolor(none)), ytitle(Histogram of/*
*/ X-axis var, axis(2) size(3))/*
*/ ytitle("Coefficients and 95% CIs", size(4))/*
*/ xlab(0 25 50 75 100 125 150 175 200 225) ylabel(, labsize(4)) yline(0, lwidth(medthick)) xtitle("Public Debt/GDP (%)", size(3)) xscal(titlegap(2)) yscal(titlegap(2))/*
*/  xsca(titlegap(2)) yscal(titlegap(2))/*
*/  legend(off) scheme(s2mono) graphregion(color(white))

graph export paper/paper_plots/me_levelhrv_mean_spreads.pdf, replace
