/* Batch file for creating graphs to illustrate the interactive effects of variables */
/* Bear F. Braumoeller, Harvard University, 11/27/04.  This file, if run as-is, will run */
/* a simulation to demonstrate how the graph works.  The annotations explain the file */
/* and point the user to the parts that need to be modified if it is to be used with */
/* real data.  Comments welcome. */

/* 1. Clean the slate. */
clear
set more off

// Set working directory, change as needed/
cd "/git_repositories/FRTIndex/paper/"

use "analysis/frt04_16_v1.dta"
drop if country == "Japan"

/* 3. Run standard regression. */

xtreg d_bond_spread_fred d_frt d_pubdebtgdp d_frtxd_pub l_bond_spread_fred l_frt l_pubdebtgdp l_frtxl_pub l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_cgdpgrowth d_cgdpgrowth l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit

/* 4. Gather parameters, initialize matrix for runs */
quietly summarize l_pubdebtgdp if e(sample), detail /* Change x2 to variable that you want to appear on X-axis */
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
   gen l_pubdebtgdpa=l_pubdebtgdp-`min'-(`inc'*`iter')  /* Alter these four lines to fit your model. */
   summarize l_pubdebtgdpa
   gen x1x2a=l_frt*l_pubdebtgdpa


xtreg d_bond_spread_fred l_frt l_pubdebtgdpa x1x2a l_bond_spread_fred d_frt d_pubdebtgdp d_frtxd_pub l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l l_cgdpgrowth d_cgdpgrowth l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate l_vix d_vix if country != "United States", cluster(imf_code) i(imf_code) fe vsquish noomit

matrix betas=e(b)                /* Stop alterations. */
   scalar l_frtcoef=betas[1,`order']
   matrix ses=e(V)
   scalar l_frtse=sqrt(ses[`order',`order'])
   local obs=e(N)                   /* Calculate 95% confidence intervals.  Assumes t-tests for signif.; */
   scalar ci95=invnorm(0.975) /* if using procedure that produces z-tests, use invnorm(0.975) */
   local xval = `min'+(`inc'*`iter')
   matrix foo = l_frtcoef-ci95*l_frtse, l_frtcoef, l_frtcoef+ci95*l_frtse, `xval'
   matrix foo2 = foo2 \ foo
   drop l_pubdebtgdpa x1x2a
   local iter=`iter'+1
   }

/* 6. Convert matrix into data */
matrix points=foo2[2..(`numparams'+1),1..4]
svmat points

/* 7. Produce graph if l_pubdebtgdp is continuous, or if l_pubdebtgdp is ordinal but */
/* fractional values are not conceptually inconceivable (e.g. some */
/* continuous quantity for which only ordinal measures are available). */
/* Change titles, labels, etc., to fit your particular needs */
twoway (connect points1 points2 points3 points4, mcolor(navy maroon navy)/*
*/ clcolor(black maroon black) lpattern(dash solid dash) lwidth(medium medthick medium) msize(small small small) msym(i D i))/*
*/(histogram l_pubdebtgdp if e(sample), bin(50) yaxis(2) blcolor(gray) bfcolor(none)), ytitle(Histogram of/*
*/ X-axis var, axis(2) size(3))/*
*/ ytitle("Coefficients and 95% CIs", size(4))/*
*/ xlab(0 25 50 75 100 125 150 175 200 225) ylabel(, labsize(4)) yline(0, lwidth(medthick)) xtitle("Public Debt/GDP (%)", size(3)) xscal(titlegap(2)) yscal(titlegap(2))/*
*/  xsca(titlegap(2)) yscal(titlegap(2))/*
*/  legend(off) scheme(s2mono) graphregion(color(white))

graph export paper_plots/me_level_frt_spreads.pdf, replace
