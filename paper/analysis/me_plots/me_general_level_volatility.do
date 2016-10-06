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

use "analysis/frt08_16_v2.dta"

* Create interactions
gen l_frt2015xl_pub_gen = l_frt_2015 * l_pubdebtgdp_gen
gen d_frt_2015xd_pubdebtgdp_gen = d_frt_2015 * d_pubdebtgdp_gen

gen l_frt2015xd_pubdebtgdp_gen = l_frt_2015 * d_pubdebtgdp_gen
gen d_frt_2015xl_pub_gen = d_frt_2015 * l_pubdebtgdp_gen

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

/* 3. Run standard regression. */

xtreg d_lt_ratecov_fred l_lt_ratecov_fred l_frt_2015 d_frt_2015 ///
	l_pubdebtgdp_gen d_pubdebtgdp_gen ///
	l_frt2015xl_pub_gen d_frt_2015xd_pubdebtgdp_gen ///
	l_frt2015xd_pubdebtgdp_gen d_frt_2015xl_pub_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth l_pcgdp2005l d_pcgdp2005l ///
	l_cgdpgrowth d_cgdpgrowth l_oecdgrowth d_oecdgrowth l_us3mrate d_us3mrate ///
	l_vix d_vix ///
	l_uds d_uds imf_program_lag, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit
	 
/* 4. Gather parameters, initialize matrix for runs */
quietly summarize l_pubdebtgdp_gen if e(sample), detail /* Change x2 to variable that you want to appear on X-axis */
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
   gen l_pubdebtgdpa= l_pubdebtgdp_gen-`min'-(`inc'*`iter')  /* Alter these four lines to fit your model. */
   summarize l_pubdebtgdpa
   gen x1x2a= l_frt_2015 * l_pubdebtgdpa


xtreg d_lt_ratecov_fred l_frt_2015 l_pubdebtgdpa x1x2a ///
	l_bond_spread_fred d_frt_2015 d_pubdebtgdp_gen ///
	d_frt_2015xd_pubdebtgdp_gen ///
	l_frt2015xd_pubdebtgdp_gen d_frt_2015xl_pub_gen ///
	l_infl d_infl l_cgdpgrowth d_cgdpgrowth ///
	l_pcgdp2005l d_pcgdp2005l l_oecdgrowth d_oecdgrowth ///
	l_us3mrate d_us3mrate l_vix d_vix l_uds d_uds imf_program_lag, ///
	cluster(imf_code) i(imf_code) fe vsquish noomit

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
/* 7. Produce graph if l_pubdebtgdp_gen is continuous, or if l_pubdebtgdp_gen is ordinal but */
/* fractional values are not conceptually inconceivable (e.g. some */
/* continuous quantity for which only ordinal measures are available). */
/* Change titles, labels, etc., to fit your particular needs */
twoway (connect points1 points2 points3 points4, mcolor(navy maroon navy)/*
*/ clcolor(black maroon black) lpattern(dash solid dash) lwidth(medium medthick medium) msize(small small small) msym(i D i))/*
*/(histogram l_pubdebtgdp_gen if e(sample), bin(50) yaxis(2) blcolor(gray) bfcolor(none)), ytitle(Histogram of/*
*/ X-axis var, axis(2) size(3))/*
*/ ytitle("Coefficients and 95% CIs", size(4))/*
*/ xlab(`min' "Minimum" `cen50' "Median" `max' "Maximum") ylabel(, labsize(4)) yline(0, lwidth(medthick)) title("Coefficient on FRT", size(3)) xtitle("Public Debt/GDP (%)", size(3)) xscal(titlegap(2)) yscal(titlegap(2))/*
*/  xsca(titlegap(2)) yscal(titlegap(2))/*
*/  legend(off) scheme(s2mono) graphregion(color(white))

graph export paper_plots/me_level_frt_yields_cov.pdf, replace
