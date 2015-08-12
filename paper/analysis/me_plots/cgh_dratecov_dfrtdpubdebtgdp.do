/* Batch file for creating graphs to illustrate the interactive effects of variables */
/* Bear F. Braumoeller, Harvard University, 11/27/04.  This file, if run as-is, will run */
/* a simulation to demonstrate how the graph works.  The annotations explain the file */
/* and point the user to the parts that need to be modified if it is to be used with */
/* real data.  Comments welcome. */

/* 1. Clean the slate. */
clear
set more off

use "/Data/frt0526.dta"

/* 3. Run standard regression. */

xtreg dratecov dfrt dpubdebtgdp dfrtxdpub lltratecov lfrt lpubdebtgdp lfrtxlpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l lcgdpgrowth dcgdpgrowth loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

/* 4. Gather parameters, initialize matrix for runs */
quietly summarize dpubdebtgdp if e(sample), detail /* Change x2 to variable that you want to appear on X-axis */
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
   gen dpubdebtgdpa=dpubdebtgdp-`min'-(`inc'*`iter')  /* Alter these four lines to fit your model. */
   summarize dpubdebtgdpa
   gen x1x2a=dfrt*dpubdebtgdpa
   
   
xtreg dratecov dfrt dpubdebtgdpa x1x2a lltratecov lfrt lpubdebtgdp lfrtxlpub linfl dinfl lcgdpgrowth dcgdpgrowth lpcgdp2005l dpcgdp2005l lcgdpgrowth dcgdpgrowth loecdgrowth doecdgrowth lus3mrate dus3mrate lvix dvix, cluster(ccode1) i(ccode1) fe vsquish noomit

matrix betas=e(b)                /* Stop alterations. */
   scalar dfrtcoef=betas[1,`order']
   matrix ses=e(V)
   scalar dfrtse=sqrt(ses[`order',`order'])
   local obs=e(N)                   /* Calculate 95% confidence intervals.  Assumes t-tests for signif.; */
   scalar ci95=invnorm(0.975) /* if using procedure that produces z-tests, use invnorm(0.975) */
   local xval = `min'+(`inc'*`iter')
   matrix foo = dfrtcoef-ci95*dfrtse, dfrtcoef, dfrtcoef+ci95*dfrtse, `xval'
   matrix foo2 = foo2 \ foo
   drop dpubdebtgdpa x1x2a
   local iter=`iter'+1
   }

/* 6. Convert matrix into data */
matrix points=foo2[2..(`numparams'+1),1..4]
svmat points

/* 7. Produce graph if dpubdebtgdp is continuous, or if dpubdebtgdp is ordinal but */
/* fractional values are not conceptually inconceivable (e.g. some */
/* continuous quantity for which only ordinal measures are available). */
/* Change titles, labels, etc., to fit your particular needs */
twoway (connect points1 points2 points3 points4, mcolor(navy maroon navy)/*
*/ clcolor(black maroon black) lpattern(dash solid dash) lwidth(medium medthick medium) msize(small small small) msym(i D i))/*
*/(histogram dpubdebtgdp if e(sample), bin(50) yaxis(2) blcolor(gray) bfcolor(none)), ytitle(Histogram of/*
*/ X-axis var, axis(2) size(3))/*
*/ ytitle("Coefficients and 95% CIs", size(4))/*
*/ xlab(-20 -10 0 10 20 30 40 50) ylabel(, labsize(4)) yline(0, lwidth(medthick)) title("Coefficient on Change in FRT", size(3)) xtitle("Change in Public Debt/GDP (%)", size(3)) xscal(titlegap(2)) yscal(titlegap(2))/*
*/  xsca(titlegap(2)) yscal(titlegap(2))/*
*/  legend(off) scheme(s2mono)

