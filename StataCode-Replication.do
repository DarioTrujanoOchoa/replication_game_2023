/**************************
Replication of Fig 2B 
***************************/
use data/data-bsr-qsr.dta, clear
keep if scoring == 1 & treatment==1

//Define program for creating Fig 2B easily.
program drop fig2b
program define fig2b 
args i
preserve
	collapse (mean) mean=false ///
			(semean) se=false ///
			(count) n=false, by(pur treatment)
			
	gen ci_up = mean + invttail(n - 1, 0.025)*se
	gen ci_lo = mean - invttail(n - 1, 0.025)*se
	
	gen category = 1 if pur==20
	replace category = 2 if pur==30
	replace category = 3 if pur==50
	replace category = 4 if pur==70
	replace category = 5 if pur==80

	#delimit ;
    graph twoway (bar mean category,  lwidth(0.1) lcolor(white) fcolor(gray) 
					mlabel(mean) mlabformat(%9.3g) mlabcolor(black) mlabposition(1))
				(rcap ci_up ci_lo category, lcolor(black) )
				, subtitle("False = more than `i' pp away", ring(0))
				ytitle("Fraction of false reports", ) 
				xtitle("Prior", margin(small)  )
				ylabel(0(0.1).8,  angle(horizontal) format(%2.1f) nogrid)
				xlabel(1 "20" 2 "30" 3 "50" 4 "70" 5 "80" , nogrid )
				legend(off)
				xsize(4) ysize(4)
				graphregion(color(white) margin(zero))
				plotregion(margin(bargraph)) ;
	#delimit cr
	graph export "2b_`i'pp.pdf", as(pdf) replace			
    restore
end

// Replication of Original Fig 2B
gen false = (belief1 != pur)
fig2b "0"

//Redefine False to +/- 1%
replace false = (abs(belief1-pur)>1)
fig2b "1"

//Redefine False to +/- 2%
replace false = (abs(belief1-pur)>2)
fig2b "2"

//Redefine False to +/- 3%
replace false = (abs(belief1-pur)>3)
fig2b "3"

//Redefine False to +/- 4%
replace false = (abs(belief1-pur)>4)
fig2b "4"

//Redefine False to +/- 5%
replace false = (abs(belief1-pur)>5)
fig2b "5"

//Redefine False to +/- 10%
replace false = (abs(belief1-pur)>10)
fig2b "10"


// Make Fig2b but with distance on the y-axis
use data/data-bsr-qsr.dta, clear
keep if scoring == 1 & treatment==1
gen dist = abs(belief1 - pur)

preserve
	collapse (mean) mean=dist ///
			(semean) se=dist ///
			(count) n=dist, by(pur treatment)
			
	gen ci_up = mean + invttail(n - 1, 0.025)*se
	gen ci_lo = mean - invttail(n - 1, 0.025)*se
	
	gen category = 1 if pur==20
	replace category = 2 if pur==30
	replace category = 3 if pur==50
	replace category = 4 if pur==70
	replace category = 5 if pur==80
	
	#delimit ;
    graph twoway (bar mean category,  lwidth(0.1) lcolor(white) fcolor(gray))
				(rcap ci_up ci_lo category, lcolor(black) )
				, //subtitle("Size of deviation from prior")
				ytitle("|Report - Prior|", ) 
				xtitle("Prior", margin(small)  )
				ylabel(,  angle(horizontal)  nogrid)
				xlabel(1 "20%" 2 "30%" 3 "50%" 4 "70%" 5 "80%" , nogrid noticks)
				legend(off)
				xsize(4) ysize(4)
				graphregion(color(white) margin(zero))
				plotregion(margin(bargraph)) ;
	#delimit cr
	graph export "2b_size.pdf", as(pdf) replace			
restore





/**************************
Replication of Fig 4B 
***************************/

//Figure 4b with relaxed def of false
use data/data-bsr-qsr.dta, clear
gen false = (abs(belief1-pur)>5)

preserve
    keep if scoring == 1
    keep id treatment subjectid period pur false
    // Get averages per subject and prior:
    collapse treatment false, by(subjectid pur)
    // Generate consecutive numbers on x-axis for each prior and treatment:
    generate catvar = .
    local val = 0
    levelsof pur, local(levelsofvar)
    foreach p of local levelsofvar {
        local val = `val'+1
        replace catvar = `val' +  0 if pur == `p' & treat == 1
        replace catvar = `val' +  6 if pur == `p' & treat == 2
        replace catvar = `val' + 12 if pur == `p' & treat == 3
    }
    // Get averages per prior in each treatment and confidence intervals:
    collapse (mean) catvar meanb=false (sd) sdb=false (count) n=false, by(treatment pur)
    generate hib = meanb + invttail(n-1,0.025)*(sdb / sqrt(n))
    generate lob = meanb - invttail(n-1,0.025)*(sdb / sqrt(n))
    #delimit ;
        graph twoway
        (bar meanb catvar, lwidth(0.1) lcolor(white) fcolor(gs10))
    || (rcap hib lob catvar, lcolor(black) msize(small) lwidth(default)),
        ytitle("Fraction of false reports" "{subscript:(false = more than 5 pp away)}", margin(zero) size(medlarge)) 
        xtitle("Prior", margin(small) size(medlarge))
        xlabel(2.5 " " , noticks labsize(medlarge) nogrid)
        ylabel(0(0.1)0.8, glwidth(0.1) glcolor("230 230 230") glpattern(solid) angle(horizontal) format(%2.1f) labsize(medlarge))
        graphregion(color("255 255 255") margin(zero))
        legend(off)
        plotregion( m(b=0) )
        text(0.02  1 "0.2", placement(n) orientation(horizontal) color(black)) 
        text(0.02  2 "0.3", placement(n) orientation(horizontal) color(black)) 
        text(0.02  3 "0.5", placement(n) orientation(horizontal) color(black)) 
        text(0.02  4 "0.7", placement(n) orientation(horizontal) color(black)) 
        text(0.02  5 "0.8", placement(n) orientation(horizontal) color(black)) 
        text(0.02  7 "0.2", placement(n) orientation(horizontal) color(black)) 
        text(0.02  8 "0.3", placement(n) orientation(horizontal) color(black)) 
        text(0.02  9 "0.5", placement(n) orientation(horizontal) color(black)) 
        text(0.02 10 "0.7", placement(n) orientation(horizontal) color(black)) 
        text(0.02 11 "0.8", placement(n) orientation(horizontal) color(black)) 
        text(0.02 13 "0.2", placement(n) orientation(horizontal) color(black)) 
        text(0.02 14 "0.3", placement(n) orientation(horizontal) color(black)) 
        text(0.02 15 "0.5", placement(n) orientation(horizontal) color(black)) 
        text(0.02 16 "0.7", placement(n) orientation(horizontal) color(black)) 
        text(0.02 17 "0.8", placement(n) orientation(horizontal) color(black)) 
        text(-0.045 3 "Information",     placement(n) orientation(horizontal) color(black)) 
        text(-0.045 9 "RCL Calculator",  placement(n) orientation(horizontal) color(black)) 
        text(-0.045 15 "No Information", placement(n) orientation(horizontal) color(black)) 
        ;
    #delimit cr
    graph export "4_B.pdf", replace as(pdf)
restore




/********************************
Fig 6 with relaxed def of false
********************************/

//Fig 6a
use data/data-bsr-qsr.dta, clear

keep if scoring == 1
gen false = (abs(belief1-pur)>5)

keep id treatment subjectid period false
// Get period averages per treatment:
collapse false, by(treat period)
#delimit ;
graph twoway 
	scatter false period if treat == 1, connect(direct) msymbol(T)  msize(medsmall) lpattern(solid)     color(gs10)
|| scatter false period if treat == 3, connect(direct) msymbol(Oh) msize(medsmall) lpattern(shortdash) color(gs10)
|| scatter false period if treat == 4, connect(direct) msymbol(Oh) msize(medsmall) lpattern(solid)     color(black)
	,
	aspectratio(1)
	ysize(6) 
	xsize(6)
	ytitle("Fraction of false reports" "{subscript:(false = more than 5 pp away)}", margin(zero) size(medlarge)) 
	xtitle("Period", margin(small) size(medlarge)) 
	xlabel(#10, labsize(medlarge) tlwidth(vthin) nogrid )
	ylabel(0(.1)0.6,  labsize(medlarge) glwidth(0.2) glcolor("230 230 230") glpattern(solid) angle(horizontal) format(%2.1f))	
	graphregion(color(white) margin(zero))
	legend( rows(3) colfirst lab(2 "No Information") lab(3 "Feedback")  lab(1 "Information") 
	region(fcolor(none) lcolor(black))  size(small)  position(6) ring(0) order(1 2 3)  )
	graphregion(color(white) margin(small))
	plotregion(margin(small))
	;
#delimit cr

graph export "Fig6a.pdf",  as(pdf) replace


// Fig 6b
use data/data-bsr-qsr.dta, clear

gen false = (abs(belief1-pur)>5)

// Select prior reports from BSR Feedback treatment:
keep if scoring == 1 & treat == 4
keep id treatment subjectid period pur false
// Generate indicator for last two periods versus first two periods:
generate double half2 = .
replace half2 = 0 if period <= 2
replace half2 = 1 if period >= 9
// Get averages per subject, prior, and first/last two periods:
collapse false, by(subjectid pur half2)
// Generate consecutive numbers on x-axis for each prior and first/last two periods:
generate catvar = .
local val = 0
levelsof pur, local(levelsofvar)
foreach p of local levelsofvar {
	local val = `val'+1
	replace catvar = `val' +  0 if pur == `p' & half2 == 0
	replace catvar = `val' +  6 if pur == `p' & half2 == 1
}
// Get averages per prior and first/last two periods:
collapse (mean) catvar meanb=false, by(pur half2)
#delimit ;
graph twoway (bar meanb catvar, lwidth(0.1) lcolor(white) fcolor(gs10)),
	aspectratio(1)
	ysize(5) 
	xsize(5)
	ytitle("Fraction of false reports" "{subscript:(false = more than 5 pp away)}", margin(zero) size(medlarge)) 
	xtitle("Prior", margin(small) size(medlarge))
	xlabel(2.5 " " , noticks labsize(medlarge) nogrid)
	ylabel(0(0.1)0.6,  glcolor("230 230 230") glpattern(solid) angle(horizontal) format(%2.1f) labsize(medlarge))
	graphregion(color(white) margin(zero))
	legend(off)
	plotregion( m(b=0) )
	text(0.02  1 "0.2", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  2 "0.3", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  3 "0.5", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  4 "0.7", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  5 "0.8", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  7 "0.2", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  8 "0.3", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02  9 "0.5", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02 10 "0.7", placement(n) orientation(horizontal) color(black) size(small)) 
	text(0.02 11 "0.8", placement(n) orientation(horizontal) color(black) size(small)) 
	text(-0.05 3 "Periods 1 and 2",  placement(n) orientation(horizontal) color(black) size(medsmall) ) 
	text(-0.05 9 "Periods 9 and 10", placement(n) orientation(horizontal) color(black) size(medsmall) ) 
	;
#delimit cr

graph export "Fig6b.pdf",  as(pdf) replace



/************************************************************
Other (not used in replication study)
*************************************************************/
use data/data-bsr-qsr.dta, clear

preserve
	global x belief1 
	
	collapse (mean) mean=$x ///
			(semean) se=$x ///
			(count) n=$x, by(pur treatment)
			
	gen ci_up = mean + invttail(n - 1, 0.025)*se
	gen ci_lo = mean - invttail(n - 1, 0.025)*se

twoway (connected mean pur if treatment==1, lcolor(blue) mcolor(blue)) ///
		(rcap ci_up ci_lo pur if treatment==1, lcolor(blue)) ///
		/// (connected mean pur if treatment==2) ///
		/// (connected mean pur if treatment==3) ///
		(scatteri 20 20 80 80, recast(line) lcolor(black%50) lpattern(dash)) ///
		, ylabel(20(10)80) ///
		xlabel(20(10)80) ///
		legend(off)
restore

// box plot
graph box belief1 if treatment==1, over(pur) marker(1, msize(vsmall))

//histogram
foreach i in 20 30 70 80 {
	histogram belief1 if treatment==1 & pur==`i' ///
	, discrete fraction lwidth(none) ///
	ytitle("Fraction of reports") ///
	ylabel(0(.1).6, labsize(small)) ///
	xtitle("Reported belief (%)") ///
	xlabel(0(10)100, labsize(small) angle(forty_five)) ///
	subtitle("Prior = `i'%", ring(0)) ///
	xsize(5) ysize(4) scale(1.5)
	graph export "histogram_`i'.pdf", as(pdf) replace
}
	

