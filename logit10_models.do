// Declining Fertility in Taiwan: The Deterring Impact of Housework

// Jac Thomas - Department of Economics, National Tsing Hua University,
// Taiwan (R.O.C.); Department of Geography and Planning, University of
// Liverpool, UK

// Francisco Rowe - Department of Geography and Planning, 
// University of Liverpool, UK

// Eric Lin - Department of Economics, National Tsing Hua University,
// Taiwan (R.O.C.)

// Cross-sectional logit models - data analysis script

// 05/05/2020

cd "C:/Users/Jac/OneDrive/paper_1/submission_3/analysis_and_results"
use b1115b_dp.dta

	// Generate all interactions between w_wrkhrs nanb whw parentsb, and also w_wrkbXwhw
generate w_wrkhrsXnanb = w_wrkhrs*nanb
generate whwXnanb = whw*nanb
generate parentsbXnanb = parentsb*nanb
generate parentsbXw_wrkhrs = parentsb*w_wrkhrs
generate parentsbXwhw = parentsb*whw
generate w_wrkhrsXwhw = w_wrkhrs*whw
generate w_wrkbXwhw = w_wrkb*whw
	// Generate interactions between intentions and variables of interest
generate morebirbXratio = morebirb*ratio
generate morebirbXwhw = morebirb*whw
	// Generate genideo interactions
generate genideoXratio = genideo*ratio
generate genideoXwhw = genideo*whw
	// Generate log income and income / 1000
generate loginc = log(inc) 
generate inc_thou = inc / 1000
	// Generate inverse age of youngest child
generate cyoung10YOB_01 = cyoung10YOB
replace cyoung10YOB_01 = 0.1 if cyoung10YOB_01 == 0
generate cyoung10YOB_inv = 1 / cyoung10YOB_01
	// Generate binary for youngest child being zero
generate cyoung10YOB0b = 0
replace cyoung10YOB0b = 1 if cyoung10YOB > 0
	// Generate binary for youngest child being less than 5
generate cyng_agel5b = 0
replace cyng_agel5b = 1 if cyng_age < 5

// Candidate variables; check for correlations

correlate b11_15b cyoung10YOB weduH heduH ratio whw hhw wy_age w_wrkb ///
parentsb inc_thou loginc p2plusb nanb morebirb parentsbXnanb w_wrkbXwhw ///
morebirbXratio morebirbXwhw gender

	// 0.9161	loginc inc_thou
	// 0.8113	morebirb morebirbXratio

	// Omit inc_thou and morebirbXratio

quietly logit b11_15b cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw gender, vce(robust)
estimates store log1 // All
quietly logit b11_15b cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw if gender == 1, vce(robust)
estimates store log2 // Females
quietly logit b11_15b cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw if gender == 2, vce(robust)
estimates store log3 // Males

esttab log1 log2 log3, se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(N)

	// Births file output: 
quietly esttab log1 log2 log3 using logit10f10, ///
se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(N) csv title(Comparison of ///
models predicting births) mtitles("All" "Females" "Males") ///
coeflabels(cyng_agel5b "Youngest child younger than five" ///
weduH "Wife's tertiary education" heduH "Husband's tertiary education" ///
ratio "Proportion" whw "Wife's hours of housework" hhw "Husband's hours of housework" ///
wy_age "Wife's age" w_wrkb "Wife works" parentsb "Parents co-reside" ///
loginc "Log income" gender "Gender" p2plusb "Parity 2 or more" nanb "Pays for nanny" ///
ddum "Year of the Dragon" morebirb "Desires more children" parentsbXnanb "Parents co-reside * pays for nanny" ///
w_wrkbXwhw "Wife works * wife's hours of housework" ///
morebirbXwhw "Desires more children * wife's hours of housework") ///
replace

	// Summary statistics output:
quietly estpost summarize b11_15b, detail
esttab . using sumstats_b11_15bf10, cells("mean sd min p50 max") title(Summary statistics) nonumbers noobs replace
