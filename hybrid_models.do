// Declining Fertility in Taiwan: The Deterring Impact of Housework

// Jac Thomas - Department of Economics, National Tsing Hua University,
// Taiwan (R.O.C.); Department of Geography and Planning, University of
// Liverpool, UK

// Francisco Rowe - Department of Geography and Planning, 
// University of Liverpool, UK

// Eric Lin - Department of Economics, National Tsing Hua University,
// Taiwan (R.O.C.)

// Hybrid models - data analysis script

// 05/05/2020

cd "C:/Users/Jac/OneDrive/paper_1/submission_3/analysis_and_results"
use mast_w2014_dp_f2010
xtset x01 syear, yearly

	// Generate all interactions between w_wrkhrs nanb whw parentsb, and also w_wrkbXwhw
generate w_wrkhrsXnanb = w_wrkhrs*nanb
generate whwXnanb = whw*nanb
generate parentsbXnanb = parentsb*nanb
generate parentsbXw_wrkhrs = parentsb*w_wrkhrs
generate parentsbXwhw = parentsb*whw
generate w_wrkhrsXwhw = w_wrkhrs*whw
generate w_wrkbXwhw = w_wrkb*whw
	// Generate interactions between intentions and variables of interest
generate morebirbXratio = morebirb*ratio if syear > 2009
generate morebirbXwhw = morebirb*whw if syear > 2009
	// Generate genideo interactions
generate genideoXratio = genideo*ratio if syear > 2009
generate genideoXwhw = genideo*whw if syear > 2009
	// Generate log income and income / 1000
generate loginc = log(inc) 
generate inc_thou = inc / 1000
	// Generate inverse age of youngest child
generate cyng_age_01 = cyng_age
replace cyng_age_01 = 0.1 if cyng_age_01 == 0
generate cyng_age_inv = 1 / cyng_age_01
	// Generate binary for youngest child being zero
generate cyng_age0b = 0
replace cyng_age0b = 1 if cyng_age > 0
	// Generate binary for youngest child being less than 5
generate cyng_agel5b = 0
replace cyng_agel5b = 1 if cyng_age < 5

// Candidate variables; check for correlations

correlate birth cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb ///
parentsb inc_thou loginc gender p2plusb nanb ddum morebirb parentsbXnanb ///
w_wrkbXwhw morebirbXratio morebirbXwhw

	// 0.9200	loginc inc_thou
	// 0.8239	morebirb morebirbXratio
	// 0.7186	morebirb morebirbXwhw

	// Omit inc_thou and morebirbXratio

// Births

quietly xthybrid birth cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc gender p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid1 // All
quietly xthybrid birth cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw if gender == 1, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid2 // Only females
quietly xthybrid birth cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb  loginc p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw  morebirbXwhw if gender == 2, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid3 // Only males
	// No wife's hours of housework, no husband's hours of housework
quietly xthybrid birth cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc gender p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid4 // All
quietly xthybrid birth cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw if gender == 1, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid5 // Only females
quietly xthybrid birth cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc p2plusb nanb ddum morebirb parentsbXnanb w_wrkbXwhw morebirbXwhw if gender == 2, clusterid(x01) family(binomial) link(logit)
estimates store bhybrid6 // Only males

esttab bhybrid1 bhybrid2 bhybrid3 bhybrid4 bhybrid5 bhybrid6, ///
se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(N) title(Comparison of ///
models predicting births) mtitles("All" ///
"Females" "Males" "All" "Females" "Males")

// Desire for another child

quietly xthybrid morebirb cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc gender p2plusb nanb parentsbXnanb w_wrkbXwhw, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid1 // All
quietly xthybrid morebirb cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb parentsbXnanb w_wrkbXwhw if gender == 1, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid2 // Only females
quietly xthybrid morebirb cyng_agel5b weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb parentsbXnanb w_wrkbXwhw if gender == 2, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid3 // Only males
	// No wife's hours of housework, no husband's hours of housework
quietly xthybrid morebirb cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc gender p2plusb nanb parentsbXnanb w_wrkbXwhw, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid4 // All
quietly xthybrid morebirb cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc p2plusb nanb parentsbXnanb w_wrkbXwhw if gender == 1, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid5 // Only females
quietly xthybrid morebirb cyng_agel5b weduH heduH ratio wy_age w_wrkb parentsb loginc p2plusb nanb parentsbXnanb w_wrkbXwhw if gender == 2, clusterid(x01) family(binomial) link(logit)
estimates store dhybrid6 // Only males

esttab dhybrid1 dhybrid2 dhybrid3 dhybrid4 dhybrid5 dhybrid6, ///
se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) title(Comparison of ///
models predicting desire for more births) mtitles("All" ///
"Females" "Males" "All" "Females" "Males")

	// Hybrid file output:
esttab bhybrid1 bhybrid2 bhybrid3 dhybrid1 dhybrid2 dhybrid3 ///
using hybrid, csv ///
se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(N) title(Comparison of ///
models predicting births and desire for more births) mtitles("All" ///
"Females" "Males" "All" "Females" "Males") ///
coeflabels(W__cyng_agel5b "Youngest child younger than five" ///
W__weduH "Wife's tertiary education" W__heduH "Husband's tertiary education" ///
W__ratio "Proportion" W__whw "Wife's hours of housework" W__hhw "Husband's hours of housework" ///
W__wy_age "Wife's age" W__w_wrkb "Wife works" W__parentsb "Parents co-reside" ///
W__loginc "Log income" R__gender "Gender" W__p2plusb "Parity 2 or more" W__nanb "Pays for nanny" ///
W__ddum "Year of the Dragon" W__morebirb "Desires more children" W__parentsbXnanb "Parents co-reside * pays for nanny" ///
W__w_wrkbXwhw "Wife works * wife's hours of housework" W__morebirbXratio ///
"Desires more children * proportion" W__morebirbXwhw "Desires more children * wife's hours of housework" ///
R__weduH "Wife's tertiary education" B__cyng_agel5b "Youngest child younger than five" ///
B__weduH "Wife's tertiary education" B__heduH "Husband's tertiary education" ///
B__ratio "Proportion" B__whw "Wife's hours of housework" B__hhw "Husband's hours of housework" ///
B__wy_age "Wife's age" B__w_wrkb "Wife works" B__parentsb "Parents co-reside" B__inc_thou "Income" ///
B__loginc "Log income" B__p2plusb "Parity 2 or more" B__nanb "Pays for nanny" ///
B__ddum "Year of the Dragon" B__morebirb "Desires more children" B__parentsbXnanb "Parents co-reside * pays for nanny" ///
B__w_wrkbXwhw "Wife works * wife's hours of housework" B__morebirbXwhw "Desires more children * wife's hours of housework" ///
_cons "Constant") ///
replace

	// Summary statistics output:
quietly estpost summarize birth cyng_agel5b morebirb weduH heduH ratio whw hhw wy_age w_wrkb parentsb loginc p2plusb nanb ddum, detail
esttab . using sumstats, csv cells("mean sd min p50 max") title(Summary statistics) nonumbers noobs replace
