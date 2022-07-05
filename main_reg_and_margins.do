///Structure of the script:
///Baseline specification for bottom 50 share, margins for bottom 50 share, supplementary information margins for bottom 50 share
///Baseline specification for pc GDP growth and pc agricultural GDP growth, margins for pc GDP growth and pc agricultural GDP growth, 
///supplementary information margins for pc GDP growth
/// all data produced with margins are also available in stata_data folder


clear all
set mem 1G
set matsize 800
//set maxvar 10000
set more off

cd "/Users/elisapalagi/Documents/replication/replication_clim_ineq"


//DJO SAF //Baseline specification for bottom 50 shares
import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq/data/data_djo_ssaf.csv"

destring growthwdi, gen(growthWDI) force
destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force
destring agrgdpgrowthcap, gen(agr_gdpgrowthcap) force
destring avg_agri_emp, gen(avg_agriemp) force
destring agri_emp, gen(agriemp) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen b80_growth = agri2_b80*growthWDI
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2
gen t20_growth = agri2_top20*growthWDI

gen emp_temp =avg_agriemp*temp
gen emp_temp2=avg_agriemp*temp_2
gen emp_prec=avg_agriemp*prec
gen emp_prec2=avg_agriemp*prec_2


gen sq_emp_temp =((avg_agriemp)^2)*temp
gen sq_emp_temp2=((avg_agriemp)^2)*temp_2
gen sq_emp_prec=((avg_agriemp)^2)*prec
gen sq_emp_prec2=((avg_agriemp)^2)*prec_2

gen emp_temp_c =agriemp*temp
gen emp_temp2_c=agriemp*temp_2
gen emp_prec_c=agriemp*prec
gen emp_prec2_c=agriemp*prec_2


gen sq_emp_temp_c =((agriemp)^2)*temp
gen sq_emp_temp2_c=((agriemp)^2)*temp_2
gen sq_emp_prec_c=((agriemp)^2)*prec
gen sq_emp_prec2_c=((agriemp)^2)*prec_2

gen agriemp_sq=agriemp^2
encode continent, gen(continent_2)
encode continent_ssaf, gen(continent_ssaf2)

drop y_1990_mena



eststo clear

eststo: reg bottom50 temp temp_2 prec prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg bottom50 temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg bottom50 temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 y_*, cluster(iso2) 
eststo: reg bottom50 temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 y_*, cluster(iso2) 
eststo: reg bottom50 temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.iso2 y_*, cluster(iso2) 
esttab using b50_djo_saf.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

//gdp djo saf //robustness gdp
eststo: reg growthWDI temp temp_2 prec prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 y_*, cluster(iso2) 
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 y_*, cluster(iso2) 
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.iso2 y_*, cluster(iso2) 
esttab using gdp_djo_saf.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

//agrgdp djo saf //robustness agri gdp
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg agr_gdpgrowthcap  temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 y_*, cluster(iso2) 
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 y_*, cluster(iso2) 
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.iso2 y_*, cluster(iso2) 
esttab using agrgdp_djo_saf.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear


//Seemingly Unrelated Regression (SUR), SI Appendix, Table S6 //Uncomment to replicate
//set matsize 800
//eststo clear
//no dummies
//eststo: gsem (growthWDI <- temp temp_2 prec prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2) (agr_gdpgrowthcap  <- temp temp_2 prec prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2)  (bottom50 <- temp temp_2 prec prec_2 i.iso2 y_*), cov(e.growthWDI*e.bottom50) cov(e.growthWDI*e.agr_gdpgrowthcap) cov(e.bottom50*e.agr_gdpgrowthcap) vce(cluster iso2) 

//top20 as benchmark
//eststo: gsem (growthWDI <- temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2) (agr_gdpgrowthcap  <- temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2)  (bottom50 <- temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*), cov(e.growthWDI*e.bottom50) cov(e.growthWDI*e.agr_gdpgrowthcap) cov(e.bottom50*e.agr_gdpgrowthcap) vce(cluster iso2) 

//bottom80 as benchmark
//eststo: gsem (growthWDI <- temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2) (agr_gdpgrowthcap  <- temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 i.year c.time_trend##i.continent_ssaf2)  (bottom50 <- temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 y_*), cov(e.growthWDI*e.bottom50) cov(e.growthWDI*e.agr_gdpgrowthcap) cov(e.bottom50*e.agr_gdpgrowthcap) vce(cluster iso2) 

//continuous agri employment every year
//eststo: gsem (growthWDI <- temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 i.year c.time_trend##i.continent_ssaf2) (agr_gdpgrowthcap  <- temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 i.year c.time_trend##i.continent_ssaf2)  (bottom50 <- temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.iso2 y_*), cov(e.growthWDI*e.bottom50) cov(e.growthWDI*e.agr_gdpgrowthcap) cov(e.bottom50*e.agr_gdpgrowthcap) vce(cluster iso2) 


//uncomment for exporting table
//esttab using d_gsem_mix_saf.tex, keep(temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c) star(* 0.10 ** 0.05 *** 0.01)  replace

//eststo clear


/////// MARGINS ///////

///Bottom 50 

//HIGH AGRI (TOP 20)
reg bottom50 temp temp_2 c.prec##c.prec b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_b50_top20_djossaf.csv, comma replace

//or
//LOW AGRI (BOTTOM 80)
reg bottom50 temp temp_2 c.prec##c.prec t20_temp t20_temp_2 t20_prec t20_prec_2 i.iso2 y_*, cluster(iso2) 
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_b50_bottom80_djossaf.csv, comma replace






//// Margins for Supplementary Information 

//if previous margins are run, re-run lines 16-68 to import dataset
///marginal effects, changing top group ///
//top20
reg bottom50 temp temp_2 c.prec##c.prec b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top20_djossaf.csv, comma replace  

//or top 25
destring agri_b75, gen(agri2_b75) force
gen b75_temp= agri2_b75*temp 
gen b75_temp_2 = agri2_b75*temp_2 
gen b75_prec = agri2_b75*prec
gen b75_prec_2 = agri2_b75*prec_2


reg bottom50 temp temp_2 c.prec##c.prec b75_temp b75_temp_2 b75_prec b75_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top25_djossaf.csv, comma replace  


//or top 10
destring agri_b90, gen(agri2_b90) force
gen b90_temp= agri2_b90*temp 
gen b90_temp_2 = agri2_b90*temp_2 
gen b90_prec = agri2_b90*prec
gen b90_prec_2 = agri2_b90*prec_2


reg bottom50 temp temp_2 c.prec##c.prec b90_temp b90_temp_2 b90_prec b90_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top10_djossaf.csv, comma replace  




//////////////////////////////////////////////////
//// margins no top 10
/// Now top 20 is top 10, top 25 is top 15, top 30 is top 20.


clear all
set mem 1G
set matsize 800
//set maxvar 10000
set more off

cd "/Users/elisapalagi/Documents/replication/replication_clim_ineq"

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq/data/data_no_top10_djosaf.csv"

destring growthwdi, gen(growthWDI) force
destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force
destring agrgdpgrowthcap, gen(agr_gdpgrowthcap) force
destring avg_agri_emp, gen(avg_agriemp) force
destring agri_emp, gen(agriemp) force

gen b80_temp= agri_b80*temp 
gen b80_temp_2 = agri_b80*temp_2 
gen b80_prec = agri_b80*prec
gen b80_prec_2 = agri_b80*prec_2
gen b75_temp= agri_b75*temp 
gen b75_temp_2 = agri_b75*temp_2 
gen b75_prec = agri_b75*prec
gen b75_prec_2 = agri_b75*prec_2
gen b70_temp= agri_b70*temp 
gen b70_temp_2 = agri_b70*temp_2 
gen b70_prec = agri_b70*prec
gen b70_prec_2 = agri_b70*prec_2


//top10
reg bottom50 temp temp_2 c.prec##c.prec b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top10_notop10_djossaf.csv, comma replace  

//or top 15


reg bottom50 temp temp_2 c.prec##c.prec b75_temp b75_temp_2 b75_prec b75_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top15_notop10_djossaf.csv, comma replace  



//or top 20


reg bottom50 temp temp_2 c.prec##c.prec b70_temp b70_temp_2 b70_prec b70_prec_2 i.iso2 y_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, dydx(prec) at(prec=(`min'(0.1)4)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using mg_b50_top20_notop10_djossaf.csv, comma replace 





//////////////////////////////////////
/// Baseline specifications for GDP////

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq/data/data_macroreg_and_djo.csv", clear

destring growthwdi, gen(growthWDI) force
destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force
destring agrgdpgrowthcap, gen(agr_gdpgrowthcap) force
destring avg_agri_emp, gen(avg_agriemp) force
destring agri_emp, gen(agriemp) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen b80_growth = agri2_b80*growthWDI
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2
gen t20_growth = agri2_top20*growthWDI

gen emp_temp =avg_agriemp*temp
gen emp_temp2=avg_agriemp*temp_2
gen emp_prec=avg_agriemp*prec
gen emp_prec2=avg_agriemp*prec_2


gen sq_emp_temp =((avg_agriemp)^2)*temp
gen sq_emp_temp2=((avg_agriemp)^2)*temp_2
gen sq_emp_prec=((avg_agriemp)^2)*prec
gen sq_emp_prec2=((avg_agriemp)^2)*prec_2

gen emp_temp_c =agriemp*temp
gen emp_temp2_c=agriemp*temp_2
gen emp_prec_c=agriemp*prec
gen emp_prec2_c=agriemp*prec_2


gen sq_emp_temp_c =((agriemp)^2)*temp
gen sq_emp_temp2_c=((agriemp)^2)*temp_2
gen sq_emp_prec_c=((agriemp)^2)*prec
gen sq_emp_prec2_c=((agriemp)^2)*prec_2

gen agriemp_sq=agriemp^2
encode continent, gen(continent_2)

drop t_saf
drop y_1990_mena
//encode continent_ssaf, gen(continent_ssaf2)


eststo clear


//macroreg flex trends
//gdp

eststo: reg growthWDI temp temp_2 prec prec_2 i.year i.iso2 t_*, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 t_*, cluster(iso2)
eststo: reg growthWDI  temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 t_*, cluster(iso2) 
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 

esttab using gdp_macroreg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

//agr gdp

eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 i.year i.iso2 t_*, cluster(iso2)
eststo: reg agr_gdpgrowthcap  temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 t_*, cluster(iso2)
eststo: reg agr_gdpgrowthcap  temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 t_*, cluster(iso2) 
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 

esttab using agrgdp_macroreg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

//bottom 50 //robustness
eststo: reg bottom50 temp temp_2 prec prec_2 i.year i.iso2 t_*, cluster(iso2)
eststo: reg bottom50 temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 t_*, cluster(iso2) //SI Appendix, Table S7
eststo: reg bottom50 temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 t_*, cluster(iso2) 
eststo: reg bottom50 temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 
eststo: reg bottom50 temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 

esttab using b50_macroreg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

eststo clear


//gdp flex trends
 
eststo: reg growthWDI temp temp_2 prec prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg growthWDI  temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg growthWDI temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2) 

esttab using gdp_flex.tex, star(* 0.10 ** 0.05 *** 0.01) replace //SI Appendix, Table S11

eststo clear

//agr gdp flex trends

eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg agr_gdpgrowthcap  temp temp_2 prec prec_2 t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2)
eststo: reg agr_gdpgrowthcap temp temp_2 prec prec_2 agriemp_sq sq_emp_temp_c sq_emp_temp2_c sq_emp_prec_c sq_emp_prec2_c i.year i.iso2 c.time_trend##i.iso2 c.time_trend_sq##i.iso2, cluster(iso2) 

esttab using agrgdp_flex.tex, star(* 0.10 ** 0.05 *** 0.01) replace //SI Appendix, Table S12

eststo clear


//////////////////
//Margins for GDP

//High agri, Prec for GDP

reg growthWDI temp temp_2 c.prec##c.prec b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 t_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)
parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_macroreg_growth.csv, comma replace


//High agri, Prec for Agri GDP
//each time you run margins for the second time, re-load data (lines 370-423)

reg agr_gdpgrowthcap temp temp_2 c.prec##c.prec b80_temp b80_temp_2 b80_prec b80_prec_2 i.year i.iso2 t_*, cluster(iso2)

mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)
parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_macroreg_agrgdp.csv, comma replace


//Low agri, GDP

reg growthWDI temp temp_2 c.prec##c.prec t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 t_*, cluster(iso2)
mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)
parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_macroreg_growthb80.csv, comma replace

//High agri, Agri GDP

reg agr_gdpgrowthcap temp temp_2 c.prec##c.prec t20_temp t20_temp_2 t20_prec t20_prec_2 i.year i.iso2 t_*, cluster(iso2)

mat b = e(b)

mat b = b[1,3..4] 

matrix list b
di _b[prec]/-2/_b[c.prec#c.prec]
loc min 0
margins, at(prec=(`min'(0.1)4)) post noestimcheck level(90)
parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using estimated_response_macroreg_agrgdpb80.csv, comma replace



//// Margins for Supplementary Information 

///// Temperature margins for gdppc
//each time you run margins for the second time, re-load data (lines 370-423)


reg growthWDI c.temp##c.temp prec prec_2 agriemp emp_temp_c emp_temp2_c emp_prec_c emp_prec2_c i.year i.iso2 t_*, cluster(iso2) 
mat b = e(b)

mat b = b[1,1..2] 

matrix list b
di _b[temp]/-2/_b[c.temp#c.temp]
loc min 0
margins, at(temp=(`min'(1)35)) post noestimcheck level(90)


parmest, norestore level(90)
split parm, p("." "#")
ren parm1 x
destring x, replace
replace x = x + `min' - 1  
drop parm* 
outsheet using est_response_gdp_temp_cont.csv, comma replace



