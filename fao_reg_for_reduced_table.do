///Data for prices farmers obtain on agricultural products

clear all
set more off 
cd "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision"

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision/data_revision/data_bottom_fao_prod.csv"


destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2

gen b80_value=agri2_b80*value
gen b80_temp_value= agri2_b80*temp*value
gen b80_temp_2_value = agri2_b80*temp_2*value
gen b80_prec_value = agri2_b80*prec*value
gen b80_prec_2_value = agri2_b80*prec_2*value

gen temp_value= temp*value
gen temp_2_value = temp_2*value
gen prec_value = prec*value
gen prec_2_value = prec_2*value

eststo clear
	
eststo: reg value temp temp_2 prec prec_2 i.iso2 i.year, cluster(iso2)

eststo: reg value temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year, cluster(iso2)

reg bottom50 value temp temp_2 prec prec_2 temp_value temp_2_value prec_value prec_2_value i.iso2 i.year, cluster(iso2)

//This reg 
reg bottom50 value temp temp_2 prec prec_2 temp_value temp_2_value prec_value prec_2_value b80_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//reg bottom50 temp temp_2 prec prec_2 temp_value temp_2_value prec_value prec_2_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//reg bottom50 temp_value temp_2_value prec_value prec_2_value b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)


reg bottom50 value temp temp_2 prec prec_2 b80_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//esttab using table_revision_prod.tex, replace compress nogaps title(Regression table\label{tab}) star(* 0.10 ** 0.05 *** 0.01)


///Data for food prices for consumption 

//clear all 

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision/data_revision/data_bottom_fao_cons.csv", clear


destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2

gen b80_year_price=agri2_b80*year_price
gen b80_temp_year_price= agri2_b80*temp*year_price
gen b80_temp_2_year_price = agri2_b80*temp_2*year_price
gen b80_prec_year_price = agri2_b80*prec*year_price
gen b80_prec_2_year_price = agri2_b80*prec_2*year_price

gen temp_year_price= temp*year_price
gen temp_2_year_price = temp_2*year_price
gen prec_year_price = prec*year_price
gen prec_2_year_price = prec_2*year_price

eststo: reg year_price temp temp_2 prec prec_2 i.iso2 i.year, cluster(iso2)

eststo: reg year_price temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year, cluster(iso2)

reg bottom50 year_price temp temp_2 prec prec_2 temp_year_price temp_2_year_price prec_year_price prec_2_year_price i.iso2 i.year, cluster(iso2)

//This reg 
reg bottom50 year_price temp temp_2 prec prec_2 temp_year_price temp_2_year_price prec_year_price prec_2_year_price b80_year_price b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_year_price b80_temp_2_year_price b80_prec_year_price b80_prec_2_year_price i.iso2 i.year, cluster(iso2)

//reg bottom50 temp temp_2 prec prec_2 temp_year_price temp_2_value prec_value prec_2_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//reg bottom50 temp_value temp_2_value prec_value prec_2_value b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)


//reg bottom50 value temp temp_2 prec prec_2 b80_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

 //esttab using table_revision_cons_new.tex, replace compress nogaps title(Regression table\label{tab}) star(* 0.10 ** 0.05 *** 0.01)

 //eststo clear
 

 
 ///Data for inflation on prices farmers obtain on agricultural products

//clear all
set more off 
//cd "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision"

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision/data_revision/data_bottom_fao_prod.csv", clear


destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2

tsset iso2 year
by iso2: gen inflation = log(value)-log(l.value) 

gen b80_inflation=agri2_b80*inflation
gen b80_temp_inflation= agri2_b80*temp*inflation
gen b80_temp_2_inflation= agri2_b80*temp_2*inflation
gen b80_prec_inflation = agri2_b80*prec*inflation
gen b80_prec_2_inflation = agri2_b80*prec_2*inflation

gen temp_inflation= temp*inflation
gen temp_2_inflation = temp_2*inflation
gen prec_inflation = prec*inflation
gen prec_2_inflation = prec_2*inflation

//eststo clear
	
eststo: reg inflation temp temp_2 prec prec_2 i.iso2 i.year, cluster(iso2)

eststo: reg inflation temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year, cluster(iso2)

reg bottom50 inflation temp temp_2 prec prec_2 temp_inflation temp_2_inflation prec_inflation prec_2_inflation i.iso2 i.year, cluster(iso2)

//This reg 
reg bottom50 inflation temp temp_2 prec prec_2 temp_inflation temp_2_inflation prec_inflation prec_2_inflation b80_inflation b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_inflation b80_temp_2_inflation b80_prec_inflation b80_prec_2_inflation i.iso2 i.year, cluster(iso2)

//reg bottom50 temp temp_2 prec prec_2 temp_value temp_2_value prec_value prec_2_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//reg bottom50 temp_value temp_2_value prec_value prec_2_value b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)


reg bottom50 inflation temp temp_2 prec prec_2 b80_inflation b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_inflation b80_temp_2_inflation b80_prec_inflation b80_prec_2_inflation i.iso2 i.year, cluster(iso2)

//esttab using table_revision_prod_inflation.tex, replace compress nogaps title(Regression table\label{tab}) star(* 0.10 ** 0.05 *** 0.01)




///Data for food INFLATION for consumption 

//clear all 

import delimited "/Users/elisapalagi/Documents/replication/replication_clim_ineq_revision/data_revision/data_bottom_fao_cons_inflation.csv", clear


destring udel_temp_popweight, gen(temp) force
encode country, gen(iso2)
destring udel_precip_popweight, gen(prec) force
destring udel_precip_popweight_2, gen(prec_2) force
destring temp_sq, gen(temp_2) force

destring agri_b80, gen(agri2_b80) force
destring agri_top20, gen(agri2_top20) force
gen b80_temp= agri2_b80*temp 
gen b80_temp_2 = agri2_b80*temp_2 
gen b80_prec = agri2_b80*prec
gen b80_prec_2 = agri2_b80*prec_2
gen t20_temp= agri2_top20*temp 
gen t20_temp_2 = agri2_top20*temp_2 
gen t20_prec = agri2_top20*prec
gen t20_prec_2 = agri2_top20*prec_2

gen b80_year_inflation=agri2_b80*year_inflation
gen b80_temp_year_inflation= agri2_b80*temp*year_inflation
gen b80_temp_2_year_inflation = agri2_b80*temp_2*year_inflation
gen b80_prec_year_inflation = agri2_b80*prec*year_inflation
gen b80_prec_2_year_inflation = agri2_b80*prec_2*year_inflation

gen temp_year_inflation= temp*year_inflation
gen temp_2_year_inflation = temp_2*year_inflation
gen prec_year_inflation = prec*year_inflation
gen prec_2_year_inflation = prec_2*year_inflation

eststo: reg year_inflation temp temp_2 prec prec_2 i.iso2 i.year, cluster(iso2)

eststo: reg year_inflation temp temp_2 prec prec_2 b80_temp b80_temp_2 b80_prec b80_prec_2 i.iso2 i.year, cluster(iso2)

reg bottom50 year_inflation temp temp_2 prec prec_2 temp_year_inflation temp_2_year_inflation prec_year_inflation prec_2_year_inflation i.iso2 i.year, cluster(iso2)

//This reg 
reg bottom50 year_inflation temp temp_2 prec prec_2 temp_year_inflation temp_2_year_inflation prec_year_inflation prec_2_year_inflation b80_year_inflation b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_year_inflation b80_temp_2_year_inflation b80_prec_year_inflation b80_prec_2_year_inflation i.iso2 i.year, cluster(iso2)

//reg bottom50 temp temp_2 prec prec_2 temp_value temp_2_value prec_value prec_2_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

//reg bottom50 temp_value temp_2_value prec_value prec_2_value b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)


//reg bottom50 value temp temp_2 prec prec_2 b80_value b80_temp b80_temp_2 b80_prec b80_prec_2 b80_temp_value b80_temp_2_value b80_prec_value b80_prec_2_value i.iso2 i.year, cluster(iso2)

 //esttab using table_revision_cons_inflation.tex, replace compress nogaps title(Regression table\label{tab}) star(* 0.10 ** 0.05 *** 0.01)

 //eststo clear

 esttab using table_revision_price_and_inflation.tex, replace compress nogaps title(Regression table\label{tab}) star(* 0.10 ** 0.05 *** 0.01)

 
 
 
