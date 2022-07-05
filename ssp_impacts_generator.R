rm(list = ls())
setwd("~/Google Drive/replication_clim_ineq_sub2")

library("readxl")
library(readr)
library(tidyverse)
library(magrittr)
library(plm)
require(lmtest)
library(ISOcodes)
library(stargazer)
library(countrycode)
library(haven)
library(labstatR)
library(ggallin)
library(grattan)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggplot2)
library(dineq)  
library(ineq)

#combinations:
#RCP8.5 SSP5
#RCP8.5 SSP3
#RCP6.0 SSP2
#RCP4.5 SSP2
#RCP2.6 SSP1

#RCPs
rcps <- c("85","60","45","26")
ssps <- c("1","2","3","5")
i_rcp <- 1 #select RCP
i_ssp <- 4 #select SSP



#### Preparing precipitations and temperature projections #### 
#for RCP 2.6 just 26 instead of 85
#wb precipitations projections #NB! Choose RCP data
some_proj_1 <- read_csv(paste0("data/projections_wb/pr_2080_2099_mavg_rcp",rcps[i_rcp],"_AFG_ALB_DZA_AND_AGO_ATG_ARG_ARM_AUS_AUT_AZE_BHS_BHR_BGD_BRB_BLR_BEL_BLZ_BEN_BTN_BOL_BIH_BWA_BRA_BRN_BGR_BFA_BDI_KHM_CMR_CAN_CPV_CAF_TCD_CHL_CHN_COL_COM_COD_COG_CRI_CIV_HRV_CUB_CYP_CZE.csv"))
some_proj_2 <- read_csv(paste0("data/projections_wb/pr_2080_2099_mavg_rcp",rcps[i_rcp],"_DNK_DJI_DMA_DOM_ECU_EGY_SLV_GNQ_ERI_EST_ETH_FRO_FSM_FJI_FIN_FRA_GAB_GMB_GEO_DEU_GHA_GRC_GRL_GRD_GTM_GIN_GNB_GUY_HTI_HND_HUN_ISL_IND_IDN_IRN_IRQ_IRL_ISR_ITA_JAM_JPN_JOR_KAZ_KEN_KIR_PRK_KOR_XRK_KWT_KGZ.csv"))
some_proj_3 <- read_csv(paste0("data/projections_wb/pr_2080_2099_mavg_rcp",rcps[i_rcp],"_LAO_LVA_LBN_LSO_LBR_LBY_LIE_LTU_LUX_MKD_MDG_MWI_MYS_MDV_MLI_MLT_MHL_MRT_MUS_MEX_MDA_MCO_MNG_MAR_MOZ_MMR_NAM_NPL_NLD_NCL_NZL_NIC_NER_NGA_MNP_NOR.csv"))
some_proj_4 <- read_csv(paste0("data/projections_wb/pr_2080_2099_mavg_rcp",rcps[i_rcp],"_OMN_PAK_PLW_PAN_PNG_PRY_PER_PHL_POL_PRT_PRI_QAT_MNE_SRB_ROU_RUS_RWA.csv"))
some_proj_5 <- read_csv(paste0("data/projections_wb/pr_2080_2099_mavg_rcp",rcps[i_rcp],"_WSM_STP_SAU_SEN_SYC_SLE_SGP_SVK_SVN_SLB_SOM_ZAF_SSD_ESP_LKA_KNA_LCA_VCT_SDN_SUR_SWZ_SWE_CHE_SYR_TJK_TZA_THA_TLS_TGO_TON_TTO_TUN_TUR_TKM_TUV_UGA_UKR_ARE_GBR_USA_URY_UZB_VUT_VEN_VNM_YEM_ZMB_ZWE.csv"))
some_proj <- rbind(some_proj_1, some_proj_2, some_proj_3, some_proj_4, some_proj_5)
remove(some_proj_1, some_proj_2, some_proj_3, some_proj_4, some_proj_5)

#exclude scenarios referring to model projections percentiles 
some_proj %<>% filter(Model!="Ensemble (10th Percentile)")
some_proj %<>% filter(Model!="Ensemble (50th Percentile)")
some_proj %<>% filter(Model!="Ensemble (90th Percentile)")
#some_proj$`Monthly Precipitation - (MM)`[some_proj$`Monthly Precipitation - (MM)`<0]

#wb prec data to calculate growth rates
wb_prec_1 <- read_csv("data/projections_wb/pr_1991_2016_AFG_ALB_DZA_AND_AGO_ATG_ARG_ARM_AUS_AUT_AZE_BHS_BHR_BGD_BRB_BLR_BEL_BLZ_BEN_BTN_BOL_BIH_BWA_BRA_BRN_BGR_BFA_BDI_KHM_CMR_CAN_CPV_CAF_TCD_CHL_CHN_COL_COM_COD_COG_CRI_CIV_HRV_CUB_CYP_CZE.csv")
wb_prec_2 <- read_csv("data/projections_wb/pr_1991_2016_DNK_DJI_DMA_DOM_ECU_EGY_SLV_GNQ_ERI_EST_ETH_FRO_FSM_FJI_FIN_FRA_GAB_GMB_GEO_DEU_GHA_GRC_GRL_GRD_GTM_GIN_GNB_GUY_HTI_HND_HUN_ISL_IND_IDN_IRN_IRQ_IRL_ISR_ITA_JAM_JPN_JOR_KAZ_KEN_KIR_PRK_KOR_XRK_KWT_KGZ.csv")
wb_prec_3 <- read_csv("data/projections_wb/pr_1991_2016_LAO_LVA_LBN_LSO_LBR_LBY_LIE_LTU_LUX_MKD_MDG_MWI_MYS_MDV_MLI_MLT_MHL_MRT_MUS_MEX_MDA_MCO_MNG_MAR_MOZ_MMR_NAM_NPL_NLD_NCL_NZL_NIC_NER_NGA_MNP_NOR.csv")
wb_prec_4 <- read_csv("data/projections_wb/pr_1991_2016_OMN_PAK_PLW_PAN_PNG_PRY_PER_PHL_POL_PRT_PRI_QAT_MNE_SRB_ROU_RUS_RWA.csv")
wb_prec_5 <- read_csv("data/projections_wb/pr_1991_2016_WSM_STP_SAU_SEN_SYC_SLE_SGP_SVK_SVN_SLB_SOM_ZAF_SSD_ESP_LKA_KNA_LCA_VCT_SDN_SUR_SWZ_SWE_CHE_SYR_TJK_TZA_THA_TLS_TGO_TON_TTO_TUN_TUR_TKM_TUV_UGA_UKR_ARE_GBR_USA_URY_UZB_VUT_VEN_VNM_YEM_ZMB_ZWE.csv")
wb_prec <- rbind(wb_prec_1, wb_prec_2, wb_prec_3, wb_prec_4, wb_prec_5)
remove(wb_prec_1, wb_prec_2, wb_prec_3, wb_prec_4, wb_prec_5)

wb_prec %<>% group_by(Year,Country) %>% mutate(
  yearly_prec = sum(`Rainfall - (MM)`),
  yearly_prec =yearly_prec/1000 #to have in meters
) %>%ungroup()

wb_prec %<>% group_by(ISO3) %>% distinct(Year,yearly_prec) %>%ungroup()
wb_prec %<>% group_by(ISO3)%>% mutate(avg_prec=mean(yearly_prec)) %>%ungroup()
wb_prec %<>% group_by(ISO3) %>% distinct(avg_prec) %>%ungroup()

some_proj %<>% group_by(Model,ISO3) %>% mutate(
  proj_tot = sum(`Monthly Precipitation - (MM)`) #get total yearly #as specified in World Bank Metadata, Precipitations are totals over the selected period!
) %>%ungroup()

some_proj %<>% mutate(proj_tot =proj_tot/1000)

some_proj %<>% group_by(Model,ISO3) %>% distinct(proj_tot,Year) %>%ungroup()

proj_wb_prec<-inner_join(wb_prec, some_proj, by = c("ISO3"="ISO3"))
#Transform Tanzania ISO3 from "United Republic of" to TZA
proj_wb_prec %<>% mutate(
  ISO3 = case_when(
    ISO3=="United Republic of, TZA" ~ "TZA",
    TRUE ~ ISO3
  )
) 


#wb temperature projections #NB! Choose RCP data
some_proj_1_t <- read_csv(paste0("data/projections_wb/tas_2080_2099_mavg_rcp",rcps[i_rcp],"_AFG_ALB_DZA_AND_AGO_ATG_ARG_ARM_AUS_AUT_AZE_BHS_BHR_BGD_BRB_BLR_BEL_BLZ_BEN_BTN_BOL_BIH_BWA_BRA_BRN_BGR_BFA_BDI_KHM_CMR_CAN_CPV_CAF_TCD_CHL_CHN_COL_COM_COD_COG_CRI_CIV_HRV_CUB_CYP_CZE.csv"))
some_proj_2_t <- read_csv(paste0("data/projections_wb/tas_2080_2099_mavg_rcp",rcps[i_rcp],"_DNK_DJI_DMA_DOM_ECU_EGY_SLV_GNQ_ERI_EST_ETH_FRO_FSM_FJI_FIN_FRA_GAB_GMB_GEO_DEU_GHA_GRC_GRL_GRD_GTM_GIN_GNB_GUY_HTI_HND_HUN_ISL_IND_IDN_IRN_IRQ_IRL_ISR_ITA_JAM_JPN_JOR_KAZ_KEN_KIR_PRK_KOR_XRK_KWT_KGZ.csv"))
some_proj_3_t <- read_csv(paste0("data/projections_wb/tas_2080_2099_mavg_rcp",rcps[i_rcp],"_LAO_LVA_LBN_LSO_LBR_LBY_LIE_LTU_LUX_MKD_MDG_MWI_MYS_MDV_MLI_MLT_MHL_MRT_MUS_MEX_MDA_MCO_MNG_MAR_MOZ_MMR_NAM_NPL_NLD_NCL_NZL_NIC_NER_NGA_MNP_NOR.csv"))
some_proj_4_t <- read_csv(paste0("data/projections_wb/tas_2080_2099_mavg_rcp",rcps[i_rcp],"_OMN_PAK_PLW_PAN_PNG_PRY_PER_PHL_POL_PRT_PRI_QAT_MNE_SRB_ROU_RUS_RWA.csv"))
some_proj_5_t <- read_csv(paste0("data/projections_wb/tas_2080_2099_mavg_rcp",rcps[i_rcp],"_WSM_STP_SAU_SEN_SYC_SLE_SGP_SVK_SVN_SLB_SOM_ZAF_SSD_ESP_LKA_KNA_LCA_VCT_SDN_SUR_SWZ_SWE_CHE_SYR_TJK_TZA_THA_TLS_TGO_TON_TTO_TUN_TUR_TKM_TUV_UGA_UKR_ARE_GBR_USA_URY_UZB_VUT_VEN_VNM_YEM_ZMB_ZWE.csv"))
some_proj_t <- rbind(some_proj_1_t, some_proj_2_t, some_proj_3_t, some_proj_4_t, some_proj_5_t)
remove(some_proj_1_t, some_proj_2_t, some_proj_3_t, some_proj_4_t, some_proj_5_t)

#exclude scenarios referring to model projections percentiles 
some_proj_t %<>% filter(Model!="Ensemble (10th Percentile)")
some_proj_t %<>% filter(Model!="Ensemble (50th Percentile)")
some_proj_t %<>% filter(Model!="Ensemble (90th Percentile)")

#wb temp data to calculate growth rates
wb_temp_1 <- read_csv("data/projections_wb/tas_1991_2016_AFG_ALB_DZA_AND_AGO_ATG_ARG_ARM_AUS_AUT_AZE_BHS_BHR_BGD_BRB_BLR_BEL_BLZ_BEN_BTN_BOL_BIH_BWA_BRA_BRN_BGR_BFA_BDI_KHM_CMR_CAN_CPV_CAF_TCD_CHL_CHN_COL_COM_COD_COG_CRI_CIV_HRV_CUB_CYP_CZE.csv")
wb_temp_2 <- read_csv("data/projections_wb/tas_1991_2016_DNK_DJI_DMA_DOM_ECU_EGY_SLV_GNQ_ERI_EST_ETH_FRO_FSM_FJI_FIN_FRA_GAB_GMB_GEO_DEU_GHA_GRC_GRL_GRD_GTM_GIN_GNB_GUY_HTI_HND_HUN_ISL_IND_IDN_IRN_IRQ_IRL_ISR_ITA_JAM_JPN_JOR_KAZ_KEN_KIR_PRK_KOR_XRK_KWT_KGZ.csv")
wb_temp_3 <- read_csv("data/projections_wb/tas_1991_2016_LAO_LVA_LBN_LSO_LBR_LBY_LIE_LTU_LUX_MKD_MDG_MWI_MYS_MDV_MLI_MLT_MHL_MRT_MUS_MEX_MDA_MCO_MNG_MAR_MOZ_MMR_NAM_NPL_NLD_NCL_NZL_NIC_NER_NGA_MNP_NOR.csv")
wb_temp_4 <- read_csv("data/projections_wb/tas_1991_2016_OMN_PAK_PLW_PAN_PNG_PRY_PER_PHL_POL_PRT_PRI_QAT_MNE_SRB_ROU_RUS_RWA.csv")
wb_temp_5 <- read_csv("data/projections_wb/tas_1991_2016_WSM_STP_SAU_SEN_SYC_SLE_SGP_SVK_SVN_SLB_SOM_ZAF_SSD_ESP_LKA_KNA_LCA_VCT_SDN_SUR_SWZ_SWE_CHE_SYR_TJK_TZA_THA_TLS_TGO_TON_TTO_TUN_TUR_TKM_TUV_UGA_UKR_ARE_GBR_USA_URY_UZB_VUT_VEN_VNM_YEM_ZMB_ZWE.csv")
wb_temp <- rbind(wb_temp_1, wb_temp_2, wb_temp_3, wb_temp_4, wb_temp_5)
remove(wb_temp_1, wb_temp_2, wb_temp_3, wb_temp_4, wb_temp_5)


wb_temp %<>% group_by(Year,Country) %>% mutate(
  yearly_temp = mean(`Temperature - (Celsius)`,na.rm = TRUE)
) %>%ungroup()

wb_temp %<>% group_by(ISO3) %>% distinct(Year,yearly_temp) %>%ungroup()
wb_temp %<>% group_by(ISO3)%>% mutate(avg_temp=mean(yearly_temp),na.rm = TRUE) %>%ungroup()
wb_temp %<>% group_by(ISO3) %>% distinct(avg_temp) %>%ungroup()

some_proj_t %<>% group_by(Model,ISO3) %>% mutate(
  proj_mean = mean(`Monthly Temperature - (Celsius)`, na.rm=TRUE) #get mean yearly
) %>%ungroup()


some_proj_t %<>% group_by(Model,ISO3) %>% distinct(proj_mean,Year) %>%ungroup()


proj_wb_temp<-inner_join(wb_temp, some_proj_t, by = c("ISO3"="ISO3"))
remove(wb_temp,some_proj_t)
#Transform Tanzania ISO3 from "United Republic of" to TZA
proj_wb_temp %<>% mutate(
  ISO3 = case_when(
    ISO3=="United Republic of, TZA" ~ "TZA",
    TRUE ~ ISO3
  )
) 

proj_wb_temp %<>% rename(proj_mean_temp=proj_mean)

proj_wb_prec %<>% rename(proj_tot_prec=proj_tot)

proj_all <-inner_join(proj_wb_prec, proj_wb_temp, by = c("ISO3"="ISO3","Year"="Year","Model"="Model"))


#### Adjust Agri emp ####

data_b <- read_csv("data/data_bottom_different_q.csv")
data_b$country[is.na(data_b$country)]<-"NA" #to include Nambia

#select columns data b
data_b %<>% 
  select(-avg_agri_share, -avg_agri_emp)

agri_emp_scenarios <- readRDS("data_revision/agri_emp_scenarios.rds")

data_b %<>% 
  left_join(
    .,
    agri_emp_scenarios %>% 
      select(country, avg_agri_emp, paste0("agri_emp_ssp",ssps[i_ssp])),
    by="country"
  ) %>% 
  rename("final_avg_agri_emp" = paste0("agri_emp_ssp",ssps[i_ssp]))
  

data_b%<>% filter(year>1990)%>%filter(year<2011)%>% group_by(country) %>% mutate( #1990 instead of 1989 for agridiff
  first= dplyr::first(bottom50),
  last=dplyr::last(bottom50),
  first_gdp=dplyr::first(na.omit(gdpCAPppp_wdi)),
  last_gdp=dplyr::last(gdpCAPppp_wdi),
  first_agri=dplyr::first(agri_emp),
  last_agri=dplyr::last(agri_emp)
) %>% ungroup()


data_b%<>% mutate(
  b50_diff= (last-first)/19, #if from 1980 diviso 30 ma più NA
  gdp_rate=(log(last_gdp)-log(first_gdp))/19,
  agri_diff=(final_avg_agri_emp-avg_agri_emp)/70 #out of sample!
) 

#manual adjustments for countries that have some NAs in GDP (without na.omit in first_gdp)
check_na<-data_b%>%filter(country=="BA"|country=="EE"|country=="HR"|country=="IQ"|country=="KW"|country=="QA"|country=="ST"|country=="SY")

data_b%<>%mutate(gdp_rate=case_when(
  country=="BA"~ (log(last_gdp)-log(first_gdp))/17,
  country=="EE"~ (log(last_gdp)-log(first_gdp))/18,
  country=="HR"~ (log(last_gdp)-log(first_gdp))/16,
  country=="IQ"~ (log(last_gdp)-log(first_gdp))/11,
  country=="KW"~ (log(last_gdp)-log(first_gdp))/16,
  country=="QA"~ (log(last_gdp)-log(first_gdp))/11,
  country=="ST"~ (log(last_gdp)-log(first_gdp))/11,
  TRUE ~ gdp_rate
))


bottom_proj_wb<-inner_join(data_b, proj_all, by = c("country_iso3"="ISO3"))


bottom_proj_wb$proj_tot_prec[bottom_proj_wb$proj_tot_prec<0]<-0  #for simplicity put to 0 negative values

bottom_proj_wb %<>% mutate(
  comp_growth_prec = ((proj_tot_prec/avg_prec)^(1/70)) - 1, #to have yearly growth rate. Here I use average hist prec.  
  comp_growth_temp = ((proj_mean_temp/avg_temp)^(1/70)) - 1 
)



#adjustment for Russia, which starts by negative non-populated average temperature (-5.255763), and so produces NaN in comp growth 
#bottom_proj_wb %<>% mutate(
#  comp_growth_temp=case_when(
#    country=="RU" & proj_mean_temp>0 ~ ((5.255763+proj_mean_temp)/70),
#    country=="RU" & proj_mean_temp<0 ~ ((5.255763-proj_mean_temp)/70),
#    TRUE ~ comp_growth_temp))

#Russia adjusted:
#bottom_proj_wb %<>% mutate(
#  comp_growth_temp=case_when(
#    country=="RU" ~ ((5.255763+proj_mean_temp)/70),
#    TRUE ~ comp_growth_temp))

#bottom_proj_wb %<>% 
#  mutate(
#    comp_growth_temp = case_when(
#      (proj_mean_temp/avg_temp)<0 ~ (proj_mean_temp - avg_temp)/70,
#      T ~ comp_growth_temp
#    )
#  )

bottom_proj_wb %>% 
  filter((proj_mean_temp/avg_temp)<0) %>% 
  pull(country) %>% unique() -> sign_change_countries

bottom_proj_wb %<>% 
  mutate(
    comp_growth_temp = case_when(
      country %in% sign_change_countries ~ (proj_mean_temp - avg_temp)/70,
      T ~ comp_growth_temp
    )
  )

#bottom_proj_wb$diff <- ave(bottom_proj_wb$bottom50 , bottom_proj_wb$country , FUN=function(i) c(NA,diff(i)))

bottom_proj_wb %<>% filter(year>1990) %>% group_by(country) %>% mutate( #or 1979! #1990 invece di 1989 per agridiff
  #mean_diff = mean(diff, na.rm=T),
  mean_b50 =mean(bottom50, na.rm=T),
  mean_prec =mean(UDel_precip_popweight, na.rm=T),
  mean_temp =mean(UDel_temp_popweight, na.rm=T),
  mean_growth =mean(growthWDI, na.rm=T),
  mean_gdp=mean(gdpCAPppp_wdi, na.rm=T)
) %>% ungroup()

d2010 <- bottom_proj_wb%>% filter(year==2010)
d2010 %<>% select(country, country_iso3, UDel_precip_popweight, UDel_precip_popweight_2, UDel_temp_popweight, temp_sq, comp_growth_prec, comp_growth_temp, agri_top20, agri_b80, bottom50, mean_prec, mean_temp, mean_b50, mean_growth, mean_gdp, avg_agri_emp, Model, b50_diff, gdp_rate, agri_diff)

d2010 %<>% 
  rename(agri_emp = avg_agri_emp) # renaming avg_agri_emp as agri_emp to employ it in the rest of the code

countries = length(unique(bottom_proj_wb$country))
model = length(unique(bottom_proj_wb$Model))
years = 71




output <- matrix(ncol=countries*model, nrow=years)

output[1,] <- d2010$mean_prec #here I put average historical weighted precipitations as starting point.  

#weighted precipitation projections
for (i in 1:length(d2010$country)){
  for (t in 1:70){
    if(output[t][!is.na(output[t])]){
      output[t+1,i] = output[1,i]*((1+d2010$comp_growth_prec[i])^t)}
    else{
      output[t+1,i] = NA}
  }}



colnames(output) <- d2010$country #ex. AE AE.1 AE2. .... AE.15 (16 models)
rownames(output) <- 2010:2080

output <-data.frame(output)
year <- rownames(output)
data <- cbind(year,output)
output_long <- gather(data, country, proj, "AE":"NA..15")  


#So country is AE: first country model 1; AE.1 first country model 2 etc. 

output_long %<>% group_by(country) %>% mutate(
  initial = proj[year==2010],
  final = proj[year==2080]
) %>% ungroup()

output_long %<>% group_by(country) %>% mutate(
  abs_diff_proj = proj[year==2080] - proj[year==2010],
  grate_proj = (proj[year==2080] - proj[year==2010])/proj[year==2010]
) %>% ungroup()







output_t <- matrix(ncol=countries*model, nrow=years)

output_t[1,] <- d2010$mean_temp #here I put average historical weighted temperatures as starting point.  

#weighted temperature projections
for (i in 1:length(d2010$country)){
  for (t in 1:70){
    if(output_t[t][!is.na(output_t[t])]){
      output_t[t+1,i] = output_t[1,i]*((1+d2010$comp_growth_temp[i])^t)}
    else{
      output_t[t+1,i] = NA}
  }}

colnames(output_t) <- d2010$country #ex. AE AE.1 AE2. .... AE.15 (16 models) 
rownames(output_t) <- 2010:2080

cols_to_heal <- which(colnames(output_t) %in% sign_change_countries)

#adjust columns
for (i in cols_to_heal){
  print(i)
  for (t in 1:70){
    output_t[t+1,i] = output_t[t,i]+d2010$comp_growth_temp[i]}
}


##ADJUST RUSSIA
#for (i in 1249:1264){
#  for (t in 1:70){
#    output_t[t+1,i] = output_t[t,i]+d2010$comp_growth_temp[i]}
#}


output_t <-data.frame(output_t)
year <- rownames(output_t)
data_t <- cbind(year,output_t)
output_long_t <- gather(data_t, country, proj, "AE":"NA..15") 

#So country is AE: first country model 1; AE.1 first country model 2 etc. 

output_long_t %<>% group_by(country) %>% mutate(
  initial = proj[year==2010],
  final = proj[year==2080]
) %>% ungroup()

output_long_t %<>% group_by(country) %>% mutate(
  abs_diff_proj = proj[year==2080] - proj[year==2010],
  grate_proj = (proj[year==2080] - proj[year==2010])/proj[year==2010]
) %>% ungroup()

output_long %<>% rename(proj_prec=proj,
                        initial_prec=initial,
                        final_prec=final,
                        abs_diff_proj_prec=abs_diff_proj,
                        grate_proj_prec=grate_proj)

output_long_t %<>% rename(proj_temp=proj,
                          initial_temp=initial,
                          final_temp=final,
                          abs_diff_proj_temp=abs_diff_proj,
                          grate_proj_temp=grate_proj)


output_long_all <-inner_join(output_long, output_long_t, by = c("country"="country","year"="year"))



#### Bottom 50 projections prec####

output_b <- matrix(ncol=countries*model, nrow=years)

output_b[1,] <- d2010$mean_b50

# -0.0331 coef prec
# 0.0133 prec^2
# 0.000774 prec*agri_emp
#-0.000311 prec^2*agri_empt

for (i in 1:length(d2010$country)){
  for (t in 1:70){
      if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) > 99){ #if upper boundary in agri emp is reached
        
        output_b[t+1,i] = output_b[t,i]+  ((-0.0331 + 2*0.0133*(output[t+1,i])) + (0.000774*99) + (2*(-0.000311)*99*(output[t+1,i])))*(output[t+1,i]-output[t,i])
        
      } else if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) < 1){ #if lower boundary in agri emp is reached
        
        output_b[t+1,i] = output_b[t,i]+ ((-0.0331 + 2*0.0133*(output[t+1,i])) + (0.000774*1) + (2*(-0.000311)*1*(output[t+1,i])))*(output[t+1,i]-output[t,i])
        
      }else{
        
        output_b[t+1,i] = output_b[t,i]+ ((-0.0331 + 2*0.0133*(output[t+1,i])) + (0.000774*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))) + (2*(-0.000311)*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*(output[t+1,i])))*(output[t+1,i]-output[t,i]) #using projected agri emp (with agri_diff)
        
      }
  }
}



output_b[output_b<0]<-0
output_b[output_b>0.5]<-0.5

colnames(output_b) <- d2010$country
rownames(output_b) <- 2010:2080

output_b <-data.frame(output_b)
year <- rownames(output_b)
data <- cbind(year,output_b)
output_b_long <- gather(data, country, b_proj, "AE":"NA..15") 

output_b_long_joined<-inner_join(output_b_long, output_long, by = c("country"="country", "year"="year"))

output_b_long_joined %<>% group_by(country) %>% mutate(
  abs_diff_b50proj_prec = b_proj[year==2080] - b_proj[year==2010],
  grate_b50proj_prec = (b_proj[year==2080] - b_proj[year==2010])/b_proj[year==2010]
) %>% ungroup()

#### Load SSP ####
ssp_proj<-read_csv(paste0("data_revision/ssp_to_join_SSP",ssps[i_ssp],".csv")) #NB! Choose SSP data
ssp_proj$country_iso2[is.na(ssp_proj$country_iso2)]<-"NA" #to include Nambia


#### GDP pc proj prec####

output_gdp <- matrix(ncol=countries*model, nrow=years)

output_gdp[1,] <- d2010$mean_gdp

for (i in 1:length(d2010$country)){
  for (t in 1:70){
    if(is_empty(ssp_proj$pc_gdp_growth_ssp[ssp_proj$country_iso2==d2010$country[i]])==FALSE){ #ssp part
      if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) > 99){ #if upper bound in agri emp reached
        
        delta = ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]] 
        eta=  (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*99*output[t+1,i]) + (-0.000916*99*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*99*output[1,i]) + (-0.000916*99*(output[1,i])^2)))
        output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta) 
        
      } else if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) < 1){ #if lower bound in agri emp reached
        
        delta = ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]]
        eta = (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*1*output[t+1,i]) + (-0.000916*1*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*1*output[1,i]) + (-0.000916*1*(output[1,i])^2)))
        output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta)
        
      }else{
        
        delta= ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]] 
        eta= (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*output[t+1,i]) + (-0.000916*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*output[1,i]) + (-0.000916*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*(output[1,i])^2)))
        output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta) #using projected agri emp with agri_diff
      }}#} 
    else{
      output_gdp[t+1,i] = NA}
  }}


colnames(output_gdp) <- d2010$country
rownames(output_gdp) <- 2010:2080

output_gdp <-data.frame(output_gdp)
year <- rownames(output_gdp)
data <- cbind(year,output_gdp)
output_gdp_long <- gather(data, country, gdp_proj_prec, "AE":"NA..15") 


output_gdp_long_joined<-inner_join(output_gdp_long, output_long, by = c("country"="country", "year"="year"))
output_gdp_long_joined %<>% group_by(country) %>% mutate(
  abs_diff_gdpproj_prec = gdp_proj_prec[year==2080] - gdp_proj_prec[year==2010],
  grate_gdpproj_prec = (gdp_proj_prec[year==2080] - gdp_proj_prec[year==2010])/gdp_proj_prec[year==2010]
) %>% ungroup()


#### GDP pc proj temp + prec ####
output_gdp_t <- matrix(ncol=countries*model, nrow=years)

output_gdp_t[1,] <- d2010$mean_gdp

for (i in 1:length(d2010$country)){
  for (t in 1:70){
    if( is_empty(ssp_proj$pc_gdp_growth_ssp[ssp_proj$country_iso2==d2010$country[i]])==FALSE){ #ssp part
      
      if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) > 99){ #if upper bound agri emp reached
        
        delta = ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]]
        eta= (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*99*output[t+1,i]) + (-0.000916*99*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*99*output[1,i]) + (-0.000916*99*(output[1,i])^2)))
        tau =((0.0157*(output_t[t+1,i])-0.000759*(output_t[t+1,i])^2) - ((0.0157*(output_t[1,i])-0.000759*(output_t[1,i])^2)))
        output_gdp_t[t+1,i] = output_gdp_t[t,i] * (1+ delta + eta + tau)
        
      } else if(!is.na(d2010$agri_emp[i]+(t*d2010$agri_diff[i])) & d2010$agri_emp[i]+(t*d2010$agri_diff[i]) < 1){ #if lower bound agri emp reached
        
        delta = ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]]
        eta = (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*1*output[t+1,i]) + (-0.000916*1*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*1*output[1,i]) + (-0.000916*1*(output[1,i])^2)))
        tau =  ((0.0157*(output_t[t+1,i])-0.000759*(output_t[t+1,i])^2) - ((0.0157*(output_t[1,i])-0.000759*(output_t[1,i])^2)))
        output_gdp_t[t+1,i] = output_gdp_t[t,i] * (1+ delta + eta  + tau)
        
      }else{ #using projected agriemp with agri_diff
        
        delta = ssp_proj$pc_gdp_growth_ssp[ssp_proj$year==2010+t & ssp_proj$country_iso2==d2010$country[i]]
        eta = (((0*output[t+1,i]) + (0*(output[t+1,i])^2) + (0.00236*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*output[t+1,i]) + (-0.000916*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*(output[t+1,i])^2)) - ((0*output[1,i]) + (0*(output[1,i])^2) + (0.00236*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*output[1,i]) + (-0.000916*(d2010$agri_emp[i]+(t*d2010$agri_diff[i]))*(output[1,i])^2)))
        tau = ((0.0157*(output_t[t+1,i])-0.000759*(output_t[t+1,i])^2) - ((0.0157*(output_t[1,i])-0.000759*(output_t[1,i])^2)))
        output_gdp_t[t+1,i] = output_gdp_t[t,i] * (1+ delta + eta + tau)
        
      }}#}
    else{
      output_gdp_t[t+1,i] = NA}
  }}


colnames(output_gdp_t) <- d2010$country
rownames(output_gdp_t) <- 2010:2080

output_gdp_t <-data.frame(output_gdp_t)
year <- rownames(output_gdp_t)
data <- cbind(year,output_gdp_t)
output_gdp_long_t <- gather(data, country, gdp_proj_temp, "AE":"NA..15") 

output_gdp_long_joined_t<-inner_join(output_gdp_long_t, output_long_t, by = c("country"="country", "year"="year"))
output_gdp_long_joined_t %<>% group_by(country) %>% mutate(
  abs_diff_gdpproj_temp = gdp_proj_temp[year==2080] - gdp_proj_temp[year==2010],
  grate_gdpproj_temp = (gdp_proj_temp[year==2080] - gdp_proj_temp[year==2010])/gdp_proj_temp[year==2010]
) %>% ungroup()

output_gdp_long_joined_all <-inner_join(output_gdp_long_joined, output_gdp_long_joined_t, by = c("country"="country","year"="year"))

output_gdp_long_joined_all %<>% select(-proj_prec, -initial_prec, -final_prec, -abs_diff_proj_prec, -grate_proj_prec, -proj_temp, -initial_temp, -final_temp, -abs_diff_proj_temp, -grate_proj_temp)
long_data_proj <-inner_join(output_b_long_joined, output_gdp_long_joined_all, by = c("country"="country","year"="year"))

##

long_data_proj%<>%  mutate(tot_eff_b50=grate_b50proj_prec,
                           tot_eff_gdp=grate_gdpproj_temp)  

#division top 20 bottom 80
d2010 %<>% mutate(top20_string=case_when(
  agri_top20==1 ~ "Top 20%",
  TRUE ~ "Bottom 80%"
))


agri_q<- d2010%>% select(agri_top20,agri_b80,top20_string)


#### Country medians and means ####
short_data_proj<-long_data_proj%>% filter(year==2010)
a<- seq(1, length(d2010$country)-15, 16)
a<-a[-1]
short_data_proj$mean_gdp_grate_prec <- 0
short_data_proj$med_gdp_grate_prec <- 0

short_data_proj$mean_b50_grate_prec<-0
short_data_proj$med_b50_grate_prec<-0


short_data_proj$mean_gdp_grate_temp <- 0
short_data_proj$med_gdp_grate_temp <- 0


short_data_proj$mean_gdp_grate_tot <- 0
short_data_proj$med_gdp_grate_tot <- 0



short_data_proj$mean_gdp_grate_prec[1:(1+15)] <- mean(short_data_proj$grate_gdpproj_prec[1:(1+15)],na.rm=T)
short_data_proj$med_gdp_grate_prec[1:(1+15)] <- median(short_data_proj$grate_gdpproj_prec[1:(1+15)],na.rm=T)

short_data_proj$mean_b50_grate_prec[1:(1+15)] <- mean(short_data_proj$grate_b50proj_prec[1:(1+15)],na.rm=T)
short_data_proj$med_b50_grate_prec[1:(1+15)] <- median(short_data_proj$grate_b50proj_prec[1:(1+15)],na.rm=T)

short_data_proj$mean_gdp_grate_temp[1:(1+15)] <- mean(short_data_proj$grate_gdpproj_temp[1:(1+15)],na.rm=T)
short_data_proj$med_gdp_grate_temp[1:(1+15)] <- median(short_data_proj$grate_gdpproj_temp[1:(1+15)],na.rm=T)

short_data_proj$mean_gdp_grate_tot[1:(1+15)] <- mean(short_data_proj$tot_eff_gdp[1:(1+15)],na.rm=T)
short_data_proj$med_gdp_grate_tot[1:(1+15)] <- median(short_data_proj$tot_eff_gdp[1:(1+15)],na.rm=T)

for (i in a) {
  short_data_proj$mean_gdp_grate_prec[i:(i+15)] <- mean(short_data_proj$grate_gdpproj_prec[i:(i+15)],na.rm=T)
  short_data_proj$med_gdp_grate_prec[i:(i+15)] <- median(short_data_proj$grate_gdpproj_prec[i:(i+15)],na.rm=T)
  
  short_data_proj$mean_b50_grate_prec[i:(i+15)] <- mean(short_data_proj$grate_b50proj_prec[i:(i+15)],na.rm=T)
  short_data_proj$med_b50_grate_prec[i:(i+15)] <- median(short_data_proj$grate_b50proj_prec[i:(i+15)],na.rm=T)
  
  short_data_proj$mean_gdp_grate_temp[i:(i+15)] <- mean(short_data_proj$grate_gdpproj_temp[i:(i+15)],na.rm=T)
  short_data_proj$med_gdp_grate_temp[i:(i+15)] <- median(short_data_proj$grate_gdpproj_temp[i:(i+15)],na.rm=T)
  
  short_data_proj$mean_gdp_grate_tot[i:(i+15)] <- mean(short_data_proj$tot_eff_gdp[i:(i+15)],na.rm=T)
  short_data_proj$med_gdp_grate_tot[i:(i+15)] <- median(short_data_proj$tot_eff_gdp[i:(i+15)],na.rm=T)
  
  
  
}

short_data_proj %<>% mutate(
  quadrant_med_prec=  case_when(
    med_gdp_grate_prec>0 & med_b50_grate_prec>0 ~ 1,
    med_gdp_grate_prec>0 & med_b50_grate_prec<0 ~ 4,
    med_gdp_grate_prec<0 & med_b50_grate_prec<0 ~ 3,
    med_gdp_grate_prec<0 & med_b50_grate_prec>0 ~ 2
  ),
  quadrant_mean_prec=  case_when(
    mean_gdp_grate_prec>0 & mean_b50_grate_prec>0 ~ 1,
    mean_gdp_grate_prec>0 & mean_b50_grate_prec<0 ~ 4,
    mean_gdp_grate_prec<0 & mean_b50_grate_prec<0 ~ 3,
    mean_gdp_grate_prec<0 & mean_b50_grate_prec>0 ~ 2
  )
) 

short_data_proj$country[short_data_proj$country=="NA."]<-"NA"

short_data_proj %<>% 
select(country, grate_b50proj_prec,grate_gdpproj_prec,grate_gdpproj_temp,tot_eff_gdp,mean_b50_grate_prec,mean_gdp_grate_prec,mean_gdp_grate_temp,mean_gdp_grate_tot)

short_data_proj %<>% 
rename(impact_prec_b50=grate_b50proj_prec,
       impact_prec_gdp=grate_gdpproj_prec,
       impact_temp_gdp=grate_gdpproj_temp,
       impact_prec_and_temp_gdp=tot_eff_gdp,
       mean_impact_b50=mean_b50_grate_prec,
       mean_impact_prec_gdp=mean_gdp_grate_prec,
       mean_impact_temp_gdp=mean_gdp_grate_temp,
       mean_impact_prec_and_temp_gdp=mean_gdp_grate_tot)


write_csv(short_data_proj, paste0("data_revision/ssp",ssps[i_ssp],"_rcp",rcps[i_rcp],".csv"))



