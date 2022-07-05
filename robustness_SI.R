rm(list = ls())

library("readxl")
library(readr)
library(tidyverse)
library(magrittr)
library(plm)
require(lmtest)
library(wid)
library(ISOcodes)
library(stargazer)
library(countrycode)
library(haven)
library(gridExtra)


#creation of variables trend variables 

data_bottom <- read_csv("data/data_bottom_different_q.csv")
data_bottom$country[is.na(data_bottom$country)]<-"NA"


small<-data_bottom %>% select(gdpCAPppp_wdi,growthWDI,AgrGDPgrowthCap,UDel_temp_popweight,UDel_precip_popweight, bottom50,avg_agri_emp)
small<-as.data.frame(small)
desc_stat<-summary(small)
#stargazer(small)

#drop two countries for which temp and prec are missing RO and CD
data_bottom %<>% filter(country!="RO"& country!="CD")


data_bottom %<>% filter(year>1979)
#data_bottom %<>% filter(year>1989)  #ok robust




country_iso2<-c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
                "BN", "BO", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF",
                "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", "CV", "CY",
                "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "ER",
                "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", "GD", "GE", "GH",
                "GL", "GM", "GN", "GQ", "GR", "GT", "GW", "GY", "HK", "HN", "HR",
                "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IR", "IS", "IT",
                "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KR",
                "KW", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU",
                "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MN",
                "MO", "MR", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE",
                "NG", "NI", "NL", "NO", "NP", "NZ", "OM", "PA", "PE", "PG", "PH",
                "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RU",
                "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK", "SL", "SM",
                "SN", "SO", "SR", "ST", "SV", "SY", "SZ", "TD", "TG", "TH", "TJ", "TL",
                "TM", "TN", "TO", "TR", "TT", "TV", "TZ", "UA", "UG", "US", "UY",
                "UZ", "VC", "VE", "VN", "VU", "WS", "YE", "ZA", "ZM", "ZW")

country_region <- countrycode(c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
                                "BN", "BO", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF",
                                "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", "CV", "CY",
                                "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "ER",
                                "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", "GD", "GE", "GH",
                                "GL", "GM", "GN", "GQ", "GR", "GT", "GW", "GY", "HK", "HN", "HR",
                                "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IR", "IS", "IT",
                                "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KR",
                                "KW", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU",
                                "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MN",
                                "MO", "MR", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE",
                                "NG", "NI", "NL", "NO", "NP", "NZ", "OM", "PA", "PE", "PG", "PH",
                                "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RU",
                                "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK", "SL", "SM",
                                "SN", "SO", "SR", "ST", "SV", "SY", "SZ", "TD", "TG", "TH", "TJ", "TL",
                                "TM", "TN", "TO", "TR", "TT", "TV", "TZ", "UA", "UG", "US", "UY",
                                "UZ", "VC", "VE", "VN", "VU", "WS", "YE", "ZA", "ZM", "ZW"), origin='iso2c', destination='region23')
#KS (Republic of Corea) and XI (indigoland) excluded

corr_region_iso2 <- cbind(country_region, country_iso2)
corr_region_iso2 <- as.data.frame(corr_region_iso2) 

data_b_reg <- left_join(data_bottom, corr_region_iso2[,c("country_region","country_iso2" )], by= c("country" = 'country_iso2'))
#left join in order to not lose country "NA" 
data_b_reg$country[is.na(data_b_reg$country)]<-"NA" #Include Namibia
data_bottom<-data_b_reg

data_bottom%<>%mutate(continent_ssaf=case_when(
    country_region=="Eastern Africa" ~ "EAF",
    country_region=="Western Africa" ~ "WAF",
    country_region=="Middle Africa" ~ "MA",
    country_region=="Southern Africa" ~ "SAF",
    country=="SD"~ "EAF", #Sudan is Eastern Africa
    TRUE ~ continent))

for(i in unique(data_bottom$continent_ssaf)){
    for(j in unique(data_bottom$year)){
        data_bottom[[paste("y", j, i, sep = "_")]] <- ifelse(data_bottom$continent_ssaf==i, data_bottom$year==j, 0)
    }}

#write_csv(data_bottom, "data_djo_ssaf.csv")


long_form_y <- function(data, orig_form){
    add_var <- colnames(data)[grepl("^y_",colnames(data))]
    add_str <- paste(add_var,collapse=" + ")
    fin_form <- paste(orig_form, add_str, sep = " + ")
    return(fin_form)
}

data_bottom %<>% select(-y_1990_MENA) 
data<-data_bottom
bottom_panel <- pdata.frame(data_bottom, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2 + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2"
plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
djo_se <- coeftest(djo,c * vcovHC(djo, type = "HC2", cluster = "group"))





#### Robustness Agri Quantiles ####
#robustness 25 vs the rest

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b75*UDel_temp_popweight + agri_b75*temp_sq + agri_b75*UDel_precip_popweight + agri_b75*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top25


#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_top25,c * vcovHC(djo_top25, type = "HC2", cluster = "group")) -> djo_top25_se


orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_top25*UDel_temp_popweight + agri_top25*temp_sq + agri_top25*UDel_precip_popweight + agri_top25*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_b75

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_b75,c * vcovHC(djo_b75, type = "HC2", cluster = "group")) -> djo_b75_se

#stargazer(djo_top25_se, djo_b75_se, title="Results", align=TRUE)




#15-85
data_bottom %<>% group_by(year) %>%
    mutate(
        agri_top15 = as.numeric(ntile(avg_agri_emp, 100)>84),
        agri_b85 = as.numeric(ntile(avg_agri_emp, 100)<85)                                                               
    ) %>% ungroup()


data_bottom %<>% group_by(country)  %>% mutate( 
    agri_top15 = agri_top15[year==2007],
    agri_b85 = agri_b85[year==2007]
) %>%  ungroup()

bottom_panel <- pdata.frame(data_bottom, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b85*UDel_temp_popweight + agri_b85*temp_sq + agri_b85*UDel_precip_popweight + agri_b85*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top15

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_top15,c * vcovHC(djo_top25, type = "HC2", cluster = "group")) -> djo_top15_se

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_top15*UDel_temp_popweight + agri_top15*temp_sq + agri_top15*UDel_precip_popweight + agri_top15*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_b85

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_b85,c * vcovHC(djo_b85, type = "HC2", cluster = "group")) -> djo_b85_se





#30-70
data_bottom %<>% group_by(year) %>%
    mutate(
        agri_top30 = as.numeric(ntile(avg_agri_emp, 100)>69),
        agri_b70 = as.numeric(ntile(avg_agri_emp, 100)<70)                                                               
    ) %>% ungroup()


data_bottom %<>% group_by(country)  %>% mutate( 
    agri_top30 = agri_top30[year==2007],
    agri_b70 = agri_b70[year==2007]
) %>%  ungroup()


bottom_panel <- pdata.frame(data_bottom, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b70*UDel_temp_popweight + agri_b70*temp_sq + agri_b70*UDel_precip_popweight + agri_b70*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top30

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_top30,c * vcovHC(djo_top30, type = "HC2", cluster = "group")) -> djo_top30_se



orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_top30*UDel_temp_popweight + agri_top30*temp_sq + agri_top30*UDel_precip_popweight + agri_top30*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_b70

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_b70,c * vcovHC(djo_b70, type = "HC2", cluster = "group")) -> djo_b70_se



#stargazer(djo_top25_se, djo_b75_se, djo_top30_se, djo_b70_se, align=TRUE)


#### Rob Excluding Tops ####
#Excluding 10% agri

data_bottom %<>% group_by(year) %>%
    mutate(
        agri_top10 = as.numeric(ntile(avg_agri_emp, 100)>89)                                                             
    ) %>% ungroup()


data_bottom %<>% group_by(country)  %>% mutate( 
    agri_top10 = agri_top10[year==2007]
) %>%  ungroup()
data_no_top10 <- data_bottom %>% filter(agri_top10==0)
data<-data_no_top10


bottom_panel <- pdata.frame(data_no_top10, index=c("country","year"), drop.index=TRUE, row.names=TRUE)
#NOW TOP 2O IS TOP10 AND B80 IS B90

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top10_notop10

#correction to get Stata robust standard errors
G <- length(unique(data_no_top10$country))
c <- G/(G - 1)
coeftest(djo_top10_notop10,c * vcovHC(djo_top10_notop10, type = "HC2", cluster = "group")) -> djo_top10_notop10_se

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b75*UDel_temp_popweight + agri_b75*temp_sq + agri_b75*UDel_precip_popweight + agri_b75*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top15_notop10

#correction to get Stata robust standard errors
G <- length(unique(data_no_top10$country))
c <- G/(G - 1)
coeftest(djo_top15_notop10,c * vcovHC(djo_top15_notop10, type = "HC2", cluster = "group")) -> djo_top15_notop10_se


orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b70*UDel_temp_popweight + agri_b70*temp_sq + agri_b70*UDel_precip_popweight + agri_b70*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_top20_notop10

#correction to get Stata robust standard errors
G <- length(unique(data_no_top10$country))
c <- G/(G - 1)
coeftest(djo_top20_notop10,c * vcovHC(djo_top20_notop10, type = "HC2", cluster = "group")) -> djo_top20_notop10_se

#write for margins in stata
#write.csv(data_no_top10, "data_no_top10.csv")


#stargazer(djo_top10_notop10_se, djo_top15_notop10_se, djo_top20_notop10_se, align=TRUE)



#### Top10/Top50 as dependent variable ####



data_top10_income <- read_xlsx("data/full_data_percent1.xlsx")

data_top10_income %<>% select(year, country, p90p100)

data_bottom_top <- inner_join(data_bottom, data_top10_income, by = c("country"="country", "year"="year")) 


data_bottom_top %<>% mutate(
    top10_on_b50 = p90p100/bottom50
)


bottom_panel <- pdata.frame(data_bottom_top, index=c("country","year"), drop.index=TRUE, row.names=TRUE)



# Ratio Top10/Bottom 50 as dependent variable


#AS BM TOP 20 agri VS REST

#BASELINE
data<-data_bottom_top
orig_form<- "top10_on_b50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> t10b50_djo_top20

#correction to get Stata robust standard errors
G <- length(unique(data_bottom_top$country))
c <- G/(G - 1)
coeftest(t10b50_djo_top20,c * vcovHC(t10b50_djo_top20, type = "HC2", cluster = "group")) -> t10b50_djo_top20_se


#top10 as dependent variable
orig_form<- "p90p100 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> t10_djo_top20

#correction to get Stata robust standard errors
G <- length(unique(data_bottom_top$country))
c <- G/(G - 1)
coeftest(t10_djo_top20,c * vcovHC(t10_djo_top20, type = "HC2", cluster = "group")) -> t10_djo_top20_se


#### Controlling for GDP pc growth ####
bottom_panel <- pdata.frame(data_bottom, index=c("country","year"), drop.index=TRUE, row.names=TRUE)
data<-data_bottom
#top 20 
orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2 + growthWDI"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_growth_top20
summary(djo_growth_top20) 

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_growth_top20,c * vcovHC(djo_growth_top20, type = "HC2", cluster = "group")) -> djo_growth_top20_se

#top 20 gdp
orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2 + gdpCAP_wdi"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel,
    effect = c("individual"),
    model = "within"
) -> djo_gdp_top20

#correction to get Stata robust standard errors
G <- length(unique(data_bottom$country))
c <- G/(G - 1)
coeftest(djo_gdp_top20,c * vcovHC(djo_gdp_top20, type = "HC2", cluster = "group")) -> djo_gdp_top20_se



#### Pretis data - robustness####

data_pretis <- read_csv("data/PSTHA_2018_rep_data.csv")

#UDel_mean is mean temp in a year, UDel_prcp_mean is mean prec in a year. 

just_share_to_join <- data_bottom %>% select(country_iso3, continent_ssaf, year, bottom50, agri_top20, agri_b80)
data_pretis_bottom <- inner_join(just_share_to_join, data_pretis, by = c("country_iso3"="iso", "year"="year")) 

#transforming dm in metres
data_pretis_bottom %<>% mutate(
    UDel_prcp_max=UDel_prcp_max/10,
    UDel_prcp_min=UDel_prcp_min/10,
    UDel_prcp_mean=UDel_prcp_mean/10,
    UDel_prcp_mean_2 = UDel_prcp_mean^2
)

for(i in unique(data_pretis_bottom$continent_ssaf)){
    for(j in unique(data_pretis_bottom$year)){
        data_pretis_bottom[[paste("y", j, i, sep = "_")]] <- ifelse(data_pretis_bottom$continent_ssaf==i, data_pretis_bottom$year==j, 0)
    }}
data_pretis_bottom %<>% select(-y_1990_MENA) 

panel_pretis <- pdata.frame(data_pretis_bottom, index=c("country_iso3","year"), drop.index=TRUE, row.names=TRUE)
data<-panel_pretis



#top 20 
orig_form<- "bottom50 ~  UDel_mean  + UDel_mean_2 + UDel_prcp_mean + UDel_prcp_mean_2 + agri_b80*UDel_mean + agri_b80*UDel_mean_2 + agri_b80*UDel_prcp_mean + agri_b80*UDel_prcp_mean_2"

plm(
    formula = long_form_y(data,orig_form),
    data = panel_pretis,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_pretis_top20

#correction to get Stata robust standard errors
G <- length(unique(data_pretis_bottom$country_iso3))
c <- G/(G - 1)
coeftest(djo_bottom_pretis_top20,c * vcovHC(djo_bottom_pretis_top20, type = "HC2", cluster = "group")) -> djo_bottom_pretis_top20_se



#prec and temp variability, min and max
orig_form<- "bottom50 ~  UDel_mean  + UDel_mean_2 + UDel_prcp_mean + UDel_prcp_mean_2 + UDel_var  + UDel_prcp_var + UDel_min  + UDel_prcp_min + UDel_max  + UDel_prcp_max + agri_b80*UDel_mean + agri_b80*UDel_mean_2 + agri_b80*UDel_prcp_mean + agri_b80*UDel_prcp_mean_2 + agri_b80*UDel_var  + agri_b80*UDel_prcp_var + agri_b80*UDel_min  + agri_b80*UDel_prcp_min + agri_b80*UDel_max  + agri_b80*UDel_prcp_max"

plm(formula = long_form_y(data,orig_form),
    data = panel_pretis,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_pretis_all_top20

#correction to get Stata robust standard errors
G <- length(unique(data_pretis_bottom$country_iso3))
c <- G/(G - 1)
coeftest(djo_bottom_pretis_all_top20,c * vcovHC(djo_bottom_pretis_all_top20, type = "HC2", cluster = "group")) -> djo_bottom_pretis_all_top20_se







#prec and temp min and max - NO Variability 
orig_form<- "bottom50 ~  UDel_mean  + UDel_mean_2 + UDel_prcp_mean + UDel_prcp_mean_2  + UDel_min  + UDel_prcp_min + UDel_max  + UDel_prcp_max + agri_b80*UDel_mean + agri_b80*UDel_mean_2 + agri_b80*UDel_prcp_mean + agri_b80*UDel_prcp_mean_2 + agri_b80*UDel_min  + agri_b80*UDel_prcp_min + agri_b80*UDel_max  + agri_b80*UDel_prcp_max"

plm(
    formula = long_form_y(data,orig_form),
    data = panel_pretis,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_pretis_novar_top20

#correction to get Stata robust standard errors
G <- length(unique(data_pretis_bottom$country_iso3))
c <- G/(G - 1)
coeftest(djo_bottom_pretis_novar_top20,c * vcovHC(djo_bottom_pretis_novar_top20, type = "HC2", cluster = "group")) -> djo_bottom_pretis_novar_top20_se


#Uncomment for SI Appendix, Table S8
#stargazer(djo_bottom_pretis_top20_se, djo_bottom_pretis_all_top20_se, djo_bottom_pretis_novar_top20_se, title="Results with Pretis data", align=TRUE, report=('vc*p'))

#stargazer(djo_bottom_pretis_top20_se, djo_bottom_pretis_all_top20_se, djo_bottom_pretis_novar_top20_se, title="Results with Pretis data", align=TRUE ,report = "vc*t")





####  WB Income Classes ####

income_class <- read_excel("data/income_CLASS.xls")

#gini_class <- inner_join(data_gini_tidy, income_class, by = c("iso"="Code"))
bottom50_class <- inner_join(data_bottom, income_class, by = c("country_iso3"="Code"))





bottom50_class %<>% mutate(
    dummy_low=case_when(
        `Income group`=="Low income" ~ 1,
        TRUE ~ 0    
    ),
    dummy_lower_middle=case_when(
         `Income group`=="Lower middle income" ~ 1,
        TRUE ~ 0    
    ),
    dummy_upper_middle=case_when(
        `Income group`=="Upper middle income" ~ 1,
        TRUE ~ 0     
    ),
    dummy_high=case_when(
        `Income group`=="High income" ~ 1,
        TRUE ~ 0     
    )
)


bottom50_class %<>% mutate(
    dummy_from_midlow_to_high=case_when(
        dummy_lower_middle==1 ~ 1,
        dummy_upper_middle==1 ~ 1,
        dummy_high==1 ~ 1,
        TRUE ~ 0
    ))
for(i in unique(bottom50_class$continent_ssaf)){
    for(j in unique(bottom50_class$year)){
        bottom50_class[[paste("y", j, i, sep = "_")]] <- ifelse(bottom50_class$continent_ssaf==i, bottom50_class$year==j, 0)
    }}
bottom50_class %<>% select(-y_1990_MENA) 

bottom_panel_class <- pdata.frame(bottom50_class, index=c("country","year"), drop.index=TRUE, row.names=TRUE)
data<-bottom50_class

orig_form<- " bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + dummy_from_midlow_to_high*UDel_temp_popweight + dummy_from_midlow_to_high*temp_sq + dummy_from_midlow_to_high*UDel_precip_popweight + dummy_from_midlow_to_high*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel_class,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_model_classes

#correction to get Stata robust standard errors
G <- length(unique(bottom50_class$country))
c <- G/(G - 1)
coeftest(djo_bottom_model_classes,c * vcovHC(djo_bottom_model_classes, type = "HC2", cluster = "group")) -> djo_bottom_model_classes_se

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + dummy_low*UDel_temp_popweight + dummy_low*temp_sq + dummy_low*UDel_precip_popweight + dummy_low*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel_class,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_model_classes_mid_high

#correction to get Stata robust standard errors
G <- length(unique(bottom50_class$country))
c <- G/(G - 1)
coeftest(djo_bottom_model_classes_mid_high,c * vcovHC(djo_bottom_model_classes_mid_high, type = "HC2", cluster = "group")) -> djo_bottom_model_classes_mid_high_se




bottom50_class %<>% mutate(
    dummy_not_SSAF=case_when(
        continent=="SSAF" ~ 0,
        TRUE ~ 1
    )) 
bottom_panel_class <- pdata.frame(bottom50_class, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

orig_form<- "bottom50 ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2  + dummy_not_SSAF*UDel_temp_popweight + dummy_not_SSAF*temp_sq + dummy_not_SSAF*UDel_precip_popweight + dummy_not_SSAF*UDel_precip_popweight_2"

plm(
    formula = long_form_y(data,orig_form),
    data = bottom_panel_class,
    effect = c("individual"),
    model = "within"
) -> djo_bottom_model_SSAF

#correction to get Stata robust standard errors
G <- length(unique(bottom50_class$country))
c <- G/(G - 1)
coeftest(djo_bottom_model_SSAF,c * vcovHC(djo_bottom_model_SSAF, type = "HC2", cluster = "group")) -> djo_bottom_model_SSAF_se


#Uncomment for SI Appendix, Table S10
#stargazer(djo_bottom_model_classes_se,djo_bottom_model_SSAF_se,align=T,report = "vc*t")



#### Rob Gini ####
data_gini<- read_csv("data/data_gini.csv")
data_gini %<>% group_by(year) %>%
    mutate(
        agri_top20 = as.numeric(ntile(avg_agri_emp, 5)==5),
        agri_b80 = as.numeric(ntile(avg_agri_emp, 5)<5)                                                               
    ) %>% ungroup()


data_gini %<>% group_by(country)  %>% mutate( 
    agri_top20 = agri_top20[year==2007],
    agri_b80 = agri_b80[year==2007]
) %>%  ungroup()

country_iso3 <- countrycode(c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
                              "BN", "BO", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF",
                              "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", "CV", "CY",
                              "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "ER",
                              "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", "GD", "GE", "GH",
                              "GL", "GM", "GN", "GQ", "GR", "GT", "GW", "GY", "HK", "HN", "HR",
                              "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IR", "IS", "IT",
                              "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KR",
                              "KW", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU",
                              "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MN",
                              "MO", "MR", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE",
                              "NG", "NI", "NL", "NO", "NP", "NZ", "OM", "PA", "PE", "PG", "PH",
                              "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RU",
                              "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK", "SL", "SM",
                              "SN", "SO", "SR", "ST", "SV", "SY", "SZ", "TD", "TG", "TH", "TJ", "TL",
                              "TM", "TN", "TO", "TR", "TT", "TV", "TZ", "UA", "UG", "US", "UY",
                              "UZ", "VC", "VE", "VN", "VU", "WS", "YE", "ZA", "ZM", "ZW"), origin='iso2c', destination='iso3c')
country_iso3 <- cbind(country_iso3, country_iso2)
country_iso3<-as.data.frame(country_iso3)
data_gini_reg <- left_join(data_gini, country_iso3, by= c("iso" = 'country_iso3'))
data_gini_reg <- left_join(data_gini_reg, corr_region_iso2[,c("country_region","country_iso2" )], by= c("country_iso2" = 'country_iso2'))


#left join in order to not lose country "NA" 
data_gini_reg$country[is.na(data_gini_reg$country)]<-"NA" #include Namibia
data_gini<-data_gini_reg

data_gini%<>%mutate(continent_ssaf=case_when(
    country_region=="Eastern Africa" ~ "EAF",
    country_region=="Western Africa" ~ "WAF",
    country_region=="Middle Africa" ~ "MA",
    country_region=="Southern Africa" ~ "SAF",
    country=="SD"~ "EAF", #Sudan is Eastern Africa
    TRUE ~ continent))


#data_gini%<>%filter(year>1979)
for(i in unique(data_gini$continent_ssaf)){
    for(j in unique(data_gini$year)){
        data_gini[[paste("y", j, i, sep = "_")]] <- ifelse(data_gini$continent_ssaf==i, data_gini$year==j, 0)
    }}
data_gini %<>% select(-y_1990_MENA) 
data<-data_gini
gini_panel <- pdata.frame(data_gini, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

orig_form<- "gini_mkt ~  UDel_temp_popweight  + temp_sq + UDel_precip_popweight + UDel_precip_popweight_2 + agri_b80*UDel_temp_popweight + agri_b80*temp_sq + agri_b80*UDel_precip_popweight + agri_b80*UDel_precip_popweight_2"
plm(
    formula = long_form_y(data,orig_form),
    data = gini_panel,
    effect = c("individual"),
    model = "within"
) -> gini_djo

#correction to get Stata robust standard errors
G <- length(unique(data_gini$country))
c <- G/(G - 1)
coeftest(gini_djo,c * vcovHC(gini_djo, type = "HC2", cluster = "group")) -> gini_djo_se

#stargazer(bottom_top20_se_nogdp, gini_emp_se_fix, title="Results", align=TRUE)


#Uncomment for SI Appendix, Table S9
#stargazer(t10b50_djo_top20_se,t10_djo_top20_se ,gini_djo_se, align=TRUE)
#stargazer(t10b50_djo_top20_se,t10_djo_top20_se ,gini_djo_se, align=TRUE,report = "vc*t")



#### correlations for Supplementary Information ####
data<-data_bottom

data %<>% mutate(agr_capgdp_ppp= ((gdpCAPppp_wdi*Pop)*(AgrGDP/TotGDP))/Pop)

data_top20<-data%>%filter(agri_top20==1)
cor.test(data_top20$bottom50, data_top20$gdpCAPppp_wdi, use="complete.obs")
cor.test(data_top20$bottom50, data_top20$agr_capgdp_ppp, use="complete.obs")

data_b80<-data%>%filter(agri_top20==0)

cor.test(data_b80$bottom50, data_b80$gdpCAPppp_wdi, use="complete.obs")
cor.test(data_b80$bottom50, data_b80$agr_capgdp_ppp, use="complete.obs")


#employment in agri approximated by percentage of pop 

emp_top20 <- data %>% filter(agri_top20==1) %>% mutate(
  emp_pop=(agri_emp/100)*Pop,
  agr_gdp_emp=AgrGDP/emp_pop,
  agr_gdp_emp_ppp=agr_capgdp_ppp*Pop/emp_pop
)
emp_top20 %<>% select(country, year, agr_gdp_emp, gdpCAP_wdi, gdpCAPppp_wdi,agr_gdp_emp_ppp)    

emp_top20 %<>% mutate(
  comparison_agrppp= case_when(
    agr_gdp_emp_ppp<gdpCAPppp_wdi ~ 1,
    agr_gdp_emp_ppp>gdpCAPppp_wdi ~ 0 # TRUE ~ 0
  )
)

table(emp_top20$comparison_agrppp) #in top 20% agri countries, 100% of observations have GDP per capita >Agri GDP per agricultural worker


emp_b80 <- data %>% filter(agri_top20==0) %>% mutate(
  emp_pop=(agri_emp/100)*Pop,
  agr_gdp_emp=AgrGDP/emp_pop,
  agr_gdp_emp_ppp=agr_capgdp_ppp*Pop/emp_pop
)
emp_b80 %<>% select(country, year, agr_gdp_emp, gdpCAP_wdi, gdpCAPppp_wdi,agr_gdp_emp_ppp)    


emp_b80 %<>% mutate(
  comparison_agrppp= case_when(
    agr_gdp_emp_ppp<gdpCAPppp_wdi ~ 1,
    agr_gdp_emp_ppp>gdpCAPppp_wdi ~ 0 
  )
)

table(emp_b80$comparison_agrppp) #in bottom 80% agri countries, 97% of observations have GDP per capita >Agri GDP per agricultural worker



check_mena<- data%>%filter(continent=="MENA")

#cor.test(check_mena$bottom50, check_mena$gdpCAPppp_wdi, use="complete.obs")
#cor.test(check_mena$bottom50, check_mena$agr_capgdp_ppp, use="complete.obs")


check_mena%<>%mutate(
  emp_pop=(agri_emp/100)*Pop,
  agr_gdp_emp=AgrGDP/emp_pop,
  agr_gdp_emp_ppp=agr_capgdp_ppp*Pop/emp_pop
)

check_mena%<>%mutate(check_ratio=agr_gdp_emp_ppp/gdpCAPppp_wdi)
emp_b80%<>%mutate(check_ratio=agr_gdp_emp_ppp/gdpCAPppp_wdi)
emp_top20%<>%mutate(check_ratio=agr_gdp_emp_ppp/gdpCAPppp_wdi)

summary(check_mena$check_ratio)

summary(emp_b80$check_ratio)

summary(emp_top20$check_ratio)
