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

split.wid <- T


    ### Global - country-level ###
    #  Data: GINI #
    # merge
    
    swiid_summary <- read_csv("data/swiid8_2_summary.csv") # swiid gini data

    bhm <- read_csv("data/bhm.csv") #temperature and precipitation dataset

    bhm %>% mutate(
        countryname = case_when(
            countryname=="Bahamas, The" ~ "Bahamas",
            countryname=="Brunei Darussalam" ~ "Brunei", 
            countryname=="Cabo Verde" ~ "Cape Verde",
            countryname=="Congo, Rep." ~ "Congo-Brazzaville",
            countryname=="Congo, Dem. Rep." ~ "Congo-Kinshasa",
            countryname==  "Cote d'Ivoire" ~ "Côte d'Ivoire",
            countryname=="Egypt, Arab Rep." ~ "Egypt",
            countryname=="Gambia, The" ~"Gambia",
            countryname=="Hong Kong SAR, China" ~ "Hong Kong",
            countryname=="Iran, Islamic Rep." ~ "Iran",
            countryname== "Korea, Rep." ~ "Korea",
            countryname=="Kyrgyz Republic" ~ "Kyrgyzstan",
            countryname=="Lao PDR" ~ "Laos",
            countryname== "Macedonia, FYR" ~ "Macedonia",
            countryname=="Micronesia, Fed. Sts." ~ "Micronesia",
            countryname=="Russian Federation" ~ "Russia",
            countryname=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
            countryname=="Slovak Republic" ~ "Slovakia",
            countryname=="St. Vincent and the Grenadines" ~ "St. Vincent and Grenadines",
            countryname=="Syrian Arab Republic" ~ "Syria",
            countryname=="Venezuela, RB"  ~ "Venezuela",
            countryname=="Yemen, Rep." ~ "Yemen",
            TRUE ~ countryname
        )
    ) -> bhm_to_merge
    
    data_gini_climate <- inner_join(swiid_summary, bhm_to_merge, by = c("country"="countryname", "year"="year")) #join roine with bhm
    
    # creation of variables 
    
    #colnames(data_gini_tidy)
    
    data_gini_tidy <- data_gini_climate[,-c(46:ncol(data_gini_climate))]
    
    data_gini_tidy %<>%  select(-time, -time2, -isocode, -iso_id)
    
    data_gini_tidy %<>% mutate( #add new variables
        temp_sq = UDel_temp_popweight^2, 
        time_trend = year-1959,
        time_trend_sq = time_trend^2,
        gdp_sq = gdpCAPppp^2,
        temp_third = UDel_temp_popweight^3,
        prec_third = UDel_precip_popweight^3,
        gdp_pc_non_ppp = TotGDP/Pop, # for check wrt M&M
        gdp_pc_non_ppp_sq = gdp_pc_non_ppp^2 # for check wrt M&M
    ) 
    
    
    
    ##################
    #  Data: BOTTOM50 #
    
    if(split.wid){
        countries_1 <- c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW",
                         "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
                         "BN", "BO", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF",
                         "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", "CV", "CY",
                         "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "ER",
                         "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", "GD", "GE", "GH",
                         "GL", "GM", "GN", "GQ", "GR", "GT", "GW", "GY", "HK", "HN", "HR",
                         "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IR", "IS", "IT",
                         "JM", "JO", "JP", "KE", "KG")
        
        countries_2 <- c("KH", "KI", "KM", "KN", "KR", "KS",
                         "KW", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU",
                         "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MN",
                         "MO", "MR", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE",
                         "NG", "NI", "NL", "NO", "NP", "NZ", "OM", "PA", "PE", "PG", "PH",
                         "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RU",
                         "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK", "SL", "SM",
                         "SN", "SO", "SR", "ST", "SV", "SY", "SZ", "TD", "TG", "TH", "TJ", "TL",
                         "TM", "TN", "TO", "TR", "TT", "TV", "TZ", "UA", "UG", #"US", 
                         "UY", "UZ", "VC", "VE", "VN", "VU", "WS", "XI", "YE", "ZA", "ZM", "ZW")
        
        data_wid_bottom50_1 <- download_wid(
            indicators = "sptinc", # Shares of pre-tax national income
            areas = countries_1,
            perc = c("p0p50"), # Top 1% and top 10%
            year = 1960:2020,
            pop = "j"
        )
        
        data_wid_bottom50_2 <- download_wid(
            indicators = "sptinc", # Shares of pre-tax national income
            areas = countries_2,
            perc = c("p0p50"), # Top 1% and top 10%
            year = 1960:2020,
            pop = "j"
        )
        
        data_wid_bottom50 <- rbind(data_wid_bottom50_1, data_wid_bottom50_2)
    }else{
        #Note: sometimes problems with all countries joint
        data_wid_bottom50 <- download_wid(
            indicators = "sptinc", # Shares of pre-tax national income
            areas = c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW",
                      "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
                      "BN", "BO", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF",
                      "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", "CV", "CY",
                      "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "ER",
                      "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", "GD", "GE", "GH",
                      "GL", "GM", "GN", "GQ", "GR", "GT", "GW", "GY", "HK", "HN", "HR",
                      "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IR", "IS", "IT",
                      "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KR", "KS",
                      "KW", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU",
                      "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MN",
                      "MO", "MR", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE",
                      "NG", "NI", "NL", "NO", "NP", "NZ", "OM", "PA", "PE", "PG", "PH",
                      "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RU",
                      "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK", "SL", "SM",
                      "SN", "SO", "SR", "ST", "SV", "SY", "SZ", "TD", "TG", "TH", "TJ", "TL",
                      "TM", "TN", "TO", "TR", "TT", "TV", "TZ", "UA", "UG", "US", "UY",
                      "UZ", "VC", "VE", "VN", "VU", "WS", "XI", "YE", "ZA", "ZM", "ZW"), 
            perc = c("p0p50"), # Top 1% and top 10%
            year = 1960:2020,
            pop = "j"
        )
    }
    
    # Wid data has ISO2, BHM ISO3
    
    country_iso2 <- c("AD", "AE", "AF", "AG", "AL", "AM", "AO", "AR", "AT", "AU", "AW", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM",
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
                                  "UZ", "VC", "VE", "VN", "VU", "WS", "YE", "ZA", "ZM", "ZW"), origin = 'iso2c', destination = 'iso3c')
    #KS (Republic of Corea) and XI (indigoland?) excluded
    
    corr_iso3_2 <- cbind(country_iso3, country_iso2)
    corr_iso3_2 <- as.data.frame(corr_iso3_2) 
    
    bottom50_to_join <- left_join(data_wid_bottom50, corr_iso3_2[,c("country_iso3","country_iso2" )], by= c("country" = 'country_iso2'))
    #left join in order to not lose country "NA" 
    #Solve problem of Namibia
    bottom50_to_join$country[is.na(bottom50_to_join$country)]<-"NA"
    bottom50_to_join$country_iso3[is.na(bottom50_to_join$country_iso3)]<-"NAM"
    bottom50_bhm <- inner_join(bottom50_to_join, bhm, by = c("country_iso3"="iso", "year"="year"))
    
    
    #colnames(bottom50_bhm)
    bottom50_bhm <- bottom50_bhm[,-c(40:ncol(bottom50_bhm))]
    bottom50_bhm %<>%  select(-time, -time2, -isocode, -iso_id)
    bottom50_bhm_mutated <- bottom50_bhm %>% 
        mutate( #add new variables
            bottom50 =value,
            temp_sq = UDel_temp_popweight^2,
            #time_trend = year- min(data_roine$year),
            # time_trend_sq = time_trend^2,
            gdp_sq = gdpCAPppp^2,
            temp_third= UDel_temp_popweight^3,
            prec_third= UDel_precip_popweight^3,
            temp_times_gdppc = UDel_temp_popweight*gdpCAPppp,
            prec_times_gdppc = UDel_precip_popweight*gdpCAPppp,
            time_trend = year-1959,
            time_trend_sq = time_trend^2
        ) 
    
    ###  Agricultural per capita employment shares ###
    
    #Quantiles defined on average employment shares
    
    data_emp <-  read_csv("data/employment_wb.csv", 
                          skip = 4) 
    
    # Data - Gini
    
    data_gini_tidy %<>% mutate(
        agri_share = AgrGDP/TotGDP
    )
    
    
    data_gini_tidy %<>% group_by(country) %>%
        mutate(
            avg_agri_share=mean(agri_share, na.rm=TRUE)
        )
    
    
    
    
    
    data_emp_long <- gather(data_emp, year, agri_emp, "1960":"2019")#, factor_key=TRUE)
    
    data_emp_long %<>% mutate(
        year= as.numeric(year)
    )
    
    data_gini_emp <- inner_join(data_gini_tidy, data_emp_long, by = c("iso"="Country Code", "year"="year"))
    
    data_gini_emp %<>% mutate(
        agri_emp= as.numeric(agri_emp)
    )
    
    data_gini_emp_avg <- data_gini_emp %>% mutate(
        avg_agri_emp= mean(agri_emp, na.rm=TRUE))
    
    data_gini_emp_avg %<>% group_by(year) %>%
        mutate(
            Above50 = as.numeric(ntile(avg_agri_emp, 2)==2),
            agri_bottom25 = as.numeric(ntile(avg_agri_emp, 4)==1),
            agri_25_50 = as.numeric(ntile(avg_agri_emp, 4)==2),                                   
            agri_50_75 = as.numeric(ntile(avg_agri_emp, 4)==3),
            agri_top25 = as.numeric(ntile(avg_agri_emp, 4)==4)                                    
        ) %>% ungroup()
    
    
    data_gini_emp_fixedq <- data_gini_emp_avg %>% group_by(country)  %>% mutate(
        min_year=min(year),
        int_2007= length(intersect(year,2007))) %>% ungroup()
    
    
    data_gini_emp_fixedq %<>% filter(int_2007== 1) %>% group_by(country)  %>% mutate( 
        agri_top50 = Above50[year==2007],
        agri_b25= agri_bottom25[year==2007],
        agri_m_25_50 = agri_25_50[year==2007],
        agri_m_50_75= agri_50_75[year==2007],
        agri_top_25= agri_top25[year==2007]
    ) %>%  ungroup()
    
    
    
    data_gini_emp_fixedq <- data_gini_emp_avg %>% group_by(country)  %>% mutate(
        min_year=min(year),
        int_2007= length(intersect(year,2007))) %>% ungroup()
    
    
    data_gini_emp_fixedq %<>% filter(int_2007== 1) %>% group_by(country)  %>% mutate( 
        agri_top50 = Above50[year==2007],
        agri_b25= agri_bottom25[year==2007],
        agri_m_25_50 = agri_25_50[year==2007],
        agri_m_50_75= agri_50_75[year==2007],
        agri_top_25= agri_top25[year==2007]
    ) %>%  ungroup()
    
    
    
    #Data - Bottom 50
    
    bottom50_bhm_mutated %<>% mutate(
        agri_share = AgrGDP/TotGDP
    )
    
    
    bottom50_bhm_mutated %<>% group_by(country) %>%
        mutate(
            avg_agri_share=mean(agri_share, na.rm=TRUE)
        )  
    
    
    data_bottom_emp <- inner_join(bottom50_bhm_mutated, data_emp_long, by = c("country_iso3"="Country Code", "year"="year"))
    
    data_bottom_emp %<>% mutate(
        agri_emp= as.numeric(agri_emp)
    )
    
    
    
    
    
    data_bottom_emp_avg <- data_bottom_emp %>% mutate(
        avg_agri_emp= mean(agri_emp, na.rm=TRUE))
    
    data_bottom_emp_avg %<>% group_by(year) %>%
        mutate(
            Above50 = as.numeric(ntile(avg_agri_emp, 2)==2),
            agri_bottom25 = as.numeric(ntile(avg_agri_emp, 4)==1),
            agri_25_50 = as.numeric(ntile(avg_agri_emp, 4)==2),                                   
            agri_50_75 = as.numeric(ntile(avg_agri_emp, 4)==3),
            agri_top25 = as.numeric(ntile(avg_agri_emp, 4)==4)                                    
        ) %>% ungroup()
    
    
    
    
    data_bottom_emp_fixedq <- data_bottom_emp_avg %>%  group_by(country)  %>% mutate(
        min_year=min(year),
        int_2007= length(intersect(year,2007))) %>% ungroup()   
    
    data_bottom_emp_fixedq %<>% filter(int_2007== 1) %>% group_by(country)  %>% mutate( 
        agri_top50 = Above50[year==2007],
        agri_b25= agri_bottom25[year==2007],
        agri_m_25_50 = agri_25_50[year==2007],
        agri_m_50_75= agri_50_75[year==2007],
        agri_top_25= agri_top25[year==2007]
    ) %>%  ungroup()
    
    
    ####
    #To have precipitations in metres divide by 1000
    
    
    data_gini_emp_fixedq %<>% mutate(
        UDel_precip_popweight=UDel_precip_popweight/1000,
        UDel_precip_popweight_2 =UDel_precip_popweight^2
    )
    
    data_bottom_emp_fixedq %<>% mutate(
        UDel_precip_popweight=UDel_precip_popweight/1000,
        UDel_precip_popweight_2 =UDel_precip_popweight^2
    )
    
    #rename variables to save
    data_bottom <- data_bottom_emp_fixedq
    data_gini   <- data_gini_emp_fixedq
    
    #I have commented saving since data are provided in data folder
    #save everything but
    #rm(list= ls()[!(ls() %in% c('data_bottom','data_gini'))])
    #save.image("data/data_all.RData")
    


