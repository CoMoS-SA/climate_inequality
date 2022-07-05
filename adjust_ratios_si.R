rm(list = ls())
library("readxl")
library(readr)
library(tidyverse)
library(magrittr)



check_long<-read_csv("data_revision/longproj_data_variaz_agriemp_bound_NOERROR_withvariazb50.csv") #longdataproj_NOERROR_withvariazb50.csv")

check_long_reduced<-check_long%>%filter(substr(country,1,2)=="CN"|substr(country,1,2)=="HU"|substr(country,1,2)=="IN"|substr(country,1,2)=="MK"|substr(country,1,2)=="PL"|
                                          substr(country,1,2)=="RS"|substr(country,1,2)=="RU"|substr(country,1,2)=="RW"|substr(country,1,2)=="ZA"|substr(country,1,2)=="SL")


vedi<-check_long_reduced%>%filter(b_proj==0|b_proj==0.5)


#vedi%<>%group_by(country)%>%mutate(min_year=min(year))%>%ungroup()
vedi%<>%group_by(substr(country,1,2))%>%mutate(min_year=min(year))%>%ungroup()


vedi%<>%select(country,min_year)%>%distinct(country,.keep_all = T)

check_long_reduced<-left_join(check_long_reduced,vedi,by=c("country"="country"))

#Hungary trend, b50 goes to 0 in 2080 only for some models

check_long_reduced%<>%mutate(min_year=case_when(
  substr(country,1,2)=="HU" ~ 2080,
  TRUE  ~ min_year))

check_long_reduced%<>%group_by(country)%>%mutate(level_last_b50=b_proj[year==(min_year-1)])%>%ungroup()

small_data<-check_long_reduced%>%distinct(country,.keep_all=T)%>%select(country,min_year,level_last_b50)


allproj<-read_csv("data_revision/data_scenarios_projections_withSSP_withvariazb50.csv")

allproj_join<-allproj%>%select(country,initial_b50, b50_diff)

comparison<- inner_join(small_data, allproj_join,by=c("country"="country"))

comparison%<>%mutate(country2=substr(country,1,2))
comparison%<>%group_by(country2)%>%mutate(b50_diff2=b50_diff[!is.na(b50_diff)])%>%ungroup()

comparison%<>%mutate(trend_last_b50=initial_b50 + ((min_year-1-2010)*b50_diff2))


comparison$trend_last_b50[comparison$trend_last_b50<0]<-0
comparison$trend_last_b50[comparison$trend_last_b50>0.5]<-0.5

comparison%<>%group_by(country2)%>%mutate(mean_level_last_b50=mean(level_last_b50))%>%ungroup()

comparison%<>%mutate(ratio=mean_level_last_b50/trend_last_b50) 

#comparison%<>%mutate(check_ratio=level_last_b50/trend_last_b50) 
#comparison%<>%group_by(country2)%>%mutate(mean_check_ratio=mean(check_ratio)) %>%ungroup() #OK uguale



###check year for which trend only, yields level 0

comparison%<>% mutate(year_zero = -initial_b50/b50_diff2 + 2010) #coincide #SL non conta perché invece va a 0.5

comparison%<>% mutate(trend_last_positive_year=round(year_zero)-1)

comparison%<>% mutate(trend_last_positive_b50=initial_b50 + ((trend_last_positive_year-2010)*b50_diff2))

comparison_save<- comparison%>% select(country, min_year, level_last_b50, mean_level_last_b50,trend_last_b50, ratio)#, mean_ratio)


#write_csv(comparison_save, "data_revision/ratios_inf.csv")
