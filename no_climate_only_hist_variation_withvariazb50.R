
###Just historical trends ####
data_b <- read_csv("data/data_bottom_different_q.csv")

data_b$country[is.na(data_b$country)]<-"NA" #to include Nambia


data_b%<>% filter(year>1990)%>%filter(year<2011)%>% group_by(country) %>% mutate( #1990 invece di 1989 per agridiff
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
  agri_diff=(last_agri-first_agri)/19
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


data_just_trends <- data_b%>%select(country,gdp_rate,b50_diff)%>%distinct(country,.keep_all=T)

d2010<-read_csv("data_revision/d2010_rev_new.csv")#d2010_rev.csv")
d2010$country[is.na(d2010$country)]<-"NA" 

d2010_to_join<-d2010%>%select(country, mean_gdp, mean_b50, agri_top20)%>%distinct(country,.keep_all=T)

data_just_trends<-inner_join(data_just_trends, d2010_to_join, by=c("country"="country"))
data_just_trends%<>%filter(country!="BH")%>%filter(country!="ME") %>%filter(country!="MT") %>%filter(country!="SC")  #because nan in climate projections (to have comparable set of countries)
#filter(country!="SY")


#data_just_trends%<>%filter(country!="GQ")%>%filter(country!="BA") # for no outliers

data_just_trends%<>%mutate(final_gdp_pc=mean_gdp * ((1+gdp_rate)^70),
                           final_b50=mean_b50 + (70*b50_diff))


data_just_trends$final_b50[data_just_trends$final_b50<0]<-0
data_just_trends$final_b50[data_just_trends$final_b50>0.5]<-0.5

#### Impacts ####

data_just_trends%<>% mutate(grate_gdpproj=(final_gdp_pc-mean_gdp)/mean_gdp,
                            grate_b50proj=(final_b50-mean_b50)/mean_b50)

#write_csv(data_just_trends, "data_revision/proj_just_trends_withvariazb50.csv")

#### Ginis ####

data_just_trends%<>%mutate(initial_b50_income=mean_gdp*mean_b50)
data_just_trends%<>%mutate(initial_t50_income=mean_gdp-initial_b50_income)

data_just_trends%<>%mutate(final_b50_income=final_gdp_pc*final_b50)
data_just_trends%<>%mutate(final_t50_income=final_gdp_pc-final_b50_income)

#global
gini_global_initial<- ineq(rbind(data_just_trends$initial_b50_income,data_just_trends$initial_t50_income),type="Gini")

gini_global_final<- ineq(rbind(data_just_trends$final_b50_income,data_just_trends$final_t50_income),type="Gini")

#top20
top20<-data_just_trends%>%filter(agri_top20==1)

gini_top20_initial<-ineq(rbind(top20$initial_b50_income,top20$initial_t50_income),type="Gini")

gini_top20_final<-ineq(rbind(top20$final_b50_income,top20$final_t50_income),type="Gini")

###append to existing dataset
ginis_all<- read_csv("data_revision/ginis_all_withvariaz.csv")

gini_for_fig<-cbind(gini_global_initial, gini_global_final, NA)
colnames(gini_for_fig)<-c("Gini initial", "Gini mean final", "SE")
gini_for_fig<-as.data.frame(gini_for_fig)%>%mutate(Scenario="Hist no climate no outliers - Global")
ginis_all<-rbind(ginis_all,gini_for_fig)

gini_for_fig<-cbind(gini_top20_initial, gini_top20_final, NA)
colnames(gini_for_fig)<-c("Gini initial", "Gini mean final", "SE")
gini_for_fig<-as.data.frame(gini_for_fig)%>%mutate(Scenario="Hist no climate no outliers - Top20")
ginis_all<-rbind(ginis_all,gini_for_fig)

#write_csv(ginis_all, "data_revision/ginis_all_withvariaz.csv")
