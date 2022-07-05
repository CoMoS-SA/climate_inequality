
#### merge different projection datasets ####


new_all<- read_csv("data_revision/proj_data_variaz_agriemp_bound_NOERROR.csv")
new_all$country[is.na(new_all$country)]<-"NA" #to include Nambia
#clim<-read_csv("data_revision/only_clim_comparable.csv") #proj_data_adjusted.csv")
#clim$country[is.na(clim$country)]<-"NA" #to include Nambia
#trend<- read_csv("data_revision/proj_just_trends.csv")
trend<- read_csv("data_revision/proj_just_trends_withvariazb50.csv")#with outliers
trend$country[is.na(trend$country)]<-"NA" #to include Nambia
#new_ssp3<- read_csv("data_revision/comb_ssp_variaz_all_NOERROR.csv")
#new_ssp3$country[is.na(new_ssp3$country)]<-"NA" #to include Nambia
#new_ssp5<- read_csv("data_revision/comb_SSP5_variaz_all_NOERROR.csv")
#new_ssp5$country[is.na(new_ssp5$country)]<-"NA" #to include Nambia

#only_ssp3<- read_csv("data_revision/proj_gdp_just_ssp_v2.csv")
#only_ssp3$country_iso2[is.na(only_ssp3$country_iso2)]<-"NA" #to include Nambia
#only_ssp5<- read_csv("data_revision/proj_gdp_just_ssp_SSP5_v2.csv")
#only_ssp5$country_iso2[is.na(only_ssp5$country_iso2)]<-"NA" #to include Nambia


colnames(new_all)
new_all_to_join<- new_all%>%select(country,b_proj, gdp_proj_prec, grate_b50proj_prec,grate_gdpproj_prec,tot_eff_gdp,mean_b50_grate_prec,mean_gdp_grate_prec,mean_gdp_grate_tot)

new_all_to_join%<>%rename(initial_b50=b_proj,
                          initial_gdp=gdp_proj_prec,
                          all_impact_prec_b50=grate_b50proj_prec,
                          all_impact_prec_gdp=grate_gdpproj_prec,
                          all_impact_prec_and_temp_gdp=tot_eff_gdp,
                          all_mean_impact_b50=mean_b50_grate_prec,
                          all_mean_impact_prec_gdp=mean_gdp_grate_prec,
                          all_mean_impact_prec_and_temp_gdp=mean_gdp_grate_tot)


#colnames(clim)
#initial_prec,final_prec, initial_temp, final_temp, #prendere da adjusted (old data)
#b_proj, gdp_proj_prec
#clim_to_join<- clim%>%select(country, grate_b50proj_prec,grate_gdpproj_prec,grate_gdpproj_temp,tot_eff_gdp,mean_b50_grate_prec,mean_gdp_grate_prec,mean_gdp_grate_temp,mean_gdp_grate_tot)

#clim_to_join%<>%rename(#initial_b50=b_proj,
                       #   initial_gdp=gdp_proj_prec,
#                       clim_impact_prec_b50=grate_b50proj_prec,
#                       clim_impact_prec_gdp=grate_gdpproj_prec,
#                       clim_impact_temp_gdp=grate_gdpproj_temp,
#                       clim_impact_prec_and_temp_gdp=tot_eff_gdp,
#                       clim_mean_impact_b50=mean_b50_grate_prec,
#                       clim_mean_impact_prec_gdp=mean_gdp_grate_prec,
#                       clim_mean_impact_temp_gdp=mean_gdp_grate_temp,
#                       clim_mean_impact_prec_and_temp_gdp=mean_gdp_grate_tot)

colnames(trend)
trend_to_join<- trend%>%select(-final_gdp_pc,-final_b50,-mean_b50,-mean_gdp)

trend_to_join%<>%rename(#initial_b50=mean_b50,
                       #initial_gdp=mean_gdp,
                       trend_impact_b50=grate_b50proj,
                       trend_impact_gdp=grate_gdpproj)


#new_ssp3_to_join<- new_ssp3%>%select(country, grate_b50proj_prec,grate_gdpproj_prec,tot_eff_gdp,mean_b50_grate_prec,mean_gdp_grate_prec,mean_gdp_grate_tot)#b_proj, gdp_proj_prec,

#new_ssp3_to_join%<>%rename(#initial_b50=b_proj,
#                          #initial_gdp=gdp_proj_prec,
#                          ssp3_impact_prec_b50=grate_b50proj_prec,
#                          ssp3_impact_prec_gdp=grate_gdpproj_prec,
#                          ssp3_impact_prec_and_temp_gdp=tot_eff_gdp,
#                          ssp3_mean_impact_b50=mean_b50_grate_prec,
#                          ssp3_mean_impact_prec_gdp=mean_gdp_grate_prec,
#                          ssp3_mean_impact_prec_and_temp_gdp=mean_gdp_grate_tot)



#new_ssp5_to_join<- new_ssp5%>%select(country, grate_b50proj_prec,grate_gdpproj_prec,tot_eff_gdp,mean_b50_grate_prec,mean_gdp_grate_prec,mean_gdp_grate_tot)#b_proj, gdp_proj_prec,

#new_ssp5_to_join%<>%rename(#initial_b50=b_proj,
  #initial_gdp=gdp_proj_prec,
#  ssp5_impact_prec_b50=grate_b50proj_prec,
#  ssp5_impact_prec_gdp=grate_gdpproj_prec,
#  ssp5_impact_prec_and_temp_gdp=tot_eff_gdp,
#  ssp5_mean_impact_b50=mean_b50_grate_prec,
#  ssp5_mean_impact_prec_gdp=mean_gdp_grate_prec,
#  ssp5_mean_impact_prec_and_temp_gdp=mean_gdp_grate_tot)

#only_ssp3_to_join<- only_ssp3%>%rename(only_ssp3_impact_gdp=grate_gdpproj)
#only_ssp5_to_join<- only_ssp5%>%rename(only_ssp5_impact_gdp=grate_gdpproj)

###merge  

merge1<-left_join(new_all_to_join, trend_to_join, by=c("country"="country"))

#merge1<-left_join(new_all_to_join, clim_to_join, by=c("country"="country"))

#data_scenarios_projections<- left_join(merge1, trend_to_join, by=c("country"="country"))
#merge2<- left_join(merge1, trend_to_join, by=c("country"="country"))
#merge3<- left_join(merge2, new_ssp3_to_join, by=c("country"="country"))
#merge4<-left_join(merge3, new_ssp5_to_join, by=c("country"="country"))
#merge5<-left_join(merge4, only_ssp3_to_join, by=c("country"="country_iso2"))
#data_scenarios_projections<- left_join(merge5, only_ssp5_to_join, by=c("country"="country_iso2"))

#write_csv(data_scenarios_projections, "data_revision/data_scenarios_projections_withSSP_withvariazb50.csv")

write_csv(merge1, "data_revision/data_scenarios_projections_withSSP_withvariazb50.csv")

#### check temp ####

vedi<-read_csv("data/proj_data_adjusted.csv")
vedi$country[is.na(vedi$country)]<-"NA" #to include Nambia

#check Niger
vedi%>%filter(substr(country,1,2)=="NE")%>%select(country, initial_temp,final_temp,initial_prec,final_prec)%>%mutate(mean_temp=mean(final_temp),
                                                                                                                     mean_prec=mean(final_prec))
