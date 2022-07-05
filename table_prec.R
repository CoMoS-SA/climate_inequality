
library(xtable)

#Check initial and final avg agri emp

d2010<-read_csv("data_revision/d2010_rev_new.csv") #nb agriemp is avg 1991 to 2010 to be used in proj
d2010$country[is.na(d2010$country)]<-"NA" #to include Nambia
d2010%<>%mutate(final_agri_proj=agri_emp+(70*agri_diff))
d2010_unique<-d2010%>%distinct(country,.keep_all=T)
check_ne<-d2010_unique%>%filter(country=="NE")



#Table prec
proj<-read_csv("data_revision/proj_data_variaz_agriemp_bound_NOERROR_novariazb50.csv") #nb agriemp is avg 1991 to 2010 to be used in proj
proj$country[is.na(proj$country)]<-"NA" #to include Nambia

proj%<>%mutate(country2=substr(country,1,2))
proj%<>%group_by(country2)%>%mutate(mean_final_prec= mean(final_prec),
                                   med_final_prec =median(final_prec))%>%ungroup()


proj_for_table<-proj%>%distinct(country2,.keep_all=T)%>%select(country, initial_prec, med_final_prec) #mean_final_prec, 


View(proj_for_table)



print(xtable(proj_for_table), include.rownames=FALSE)
