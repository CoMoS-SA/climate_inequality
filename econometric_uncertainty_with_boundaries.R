#Note! Some parts take time to run. Outputs are available in the data folder 
#(CI_estimates_adj.csv  

rm(list = ls())

library(truncnorm)
library(readr)
library(tidyverse)
library(magrittr)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggallin)


#### Random draw of coefficients for econometric uncertainty ####

set.seed(1)

nrep= 1000 

#Bottom 50: nrep repetitions, coefficient, s.e. 
betas_prec_b50<-rtruncnorm(nrep, a=-0.0331-0.015, b=-0.0331+0.015, mean = -0.0331, sd = 0.015)

betas_prec_2_b50<-rtruncnorm(nrep, a=0.0133-0.00665, b=0.0133+0.00665, mean = 0.0133, sd = 0.00665)

betas_emp_prec_b50<-rtruncnorm(nrep, a=0.000774-0.0003, b=0.000774+0.0003, mean = 0.000774, sd = 0.0003)

betas_emp_prec_2_b50<-rtruncnorm(nrep, a=-0.000311-0.00001, b=-0.000311+0.00001, mean = -0.000311, sd = 0.00001)


#GDP: nrep repetitions, coefficient, s.e.
betas_emp_prec_gdp<-rtruncnorm(nrep, a=0.00236-0.00095, b=0.00236+0.00095, mean = 0.00236, sd = 0.00095)

betas_emp_prec_2_gdp<-rtruncnorm(nrep, a=-0.000916-0.000458, b=-0.000916+0.000458, mean = -0.000916, sd = 0.000458)

betas_temp_gdp<-rtruncnorm(nrep, a=0.0157-0.009, b=0.0157+0.009, mean = 0.0157, sd =0.009)

betas_temp_2_gdp<-rtruncnorm(nrep, a=-0.000759-0.0003, b=-0.000759+0.0003, mean = -0.000759, sd =0.0003)


#### UNCERTAINTY DUE ONLY TO OUR ESTIMATION #######
#Note! This part takes time to run. Output is available in data/CI_estimates_adj.csv

output_long<- read_csv("data_revision/longdataproj_NOERROR_novariazb50.csv")#output_long_adjusted.csv")

output_long$country[is.na(output_long$country)]<-"NA" #to include Nambia

#output_long_t<- read_csv("data_revision/output_long_t_baseline.csv")#adjusted.csv")  

#output_long_t$country[is.na(output_long_t$country)]<-"NA" #to include Nambia

d2010<-read_csv("data_revision/d2010_rev_new.csv") #nb agriemp is avg 1991 to 2010 to be used in proj
d2010$country[is.na(d2010$country)]<-"NA" #to include Nambia


prec_projections<- output_long%>%mutate(country=substr(country,1,2))
prec_projections%<>%group_by(country,year) %>% mutate(avg_prec_proj=mean(proj_prec, na.rm=T))
prec_projections%<>% distinct(country,year, .keep_all=T)
 
#temp_projections<- output_long_t%>%mutate(country=substr(country,1,2))
#temp_projections%<>%group_by(country,year) %>% mutate(avg_temp_proj=mean(proj_temp, na.rm=T))
#temp_projections%<>% distinct(country,year, .keep_all=T)
d2010unique<-d2010%>%distinct(country,.keep_all=T)

years = 71
countries = 104

### B50 ####
b50_list <- vector(mode = "list", length = nrep)


output_b <- matrix(ncol=countries, nrow=years)
output_b[1,] <- d2010unique$mean_b50
colnames(output_b)<-d2010unique$country
for (j in 1:length(betas_prec_b50)){
  for (i in d2010unique$country){
    for (t in 1:70){
      if(output_b[t][!is.na(output_b[t])]){
        #  output_b[t+1,i] = output_b[t,i]+ ((betas_prec_b50[j] + 2*betas_prec_2_b50[j]*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])) + (betas_emp_prec_b50[j]*d2010unique$agri_emp[d2010unique$country==i]) + (2*betas_emp_prec_2_b50[j]*d2010unique$agri_emp[d2010unique$country==i]*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]-prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t])}  
        if(!is.na(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i])) & d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]) > 99){
         
           output_b[t+1,i] = output_b[t,i]+ ((betas_prec_b50[j] + 2*betas_prec_2_b50[j]*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])) + (betas_emp_prec_b50[j]*(99)) + (2*betas_emp_prec_2_b50[j]*(99)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]-prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t])  
      
        }else if(!is.na(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i])) & d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]) < 1){
       
         output_b[t+1,i] = output_b[t,i]+ ((betas_prec_b50[j] + 2*betas_prec_2_b50[j]*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])) + (betas_emp_prec_b50[j]*(1)) + (2*betas_emp_prec_2_b50[j]*(1)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]-prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t]) 
      
    }else{
        output_b[t+1,i] = output_b[t,i]+ ((betas_prec_b50[j] + 2*betas_prec_2_b50[j]*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])) + (betas_emp_prec_b50[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))) + (2*betas_emp_prec_2_b50[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]-prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t])
        }  
      #(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i])) 
      #d2010unique$agri_emp[d2010unique$country==i]
  }
      else{
        output_b[t+1,i] = NA}
    }}
  b50_list[[j]]<-output_b
}


  check_b50_list<-b50_list
  
  ### GDP prec + temp ####
  
#  gdp_list <- vector(mode = "list", length = nrep)
  
#  output_gdp <- matrix(ncol=countries, nrow=years)
#  output_gdp[1,] <- d2010unique$mean_gdp
#  colnames(output_gdp)<-d2010unique$country
  
#  for (j in 1:length(betas_prec_b50)){
#    for (i in d2010unique$country){
#      for (t in 1:70){
#        if(output_gdp[t][!is.na(output_gdp[t])]){
#          if(!is.na(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i])) & d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]) > 99){
#          delta=d2010unique$gdp_rate[d2010unique$country==i]
#          eta=((betas_emp_prec_gdp[j]*(99)*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]) + (betas_emp_prec_2_gdp[j]*(99)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])^2)) - ((betas_emp_prec_gdp[j]*(99)*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1]) + (betas_emp_prec_2_gdp[j]*(99)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1])^2))
#          tau=((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])^2) - ((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])^2)))
#          output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta + tau)  
#        }else if(!is.na(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i])) & d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]) < 1){
#          delta=d2010unique$gdp_rate[d2010unique$country==i]
#          eta=((betas_emp_prec_gdp[j]*(1)*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]) + (betas_emp_prec_2_gdp[j]*(1)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])^2)) - ((betas_emp_prec_gdp[j]*(1)*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1]) + (betas_emp_prec_2_gdp[j]*(1)*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1])^2))
#          tau=((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])^2) - ((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])^2)))
#         output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta + tau)
#        }else{
#          delta=d2010unique$gdp_rate[d2010unique$country==i]
#          eta=((betas_emp_prec_gdp[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1]) + (betas_emp_prec_2_gdp[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+t+1])^2)) - ((betas_emp_prec_gdp[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))*prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1]) + (betas_emp_prec_2_gdp[j]*(d2010unique$agri_emp[d2010unique$country==i]+(t*d2010unique$agri_diff[d2010unique$country==i]))*(prec_projections$avg_prec_proj[prec_projections$country==i &prec_projections$year==2009+1])^2))
#          tau=((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+t+1])^2) - ((betas_temp_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])+betas_temp_2_gdp[j]*(temp_projections$avg_temp_proj[temp_projections$country==i &temp_projections$year==2009+1])^2)))
#          output_gdp[t+1,i] = output_gdp[t,i] * (1+ delta + eta + tau)
#          }}  
#        else{
#          output_gdp[t+1,i] = NA}
#      }}
#    gdp_list[[j]]<-output_gdp
#  }
  
#  check_gdp_list<-gdp_list
  
  
  
  
  #### Output analysis ####
  
  
  means_b50_prec<- matrix(0,countries,2)
  #means_gdp<- matrix(0,countries,2)
  #means_gdp_temp<- matrix(0,countries,2)
  
  
  
  
  for (i in 1:(countries)){
    means_b50_prec[i]<- mean(sapply(b50_list, `[[`, 71,i))
#    means_gdp[i]<- mean(sapply(gdp_list, `[[`, 71,i))
    #  means_gdp_temp[i]<- mean(sapply(gdp_list_temp, `[[`, 71,i))
    means_b50_prec[i,2]<- sd(sapply(b50_list, `[[`, 71,i))
#    means_gdp[i,2]<- sd(sapply(gdp_list, `[[`, 71,i))
    #  means_gdp_temp[i,2]<- sd(sapply(gdp_list_temp, `[[`, 71,i))
    
  }
  
  means_b50_prec<-as.data.frame(means_b50_prec)
  #means_gdp<-as.data.frame(means_gdp)
  #means_gdp_temp<-as.data.frame(means_gdp_temp)
  
  colnames(means_b50_prec) <- c("mean_b50_prec","sd_b50_prec")
  means_b50_prec%<>% mutate(conf_min_b50_prec=mean_b50_prec-((1.96*sd_b50_prec)/sqrt(nrep)),
                            conf_max_b50_prec=mean_b50_prec+((1.96*sd_b50_prec)/sqrt(nrep)))
  
  
  #colnames(means_gdp) <- c("mean_gdp_clim","sd_gdp")
  #means_gdp%<>% mutate(conf_min_gdp=mean_gdp_clim-((1.96*sd_gdp)/sqrt(nrep)),
                            conf_max_gdp=mean_gdp_clim+((1.96*sd_gdp)/sqrt(nrep)))
  
  #colnames(means_gdp_temp) <- c("mean_gdp_temp","sd_gdp_temp")
  #means_gdp_temp%<>% mutate(conf_min_gdp_temp=mean_gdp_temp-((1.96*sd_gdp_temp)/sqrt(nrep)),
  #                          conf_max_gdp_temp=mean_gdp_temp+((1.96*sd_gdp_temp)/sqrt(nrep)))
  
  
  sensitivity_estimates<-cbind(d2010unique,  means_b50_prec)#, means_gdp)# means_gdp_temp)
  
  sensitivity_estimates %<>% mutate(
    grate_b50=(mean_b50_prec-mean_b50)/mean_b50,
    grate_b50_min=(conf_min_b50_prec-mean_b50)/mean_b50,
    grate_b50_max=(conf_max_b50_prec-mean_b50)/mean_b50,
   # grate_gdp=(mean_gdp_clim-mean_gdp)/mean_gdp,
  #  grate_gdp_min=(conf_min_gdp-mean_gdp)/mean_gdp,
  #  grate_gdp_max=(conf_max_gdp-mean_gdp)/mean_gdp
    #grate_gdp_temp=(mean_gdp_temp-mean_gdp)/mean_gdp,
    #grate_gdp_temp_min=(conf_min_gdp_temp-mean_gdp)/mean_gdp,
    #grate_gdp_temp_max=(conf_max_gdp_temp-mean_gdp)/mean_gdp
  )
  
  #sensitivity_estimates %<>% mutate(
  # grate_gdp_tot=grate_gdp_prec+grate_gdp_temp,
  #  grate_gdp_tot_min=grate_gdp_prec_min+grate_gdp_temp_min,
  #  grate_gdp_tot_max=grate_gdp_prec_max+grate_gdp_temp_max
  #)
  
  sensitivity_estimates %<>% mutate(
    quadrant_mean_b50 = case_when(
      # grate_gdp_prec_min<0 & grate_gdp_prec_max>0 ~ 5,
      # grate_b50_min<0 & grate_b50_max>0 ~ 6, #5 #OR THIS IF AS IN OLD CSV 
      grate_b50_min<0 & grate_b50_max>0 ~ 2, #not significant
      is.na(grate_b50_min)==T ~ 0, #NA
      TRUE ~ 1)) #significant
  
 # sensitivity_estimates %<>% mutate(
#    quadrant_mean_tot_gdp = case_when(
#      # grate_gdp_prec_min<0 & grate_gdp_prec_max>0 ~ 5,
#      # grate_b50_min<0 & grate_b50_max>0 ~ 6, #5 #OR THIS IF AS IN OLD CSV 
#      grate_gdp_min<0 & grate_gdp_max>0 ~ 2, #not significant
#      is.na(grate_gdp_min)==T ~ 0, #NA
#      TRUE ~ 1)) #significant
  
  #sensitivity_estimates %<>% mutate(
  #  quadrant_mean_prec = case_when(
  #    grate_gdp_prec_min<0 & grate_gdp_prec_max>0 & grate_b50_min<0 & grate_b50_max>0 ~ 7,
  #    TRUE ~ quadrant_mean_prec)) #quadrant_mean_prec
  
  #sensitivity_estimates %<>% mutate(
  #  quadrant_mean_tot = case_when(
  #    grate_gdp_tot_min<0 & grate_gdp_tot_max>0 ~ 5,
  #    grate_b50_min<0 & grate_b50_max>0 ~ 6, #5
  #    TRUE ~ 1)) #quadrant_mean_prec
  
  #sensitivity_estimates %<>% mutate(
  #  quadrant_mean_tot = case_when(
  #    grate_gdp_tot_min<0 & grate_gdp_tot_max>0 & grate_b50_min<0 & grate_b50_max>0 ~ 7,
  #    TRUE ~ quadrant_mean_tot)) #quadrant_mean_prec
  
  
#  write_csv(sensitivity_estimates, "data_revision/CI_estimates_revision.csv")
  #write_csv(sensitivity_estimates, "data_revision/CI_estimates_revision_all.csv")
 
  
  
  #### FOR SUPPLEMENTARY INFORMATION ######
  
  #### What do results imply in terms of poverty? ####
  
  ## SSAF calibration ##
  library(readr)
  library(tidyverse)
  library(magrittr)
  
  data_b <- read_csv("data/data_bottom_different_q.csv")
  
  data_b$country[is.na(data_b$country)]<-"NA" #to include Nambia
  
  ssaf_data<-data_b%>%filter(continent=="SSAF")
  ssaf_data%<>%group_by(country)%>%mutate(avg_b50=mean(bottom50,na.rm=T),
                                          avg_pop=mean(Pop,na.rm=T),
                                          avg_gdp=mean(gdpCAPppp_wdi,na.rm=T))%>%ungroup()
  
  ssaf_data%<>%distinct(country,.keep_all = T)
  n=round(mean(ssaf_data$avg_pop,na.rm=T))
  nb=round(n/2)
  
  b50=mean(ssaf_data$avg_b50,na.rm=T)
  gdp_pc=mean(ssaf_data$avg_gdp,na.rm=T)
  gdp_pc_b50=gdp_pc*b50
  
  
  incomes<-rnorm(nb, gdp_pc_b50, 200)
  incomes_t2=incomes*(1-0.05) #assume a shock of 5% on incomes in bottom 50% (in our example equivalent to shocking bottom 50% share - see under)
  poverty_day=1.9 #change poverty line for alternatives
  poverty_y=1.9*365 #change poverty line for alternatives
  
  t1_poor=table(incomes<poverty_y)[2]
  
  #percentage of poor in period 1 in the overall population
  t1_poor/n 
  
  
  t2_poor=table(incomes_t2<poverty_y)[2]
  
  #percentage of poor after shock in the overall population
  t2_poor/n 
  
  #percentage of poor in period 1 in bottom 50%
  t1_poor/nb 
  
  #percentage of poor after shock in bottom 50%
  t2_poor/nb 
  
  #check that in our example shocking bottom 50% share is equivalent to shocking incomes 
  #((sum(incomes_t2)) / (gdp_pc*nb)) / b50
  