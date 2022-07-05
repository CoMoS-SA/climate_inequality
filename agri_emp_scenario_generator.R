#script to generate agricultural employment shares under different SSPs
rm(list = ls())
setwd("~/Google Drive/replication_clim_ineq_sub2")

data <- read_csv("data_revision/data_ssp_to_matte.csv")  
data$country[is.na(data$country)]<-"NA"

data %<>% 
  mutate(
    log_avg_agri_emp = log(avg_agri_emp)
  )

data %>% 
  ggplot() + 
  geom_text(aes(x=initial_gdp, y=avg_agri_emp, label=country)) +
  xlab("Average GDP per capita") + 
  ylab("Average agricultural employment share") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank()) -> chart_agri_gdp

ggsave("final_charts/charts_si/chart_agri_gdp.pdf", chart_agri_gdp, width = 10, height = 6, scale=0.8)

data %>% 
  ggplot() + 
  geom_text(aes(x=initial_gdp, y=log_avg_agri_emp, label=country)) +
  geom_smooth(aes(initial_gdp, y=log_avg_agri_emp), method=lm) +
  xlab("GDP per capita") + 
  ylab("Agricultural Employment Share")

reg <- summary(lm(log_avg_agri_emp ~ initial_gdp, data=data))

stargazer(lm(log_avg_agri_emp ~ initial_gdp, data=data))

coef <- coef(reg)["initial_gdp","Estimate"]

data %<>% 
  mutate(
    final_gdp_ssp1 = initial_gdp*(1+only_ssp1_impact_gdp),
    final_gdp_ssp2 = initial_gdp*(1+only_ssp2_impact_gdp),
    final_gdp_ssp3 = initial_gdp*(1+only_ssp3_impact_gdp),
    final_gdp_ssp5 = initial_gdp*(1+only_ssp5_impact_gdp)
  ) %>% 
  mutate(
    gdp_var_ssp1 = final_gdp_ssp1 - initial_gdp,
    gdp_var_ssp2 = final_gdp_ssp2 - initial_gdp,
    gdp_var_ssp3 = final_gdp_ssp3 - initial_gdp,
    gdp_var_ssp5 = final_gdp_ssp5 - initial_gdp
  ) %>% 
  mutate(
    agri_emp_ssp1 = exp((gdp_var_ssp1*coef) + log_avg_agri_emp),
    agri_emp_ssp2 = exp((gdp_var_ssp2*coef) + log_avg_agri_emp),
    agri_emp_ssp3 = exp((gdp_var_ssp3*coef) + log_avg_agri_emp),
    agri_emp_ssp5 = exp((gdp_var_ssp5*coef) + log_avg_agri_emp)
  ) %>% 
  select(-gdp_var_ssp1, -gdp_var_ssp2, -gdp_var_ssp3, -gdp_var_ssp5) %>% 
  select(country, avg_agri_emp, agri_emp_ssp1, agri_emp_ssp2, agri_emp_ssp3, agri_emp_ssp5)
  
saveRDS(data, "data_revision/agri_emp_scenarios.rds")

#agri_emp_ssp1 agri_emp_ssp2 agri_emp_ssp3 agri_emp_ssp5 contains agrishares for each scenario

#to plot them 
data %>% 
  ggplot() + 
  geom_text(aes(y=agri_emp_ssp3, x=country, label=country)) +
  ylab("Agricultural Employment Share (%) - SSP3") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

