rm(list = ls())
setwd("~/Google Drive/replication_clim_ineq_sub2")


library(readr)
library(tidyverse)
library(magrittr)
library(ggtext)
library(ggallin)
library(ggpattern)
library("rnaturalearth")
library("rnaturalearthdata")

data <- read_csv("data_revision/data_scenarios_projections_withSSP_novariazb50.csv")

#load data for agri top/bottom classification
d2010<-read_csv("data_revision/d2010_rev_new.csv")#d2010_rev.csv")
d2010$country[is.na(d2010$country)]<-"NA"

#preliminaries on data
data %<>% 
  mutate(
    country2 = substr(country,1,2) #retrieve isoa2 country code
  ) %>% 
  mutate(
    country2 = ifelse(is.na(country2),"NA",country2), #fix namibia 
    country = ifelse(is.na(country), "NA", country)
  ) %>% 
  group_by(country2) %>% #variables that have one value per country, fill relative columns
  mutate(
    trend_impact_b50 = trend_impact_b50[country==country2], 
    trend_impact_gdp = trend_impact_gdp[country==country2],
    agri_top20 = agri_top20[country==country2],
    gdp_rate = gdp_rate[country==country2],
    b50_diff = b50_diff[country==country2],
    only_ssp3_impact_gdp = only_ssp3_impact_gdp[country==country2],
    only_ssp5_impact_gdp = only_ssp5_impact_gdp[country==country2]
  ) %>% ungroup() %>% 
  left_join( #merge data for top/bottom classification
    .,
    d2010 %>% select(country, top20_string) %>% distinct(), 
    by=c("country2"="country")
  ) %>% 
  mutate(agr_int= case_when(
    top20_string == "Bottom 80%" ~ "Low agricultural intensity",
    top20_string == "Top 20%" ~ "High agricultural intensity")
  ) 

data %>% 
  select(country2, b50_diff) %>% 
  distinct() -> data_tests

t.test(data_tests$b50_diff*100) -> t_b50

data_tests %>% 
  ggplot() + 
  geom_density(aes(x=b50_diff), fill="#B3CDE3") +
  geom_vline(aes(xintercept = mean(b50_diff, na.rm=T)), color="red") +
  geom_vline(aes(xintercept=0), color="black", linetype=2) +
  scale_x_continuous(breaks = c(-0.002, -0.004, -0.006, 0.002, 0.004, 0.006, 0), labels = scales::percent_format(accuracy = 0.1)) +
  geom_richtext(aes(
    x=-.004,y=250,
    label = paste0("**Sample mean**: ", round(t_b50$estimate,2), 
                   "%<br><br>**Two-sided t-test**",
                   "<br>95% Confidence interval: [", round(t_b50$conf.int[1],2), "%, ", round(t_b50$conf.int[2],2), "%]", 
                   "<br>p-value: ", round(t_b50$p.value,2)) 
  ), size=4) +
  xlab("Average yearly change in bottom 50% income share") +
  theme_minimal() +
  theme(
    #axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()
  ) -> p_b50

ggsave("final_charts/charts_si/b50_ttest.pdf",p_b50, width = 16, height = 8, units = "cm", scale = 1.4)
