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

unc_data <- read_csv("data_revision/CI_estimates_revision.csv")
unc_data$country[is.na(unc_data$country)]<-"NA"
#1 = signif, quadrant_mean_b50


#load regions
regions <- read_csv("data/regions.csv")
regions$country[is.na(regions$country)]<-"NA"

#load data for agri top/bottom classification
d2010<-read_csv("data_revision/d2010_rev_new.csv")#d2010_rev.csv")
d2010$country[is.na(d2010$country)]<-"NA"

#load data on precipitations
data_prec <- read_csv("data_revision/proj_data_variaz_agriemp_bound_NOERROR_novariazb50.csv")
data_prec$country[is.na(data_prec$country)]<-"NA"

data_prec %<>% 
  mutate(
    country2 = substr(country,1,2)
  ) %>% 
  mutate(
    country2 = ifelse(is.na(country2),"NA",country2),
    country = ifelse(is.na(country), "NA", country)
  ) %>% 
  select(
    country2, initial_prec, final_prec
  ) %>% 
  mutate(is_wetter = (final_prec - initial_prec)>0) %>% 
  group_by(country2) %>% 
  summarise(
    freq = sum(is_wetter)/n()
  ) %>% 
  mutate(
    wet = case_when(
      freq==1 ~ "\u2601",
      freq==0 ~ "\u2600",
      T ~ ""
    )
  ) %>% select(-freq)

#preliminaries on data
data %<>% 
  mutate(
    country2 = substr(country,1,2)
  ) %>% 
  mutate(
    country2 = ifelse(is.na(country2),"NA",country2),
    country = ifelse(is.na(country), "NA", country)
  ) %>% 
  group_by(country2) %>% 
  mutate(
    trend_impact_b50 = trend_impact_b50[country==country2], 
    trend_impact_gdp = trend_impact_gdp[country==country2],
    agri_top20 = agri_top20[country==country2],
    gdp_rate = gdp_rate[country==country2],
    b50_diff = b50_diff[country==country2],
    only_ssp3_impact_gdp = only_ssp3_impact_gdp[country==country2],
    only_ssp5_impact_gdp = only_ssp5_impact_gdp[country==country2],
  ) %>% ungroup() %>% 
  left_join( #merge with top/bottom info
    .,
    d2010 %>% select(country, top20_string) %>% distinct(), 
    by=c("country2"="country")
  ) %>% 
  left_join( #merge with regions
    .,
    regions %>% filter(!is.na(country)), 
    by=c("country2"="country")
  ) %>% 
  left_join( #merge with econometric uncertainty measures
    ., 
    unc_data %>% select(country, quadrant_mean_b50),
    by=c("country2"="country")
  ) %>% 
  left_join( #merge information on precipitations
    ., 
    data_prec,
    by="country2" #use country to merge, model specific
  ) %>% 
  mutate(agr_int= case_when(
    top20_string == "Bottom 80%" ~ "Low agricultural intensity",
    top20_string == "Top 20%" ~ "High agricultural intensity")
  ) 


### Create variables of interest

data %<>% 
  mutate(
    final_b50_all = initial_b50*(1+all_impact_prec_b50)
  ) %>% 
  mutate(
    ratio_b50 = final_b50_all/initial_b50, #bottom + prec + agrishare OVER bottom
  )

# manipulate regions
data %<>% 
  mutate(
    cont = case_when(
      continent %in% c("EECA","LAC","SEAS")  ~ "Europe & Central Asia, Latin America & Caribbean, South East Asia",
      continent == "MENA" ~ "Middle East & North Africa",
      continent == "SSAF" ~ "Sub-Saharan Africa",
      continent == "WEOFF" ~ "Western Europe and Offshoots"
    ),
    cont_sa = case_when(
      continent_ssaf == "EECA" ~ "Eastern Europe & Central Asia",
      continent_ssaf %in% c("LAC","SEAS")  ~ "Latin America, Caribbean & South East Asia",
      continent_ssaf == "MENA" ~ "Middle East & North Africa",
      continent_ssaf == "SSAF" ~ "Sub-Saharan Africa",
      continent_ssaf == "WEOFF" ~ "Western Europe and Offshoots",
      continent_ssaf == "WAF" ~ "Western Africa",
      continent_ssaf == "MA" ~ "Middle Africa",
      continent_ssaf == "EAF" ~ "Eastern Africa",
      continent_ssaf == "SAF" ~ "South Africa"
    )
  )

data %>% 
  rename(variable = ratio_b50) %>% 
  filter(!is.na(variable)) %>% 
  mutate(
    variable = (variable)-1
  ) %>% 
  group_by(country2) %>% 
  summarise(
    med_b50 = median(variable),
    lower_b50 = quantile(variable, 0.25),
    upper_b50 = quantile(variable, 0.75),
    notchlower_b50 =  quantile(variable, 0.1), #min(variable[variable>=(lower_b50 - (1.5*IQR(variable)))]),
    notchupper_b50 =  quantile(variable, 0.9), #max(variable[variable<=(upper_b50 + (1.5*IQR(variable)))]),
    wet = wet[1],#ifelse((mean(final_prec) - initial_prec[1])>0, "\u2601","\u2600"),
    cont =cont[1],
    continent_ssaf =continent_ssaf[1],
    econ_sign = ifelse(quadrant_mean_b50[1]==2, "\u2717", "")
    #econ_sign_label = ifelse(quadrant_mean_prec_b50==2, paste0(country2[1],"*"), country2[1])
  ) -> data_ratio_b50_clim

#### Charts ####

pal <- c(
  "Eastern Europe, Latin America,\nCentral and South-East Asia"="#41ab5d",
  "Middle East & North Africa"="#fd8d3c",
  "Western Europe and Offshoots"="#4292c6",
  "Sub-Saharan Africa"="#d7301f"
)


data_ratio_b50_clim %>% 
  mutate(
    cont=factor(
      cont,
      levels = c(
        "Europe & Central Asia, Latin America & Caribbean, South East Asia",
        "Western Europe and Offshoots",
        "Middle East & North Africa",
        "Sub-Saharan Africa"
      ),
      labels = c(
        "Eastern Europe, Latin America,\nCentral and South-East Asia",
        "Western Europe and Offshoots",
        "Middle East & North Africa",
        "Sub-Saharan Africa"
      )
    )
  ) %>% 
  ggplot() +
  geom_hline(yintercept = 0, color="red", linetype=2) +
  geom_boxplot(
    aes(
      x=reorder(country2,med_b50), 
      middle=med_b50,
      lower = lower_b50,
      upper = upper_b50,
      ymin = notchlower_b50,
      ymax = notchupper_b50,
      fill=cont
    ),
    stat="identity",
    alpha=0.8,
    size=0.2
  ) +
  geom_text(
    aes(
      x=reorder(country2,med_b50),
      y=notchupper_b50+0.016,
      label=country2,
    ), size=2.8
  ) +
  geom_text(
    aes(
      x=reorder(country2,med_b50),
      y=notchupper_b50+0.036,
      label=econ_sign,
    ), size=3, family = "Arial Unicode MS", color="red"
  ) +
  geom_text(
    aes(
      x=reorder(country2,med_b50),
      y=notchlower_b50-0.015,
      #label="\U1F418"
      label=wet
    ), size=4, family = "Arial Unicode MS"
  ) +
  facet_wrap(cont~., scales = "free_y", nrow = 1) +
  #scale_y_continuous(limits = c(-0.17,0.21)) +
  scale_fill_manual(values = pal) +
  ylab("Percentage change in bottom 50% income share") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  coord_flip() +
  theme_minimal() +
  theme(
    #text = element_text(size=18),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y= element_blank(),
    axis.text.x= element_text(size = 10),
    axis.ticks.y = element_blank(),
    #axis.text.x= element_text(size = 18)
  ) -> p1

ggplot() +
  geom_text(
    aes(
      x=c(1),
      y=c(1),
      label=c("Dryer climate (All models)")
    ), size=5
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) -> legend1

ggplot() +
  geom_text(
    aes(
      x=c(1),
      y=c(1),
      label=c("Wetter climate (All models)")
    ), size=5
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) -> legend2

ggplot() +
  geom_text(
    aes(
      x=c(1),
      y=c(1),
      label=c("Statistical uncertainty")
    ), size=5
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) -> legend3


cowplot::plot_grid(legend1, legend2, legend3, rel_heights = c(0.3,0.3,0.3), nrow = 1) -> legend_spaced
cowplot::plot_grid(p1,legend_spaced,rel_heights = c(0.93,0.07), nrow = 2) -> boxplots

boxplots + 
  annotate(
    "text",
    x=c(0.745),
    y=c(0.04),
    label=c("\u2717"), size=7,family = "Arial Unicode MS", color="red"
  ) +
  annotate(
    "text",
    x=c(0.385),
    y=c(0.037),
    label=c("\u2601"), size=7,family = "Arial Unicode MS"
  ) +
  annotate(
    "text",
    x=c(0.060),
    y=c(0.04),
    label=c("\u2600"), size=7,family = "Arial Unicode MS"
  ) -> final_boxplots


quartz(type = 'pdf', file = 'final_charts/charts/figure3.pdf', width = 12, height = 6.5)
final_boxplots
dev.off()
