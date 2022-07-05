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

ginis_all<- read_csv("data_revision/ginis_all_novariazb50.csv")

pal <- c(
  "High agricultural intensity countries"="#E58601",
  "Global" ="#46ACC8"
)

alphas <- c(
  "Initial" = 0.3,
  "Only_climate" = 0.5,
  "Prec" = 0.7,
  "Prec+Temp" = 0.9
)

pal2 <- c(
  "InitialHigh agricultural intensity countries" = "#fee391",
  "Only precHigh agricultural intensity countries" = "#E58601",
  "PrecipitationHigh agricultural intensity countries" = "#cc4c02",
  "Precipitation + TemperatureHigh agricultural intensity countries" = "#800026",
  "InitialGlobal" = "#a8ddb5",          
  "Only precGlobal" = "#46ACC8",     
  "PrecipitationGlobal" = "#0570b0",                                 
  "Precipitation + TemperatureGlobal" = "#084081"
)



ginis_all %>% 
  rename(final=`Gini mean final`) %>% 
  rename(initial=`Gini initial`) %>% 
  mutate(
    scen = case_when(
      Scenario %in% c("Baseline - Global - Prec", "Baseline - Top20 - Prec") ~ "Precipitation",
      Scenario %in% c("Baseline - Global - Prec+Temp", "Baseline - Top20 - Prec+Temp") ~ "Precipitation + Temperature",
      Scenario %in% c("Hist no climate no outliers - Global", "Hist no climate no outliers - Top20") ~ "No climate",
      Scenario %in% c("Only climate no outliers - Global - Prec", "Only climate no outliers - Top20 - Prec") ~ "Only prec",
      Scenario %in% c("Only climate no outliers - Global - Prec+Temp", "Only climate no outliers - Top20 - Prec+Temp") ~ "Only prec+temp"
    ),
    grp = case_when(
      Scenario %in% c("Baseline - Global - Prec", "Baseline - Global - Prec+Temp", "Hist no climate no outliers - Global",
                      "Only climate no outliers - Global - Prec", "Only climate no outliers - Global - Prec+Temp") ~ "Global",
      T ~ "High agricultural intensity countries"
    )
  ) -> data_fig4

data_fig4 %>% 
  filter(scen!="Only prec+temp") %>% 
  select(-Scenario) %>% 
  pivot_longer(cols=c(initial, final)) %>% 
  filter(!(name=="initial"&scen!="Precipitation")) %>% 
  filter(scen!="No climate") %>% 
  mutate(
    scen = case_when(
      name=="initial" ~ "Initial",
      T ~ scen
    )
  ) %>% 
  mutate(
    SE = case_when(
      scen=="Initial" ~ as.numeric(NA),
      T ~ SE
    )
  ) %>% 
  mutate(
    col = paste0(scen, grp)
  ) -> data_main 


data_main %>% 
  mutate(
    sign = case_when(
      scen=="Only prec" ~ "\u2602",
      scen=="Precipitation" ~ "\u2602 \u279a",
      scen=="Precipitation + Temperature" ~ "\u2602 \u279a \u2668",
      T ~ ""
    )
  ) %>% 
  ggplot() +
  geom_col(aes(x=scen, y=value, fill=col), alpha=0.9) + 
  geom_errorbar(aes(x=scen, ymin=value-(1.96*SE), ymax=value+(1.96*SE), group=scen), width=0.15, size=0.2) +
  facet_wrap(.~grp, nrow=1) +
  geom_text(
    aes(
      x=scen,
      y=value+(1.96*SE) +0.07,
      label=sign
    ),
    size=7,family = "Arial Unicode MS"
  ) +
  scale_x_discrete(labels = c('Initial' = "Initial \n\n\n",
                              'Only prec'   = "2080-99\n (\u03C4=0, \u03B4=0)",
                              'Precipitation' = "2080-99\n (\u03C4=0)",
                              'Precipitation + Temperature'   = "2080-99"
  )
  ) +
  scale_fill_manual(values = pal2) +
  theme_minimal() + 
  ylab("Gini Index") + 
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    strip.text = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.text.x = element_text(hjust = 0.5, "Arial Unicode MS", size=15)
    #plot.margin = unit(c(0, 0, 0, 0), "null")#,
    #panel.margin = unit(c(0, 0, 0, 0), "null")
  ) -> fig_4_main

ggplot() +
  geom_text(
    aes(
      x=c(1),
      y=c(1),
      label=c("Precipitations")
    ), size=6
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
      label=c("Macroeconomic trends")
    ), size=6
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
      label=c("Temperature")
    ), size=6
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
cowplot::plot_grid(fig_4_main + theme(plot.margin = unit(c(0, 0, -0.1, 0), "cm")),
                   legend_spaced + theme(plot.margin = unit(c(-1, 0, 0, 0), "cm")),
                   rel_heights = c(0.9,0.1), nrow = 2) -> fig_4_adj


fig_4_adj + 
  annotate(
    "text",
    x=c(0.062),
    y=c(0.098),
    label=c("\u2602"), size=7,family = "Arial Unicode MS"
  ) +
  annotate(
    "text",
    x=c(0.335),
    y=c(0.098),
    label=c("\u279a"), size=7,family = "Arial Unicode MS"
  ) +
  annotate(
    "text",
    x=c(0.73),
    y=c(0.095),
    label=c("\u2668"), size=7,family = "Arial Unicode MS"
  ) +
  theme(plot.margin = unit(c(0, 0, -0.7, 0.02), "cm")) -> fig_4


quartz(type = 'pdf', file = 'final_charts/charts/figure4.pdf', width = 8, height = 4.5)
fig_4
dev.off()

