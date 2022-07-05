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


data.frame(
  rcp = c("85","85","60","45","26"),
  ssp = c("5","3","2","2","1")
) -> scen

data_list <- list()

for(i in 1:nrow(scen)){
  ginis_all <- read_csv(paste0("data_revision/ginis_all_ssp_",scen$ssp[i],"_rcp_", scen$rcp[i], ".csv"))
  
  
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
  
  str_ssp <- paste0("SSP", scen$ssp[i], " RCP", scen$rcp[i])
  
  ginis_all %>% 
    rename(final=`Gini mean final`) %>% 
    rename(initial=`Gini initial`) %>% 
    mutate(
      scen = case_when(
        Scenario %in% c(paste0(str_ssp, " - Global - Prec") ,     paste0(str_ssp," - Top20 - Prec")) ~ "Precipitation",
        Scenario %in% c(paste0(str_ssp, " - Global - Prec+Temp"), paste0(str_ssp," - Top20 - Prec+Temp")) ~ "Precipitation + Temperature",
        Scenario %in% c("Only climate no outliers - Global - Prec", "Only climate no outliers - Top20 - Prec") ~ "Only prec",
        Scenario %in% c("Only climate no outliers - Global - Prec+Temp", "Only climate no outliers - Top20 - Prec+Temp") ~ "Only prec+temp"
      ),
      grp = case_when(
        Scenario %in% c("Only climate no outliers - Global - Prec", "Only climate no outliers - Global - Prec+Temp",
                        paste0(str_ssp, " - Global - Prec"), paste0(str_ssp," - Global - Prec+Temp")) ~ "Global",
        T ~ "High agricultural intensity countries"
      )
    ) -> data_fig4
  
  data_fig4 %>% 
    filter(scen!="Only prec+temp") %>% 
    select(-Scenario) %>% 
    pivot_longer(cols=c(initial, final)) %>% 
    filter(!(name=="initial"&scen!="Precipitation")) %>% 
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
  
  data_list[[i]] <- data_main
}

#### Plot ####


figures <- list()

for(i in 1:nrow(scen)){
  data_list[[i]] %>% 
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
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title = element_blank(),
      strip.text = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.text.x = element_text(hjust = 0.5, "Arial Unicode MS", size=15)
      #plot.margin = unit(c(0, 0, 0, 0), "null")#,
      #panel.margin = unit(c(0, 0, 0, 0), "null")
    ) -> figures[[i]]
  
}

cowplot::plot_grid(
  figures[[2]] + 
    ggtitle("SSP3 - RCP8.5") + 
    theme(
      plot.title = element_text(hjust=0.5, size=15),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      ), 
  figures[[1]] + 
    ggtitle("SSP5 - RCP8.5") + 
    theme(
      plot.title = element_text(hjust=0.5, size=15),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ), 
  nrow = 1
) -> row1

cowplot::plot_grid(
  figures[[3]] + 
    ggtitle("SSP2 - RCP6.0") + 
    theme(
      plot.title = element_text(hjust=0.5, size=15),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      ),
  figures[[4]] + 
    ggtitle("SSP2 - RCP4.5") + 
    theme(
      plot.title = element_text(hjust=0.5, size=15),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      ), 
  nrow = 1
) -> row2

cowplot::plot_grid(
  NULL,
  figures[[5]] + 
    ggtitle("SSP1 - RCP2.6") + 
    theme(
      plot.title = element_text(hjust=0.5, size=15),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      ),
  NULL,
  rel_widths = c(0.25, 0.5, 0.25), nrow = 1
) -> row3

cowplot::plot_grid(row1, row2, row3, nrow=3) -> fig_4_main

ggplot() +
  geom_text(
    aes(
      x=c(1),
      y=c(1),
      label=c("Precipitations")
    ), size=8
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
    ), size=8
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
    ), size=8
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
cowplot::plot_grid(fig_4_main + theme(plot.margin = unit(c(1, 0, 0, 0), "cm")),
                   legend_spaced + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                   rel_heights = c(0.9,0.1), nrow = 2) -> fig_4_adj

#+ theme(plot.margin = unit(c(0, 0, -0.1, 0), "cm"))
fig_4_adj + 
  annotate(
    "text",
    x=c(0.0832),
    y=c(0.055),
    label=c("\u2602"), size=12,family = "Arial Unicode MS"
  ) +
  annotate(
    "text",
    x=c(0.377),
    y=c(0.055),
    label=c("\u279a"), size=12,family = "Arial Unicode MS"
  ) +
  annotate(
    "text",
    x=c(0.752),
    y=c(0.054),
    label=c("\u2668"), size=12,family = "Arial Unicode MS"
  ) -> fig_4


quartz(type = 'pdf', file = paste0("final_charts/charts_si/figure4_all_ssps.pdf"), width = 15.5, height = 12)
fig_4
dev.off()

















