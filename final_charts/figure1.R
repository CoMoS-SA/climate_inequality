rm(list = ls())


library(readr)
library(tidyverse)
library(magrittr)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggallin)
library(ggpattern)

#create full dataset for histogram and parabolas

#create dataset for parabolas
#bottom 50, high
bott_high <- read.csv("stata_data/estimated_response_b50_top20_djossaf.csv") 
bott_high$variable <- "bot50"
bott_high$intensity <- "high"
#bottom 50, low
bott_low <- read.csv("stata_data/estimated_response_b50_bottom80_djossaf.csv") 
bott_low$variable <- "bot50"
bott_low$intensity <- "low"

#GDP, high
gdp_high <- read.csv("stata_data/estimated_response_macroreg_growth.csv") 
gdp_high$variable <- "gdp"
gdp_high$intensity <- "high"
#GDP, low
gdp_low <- read.csv("stata_data/estimated_response_macroreg_growthb80.csv") 
gdp_low$variable <- "gdp"
gdp_low$intensity <- "low"

# Agri, high
agri_high <- read.csv("stata_data/estimated_response_macroreg_agrgdp.csv") 
agri_high$variable <- "agri"
agri_high$intensity <- "high"
# Agri, low
agri_low <- read.csv("stata_data/estimated_response_macroreg_agrgdpb80.csv") 
agri_low$variable <- "agri"
agri_low$intensity <- "low"

fig1_left_data <- rbind(bott_high, bott_low, gdp_high, gdp_low, agri_high, agri_low)

#create dataset for histograms
data_b <- read_csv("data/data_bottom_different_q.csv")

hist_high<- data_b %>% filter(agri_top20==1)
hist_low<- data_b %>% filter(agri_top20==0)

tidybayes::histogram_bins(hist_high$UDel_precip_popweight, n = 50) -> hist_data_high
tidybayes::histogram_bins(hist_low$UDel_precip_popweight, n = 50) -> hist_data_low

hist_data_high$intensity <- "high"
hist_data_low$intensity <- "low"

hist_data1 <- hist_data_high
hist_data1$variable <- "bot50"
hist_data2 <- hist_data_low
hist_data2$variable <- "bot50"
hist_data3 <- hist_data_high
hist_data3$variable <- "gdp"
hist_data4 <- hist_data_low
hist_data4$variable <- "gdp"
hist_data5 <- hist_data_high
hist_data5$variable <- "agri"
hist_data6 <- hist_data_low
hist_data6$variable <- "agri"

hist_data <- rbind(hist_data1,hist_data2,hist_data3,hist_data4,hist_data5,hist_data6)

fig1_left_data %<>% 
  mutate(
    variable = factor(
      variable,
      levels = c("bot50", "gdp", "agri"),
      labels = c("Bottom 50% income share", "GDP per capita growth rate", "Agricultural GDP per capita growth rate")
    ),
    intensity = factor(
      intensity,
      levels = c("high", "low"),
      labels = c("High agricultural intensity","Low agricultural intensity")
    )
  )

hist_data %<>% 
  mutate(
    variable = factor(
      variable,
      levels = c("bot50", "gdp", "agri"),
      labels = c("Bottom 50% income share", "GDP per capita growth rate", "Agricultural GDP per capita growth rate")
    ),
    intensity = factor(
      intensity,
      levels = c("high", "low"),
      labels = c("High agricultural intensity","Low agricultural intensity")
    )
  )


#function for main graph
main_plot <- function(var, pal){
  fig1_left_data %>%
    filter(x<31) %>%
    filter(variable==var) %>% 
    ggplot()+
    geom_line(aes(x=x/10 , y=estimate, color=intensity, linetype=intensity), size=1) +
    facet_wrap(~variable, scales = "free") +
    geom_ribbon(aes(x=x/10, ymax=max90, ymin=min90, fill=intensity), alpha=.3)  +
    scale_colour_manual(
      values = pal,
      aesthetics = c("colour", "fill")
    ) -> out
  return(out)
}


#function to retrieve data to rescale histograms
retrieve_hist <- function(plot, var, varcode){
  rbind(
    ggplot_build(plot)$layout$panel_params[[1]]$y.range
  ) -> rescale_data
  
  ggplot_build(plot) -> built_plot
  
  rescale_data <- as_tibble(cbind(rescale_data,
                                  as.character(built_plot[["layout"]][["layout"]][["variable"]])
  ))
  
  colnames(rescale_data) <- c("min", "max", "variable")
  
  rescale_data$min <- as.numeric(rescale_data$min)
  rescale_data$max <- as.numeric(rescale_data$max)
  
  #transform to factors
  rescale_data %<>% 
    mutate(
      variable = fct_recode(variable, !!varcode := var),
      variable = factor(
        variable,
        levels = c(varcode),
        labels = c(var)
      )
    ) 
  
  #distinguish by agricultural intensity
  hist_data_high <- hist_data %>% 
    filter(intensity=="High agricultural intensity") %>% 
    filter(variable==var) 
  hist_data_low <- hist_data %>% 
    filter(intensity=="Low agricultural intensity") %>% 
    filter(variable==var) 
  
  hist_data_high <-left_join(hist_data_high, rescale_data, by=c("variable"))
  hist_data_low <-left_join(hist_data_low, rescale_data, by=c("variable"))
  
  hist_data_high %<>% 
    mutate(
      range = max - min,
      rescaled_max = min + (density*(range*0.1))
    )
  
  hist_data_low %<>% 
    mutate(
      range = max - min,
      rescaled_min = min - range*0.2,
      rescaled_max = rescaled_min + (density*(range*0.1))
    )
  
  out <- list(hist_data_high, hist_data_low)
  return(out)
}



#### Chart Parabolas - Figure 1 ####

# # # # # # # # # # # # # # # bottom 50 # # # # # # # # # # # # # # #
pal_bot <- c(
  "High agricultural intensity"="#E58601",
  "Low agricultural intensity" ="#46ACC8"
)


bot <- main_plot("Bottom 50% income share", pal = pal_bot)
hist_bot <- retrieve_hist(bot, "Bottom 50% income share", "bot50")

bot +
  geom_rect(
    data = hist_bot[[1]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=min, ymax=rescaled_max),
    colour = pal_bot[1], fill = pal_bot[1], alpha=0.4
  ) +
  geom_rect(
    data = hist_bot[[2]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=rescaled_min, ymax=rescaled_max),
    colour = pal_bot[2], fill = pal_bot[2], alpha=0.4
  ) +
  scale_y_continuous(labels=scales::percent_format(0.1), breaks = c(0.125, 0.15, 0.175, 0.2)) +
  theme_minimal() +
  xlab("Yearly precipitation") +
  theme(
    panel.grid=element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) -> bot_hist

hist_bot[[1]] %>% 
  summarise(mean = mean((rescaled_max + min)/2)) %>% as.numeric() -> pos_lab_high
hist_bot[[2]] %>% 
  summarise(mean = mean((rescaled_max + min)/2)) %>% as.numeric() -> pos_lab_low

segment_data <- data.frame(
  start = c(1.11, 1.03, 0.46, 1.09, 0.92, 0.88, 0.72, 1.59, 0.9, 0.73, 0.6),
  end = c(1.14, 1.1, 0.47, 1.13, 1.05, 1.05, 0.74, 1.54, 0.94, 0.75, 0.7),
  ymin = 0.12,
  ymax = 0.196,
  name = c("Rwanda", "Burundi", "Niger", "Etiopia", "Malawi", "Zambia", "Mali", "Madagascar", "Mozambico", "Chad", "Zimbabwe")
)


segment_data %>% 
  filter(name %in% c("Zambia", "Malawi", "Burundi")) -> segment_data_1
segment_data_1 <- segment_data_1[order(segment_data_1$name, decreasing = T),]
segment_data_1$yarr <- c(0.19, 0.185, 0.181)
segment_data_1$ytext <- c(0.194, 0.185, 0.181)
segment_data_1$xtext <- c(0.93, 0.91, 1.02)


segment_data %>% 
  filter(name %in% c("Zimbabwe")) -> segment_data_2
segment_data_2$yarr <- c(0.194)

segment_data %>% 
  filter(name %in% c("Madagascar")) -> segment_data_3
segment_data_3$yarr <- c(0.194)


bot_hist +
  annotate(geom = "text", x = 1.9, y = pos_lab_high*1.07,
           label = "High agricultural intensity", hjust = "center", size=3) +
  annotate(geom = "text", x = 1.9, y = pos_lab_low*0.96,
           label = "Low agricultural intensity", hjust = "center", size=3) +
  geom_segment(data=segment_data_1 %>% filter(name!="Malawi"), aes(x=end, xend = end, y=ymin, yend=ymax), size=0.3) +
  geom_segment(data=segment_data_1, aes(x=start, xend = end, y=yarr, yend=yarr), size=0.3, arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  geom_text(data=segment_data_1, aes(x=xtext, y=ytext, label=name), size=2.3, hjust=1) +
  
  geom_segment(data=segment_data_2, aes(x=end, xend = end, y=ymin, yend=ymax), size=0.3) +
  geom_segment(data=segment_data_2, aes(x=start, xend = end, y=yarr, yend=yarr), size=0.3, arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  geom_text(data=segment_data_2, aes(x=start-0.14, y=yarr, label=name), size=2.3) +
  
  geom_segment(data=segment_data_3, aes(x=end, xend = end, y=ymin, yend=ymax), size=0.3) +
  geom_segment(data=segment_data_3, aes(x=start, xend = end, y=yarr, yend=yarr), size=0.3, arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  geom_text(data=segment_data_3, aes(x=end+0.22, y=yarr, label=name), size=2.3) +
  theme(legend.position = "none") -> bot_hist

# # # # # # # # # # # # # # # gdp # # # # # # # # # # # # # # #

pal_gdp <- c(
  "High agricultural intensity"="#FD6467",
  "Low agricultural intensity" ="#7294D4"
)

gdp <- main_plot("GDP per capita growth rate", pal = pal_gdp)
hist_gdp <- retrieve_hist(gdp, "GDP per capita growth rate", "gdp")

fig1_left_data %>% 
  filter(variable=="GDP per capita growth rate") %>% 
  filter(intensity=="High agricultural intensity") %>% 
  select(x, estimate) %>% 
  mutate(x=x/10) %>% 
  slice(5)-> pos_gdp_high

fig1_left_data %>% 
  filter(variable=="GDP per capita growth rate") %>% 
  filter(intensity=="Low agricultural intensity") %>% 
  select(x, estimate) %>% 
  mutate(x=x/10) %>% 
  slice(4)-> pos_gdp_low

gdp +
  geom_rect(
    data = hist_gdp[[1]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=min, ymax=rescaled_max),
    colour = pal_gdp[1], fill = pal_gdp[1], alpha=0.4
  ) +
  geom_rect(
    data = hist_gdp[[2]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=rescaled_min, ymax=rescaled_max),
    colour = pal_gdp[2], fill = pal_gdp[2], alpha=0.4
  ) +
  scale_y_continuous(labels=scales::percent_format(0.1), breaks = c(-0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075)) +
  theme_minimal() +
  xlab("Yearly precipitation") +
  annotate(
    geom = "segment", x = pos_gdp_high[,"x"], y = pos_gdp_high[,"estimate"],
    xend = pos_gdp_high[,"x"] + 0.65, yend = pos_gdp_high[,"estimate"] -0.03
  ) +
  annotate(geom = "text", x = pos_gdp_high[,"x"] + 1.25, y = pos_gdp_high[,"estimate"] -0.04,
           label = "High agricultural\nintensity", hjust = "center", size=2.7) + #use size=3 if same size as facet titles
  annotate(
    geom = "segment", x = pos_gdp_low[,"x"], y = pos_gdp_low[,"estimate"],
    xend = pos_gdp_low[,"x"] + 0.1, yend = pos_gdp_low[,"estimate"] +0.027
  ) +
  annotate(geom = "text", x = pos_gdp_low[,"x"] + 0.2, y = pos_gdp_low[,"estimate"] +0.05,
           label = "Low agricultural\nintensity", hjust = "center", size=2.7) +
  theme(
    panel.grid=element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) -> gdp_hist

# # # # # # # # # # # # # # # agri # # # # # # # # # # # # # # #
pal_agri <- c(
  "High agricultural intensity"="#C93312",
  "Low agricultural intensity" ="#899DA4"
)

agri <- main_plot("Agricultural GDP per capita growth rate", pal = pal_agri)
hist_agri <- retrieve_hist(agri, "Agricultural GDP per capita growth rate", "agri")

fig1_left_data %>% 
  filter(variable=="Agricultural GDP per capita growth rate") %>% 
  filter(intensity=="High agricultural intensity") %>% 
  select(x, estimate) %>% 
  mutate(x=x/10) %>% 
  slice(5)-> pos_agri_high

fig1_left_data %>% 
  filter(variable=="Agricultural GDP per capita growth rate") %>% 
  filter(intensity=="Low agricultural intensity") %>% 
  select(x, estimate) %>% 
  mutate(x=x/10) %>% 
  slice(4)-> pos_agri_low

agri +
  geom_rect(
    data = hist_agri[[1]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=min, ymax=rescaled_max),
    colour = pal_agri[1], fill = pal_agri[1], alpha=0.4
  ) +
  geom_rect(
    data = hist_agri[[2]] %>% filter(upper<3.1), aes(xmin=lower, xmax=upper, ymin=rescaled_min, ymax=rescaled_max),
    colour = pal_agri[2], fill = pal_agri[2], alpha=0.4
  ) +
  scale_y_continuous(labels=scales::percent_format(1), breaks = c(-0.4, -0.3, -0.2, -0.10, 0, 0.10, 0.2)) +
  theme_minimal() +
  xlab("Yearly precipitation") +
  annotate(
    geom = "segment", x = pos_agri_high[,"x"], y = pos_agri_high[,"estimate"],
    xend = pos_agri_high[,"x"] + 0.65, yend = pos_agri_high[,"estimate"] -0.10
  ) +
  annotate(geom = "text", x = pos_agri_high[,"x"] + 1.25, y = pos_agri_high[,"estimate"] -0.14,
           label = "High agricultural\nintensity", hjust = "center", size=2.7) +
  annotate(
    geom = "segment", x = pos_agri_low[,"x"], y = pos_agri_low[,"estimate"],
    xend = pos_agri_low[,"x"] + 0.1, yend = pos_agri_low[,"estimate"] +0.12
  ) +
  annotate(geom = "text", x = pos_agri_low[,"x"] + 0.2, y = pos_agri_low[,"estimate"] +0.21,
           label = "Low agricultural\nintensity", hjust = "center", size=2.7) +
  theme(
    panel.grid=element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) -> agri_hist



# # # # # # # # # # # # # # # continuous # # # # # # # # # # # # # # #
data_cont_b50<- read_csv("data/data_par_cont_b50.csv")
data_cont_b50 <- data_cont_b50 %>%
  filter(agri_emp=="30"|agri_emp=="50"|agri_emp=="70"|agri_emp=="90")

data_cont_gdp<- read_csv("data/data_par_cont_gdp.csv")
data_cont_gdp<-data_cont_gdp %>%
  filter(agri_emp=="30"|agri_emp=="50"|agri_emp=="70"|agri_emp=="90")

ramp_bot <- colorRampPalette(c(pal_bot[1], pal_bot[2]))
ramp_gdp <- colorRampPalette(c(pal_gdp[1], pal_gdp[2]))

data_cont_b50 %>% 
  mutate(
    agri_emp = factor(
      agri_emp,
      levels = c("30","50","70","90"),
      labels = c("30%","50%","70%","90%")
    )
  ) %>% 
  filter(x<3.1) %>%
  mutate(title="Bottom 50% income share\n(selected agricultural intensities)") %>% 
  ggplot() +
  geom_line(aes(x=x , y=y+0.165, color=as.factor(agri_emp)), size=1) +
  facet_wrap(.~title) +
  scale_y_continuous(labels=scales::percent_format(1)) +
  labs(color="Agricultural intensity") +
  xlab("Yearly precipitation") +
  scale_color_manual(values=rev(ramp_bot(4))) +
  #ylim(0.06,0.22) +
  guides(col = guide_legend(nrow=1)) +
  theme_minimal()+
  theme(
    legend.title = element_text(size=9),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    legend.justification="center",
    legend.title.align = 0.5,
    legend.position=c(0.5,0.1)
  ) -> cont_bot

data_cont_gdp %>%  
  mutate(
    agri_emp = factor(
      agri_emp,
      levels = c("30","50","70","90"),
      labels = c("30%","50%","70%","90%")
    )
  ) %>% 
  filter(x<3.1) %>%
  mutate(title="GDP per capita growth rate\n(selected agricultural intensities)") %>% 
  ggplot() +
  geom_line(aes(x=x , y=y-0.025, color=as.factor(agri_emp)), size=1) +
  facet_wrap(.~title) +
  scale_y_continuous(labels=scales::percent_format(1), breaks = c(-0.1, -0.05, 0, 0.05, 0.1)) +
  labs(color="Agricultural intensity") +
  xlab("Yearly precipitation") +
  scale_color_manual(values=rev(ramp_gdp(4))) +
  #ylim(-0.2,0.14) +
  guides(col = guide_legend(nrow=1)) +
  theme_minimal() +
  theme(
    legend.title = element_text(size=9),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    legend.justification="center",
    legend.title.align = 0.5,
    legend.position=c(0.5,0.1)
  ) -> cont_gdp


cowplot::plot_grid(
  gdp_hist, 
  agri_hist, 
  nrow=1, 
  labels = c("B","C")) -> floor_left_fig1

cowplot::plot_grid(
  bot_hist,
  floor_left_fig1,
  nrow=2, labels = c("A"), rel_heights = c(0.6,0.4)) -> left_fig1

cowplot::plot_grid(
  cont_bot,
  cont_gdp,
  nrow=2, labels = c("D","E")) -> right_fig1

cowplot::plot_grid(
  left_fig1,
  right_fig1, 
  nrow=1, rel_widths = c(0.65,0.35)) -> fig1

ggsave("final_charts/charts/figure1.pdf", fig1, width = 25, height = 15, units = "cm")





