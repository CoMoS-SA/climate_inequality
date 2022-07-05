rm(list = ls())


library(readr)
library(tidyverse)
library(magrittr)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggallin)

#### Descriptives ####

data <- read_csv("data/data_bottom_different_q.csv")
data$country[is.na(data$country)]<-"NA" #to recognize Namibia

data <- data[,!startsWith(colnames(data), "_y")] #remove unecessary columns

data %<>% filter(year>1979)%>% group_by(country) %>% mutate(mean_prec = mean(UDel_precip_popweight, na.rm=T),
                                                            mean_b50 = mean(bottom50, na.rm=T)) %>% ungroup()

world <- ne_countries(scale = "medium", returnclass = "sf")

small_data <- data[data$year==2007,c("country_iso3","agri_top20","agri_b80", "bottom50","avg_agri_emp","mean_prec","mean_b50")]

small_data %<>% mutate(
    agri_quant = case_when(
        agri_top20==1 ~ 2,
        agri_b80==1 ~ 1
    )
) 

world_agri <- left_join(world, small_data, by=c("iso_a3"="country_iso3"))

world_agri %>% 
    ggplot() +
    theme_light() +
    geom_sf(aes(fill = mean_b50*100)) +
    scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "white", guide = guide_colourbar(direction = "horizontal")) +
    ggtitle("Average Bottom 50 Share (%)") +
    theme(
        plot.title = element_text(hjust = 0.5, size=14),
        legend.title = element_blank(),
        legend.position = c(0.15,0.25),
        legend.key.size = unit(0.45, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid = element_blank()
    ) -> p0

world_agri %>% 
    ggplot() +
    theme_light() +
    geom_sf(aes(fill = avg_agri_emp)) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "white", guide = guide_colourbar(direction = "horizontal")) +
    ggtitle("Agricultural Average Employment (%)") +
    theme(
        plot.title = element_text(hjust = 0.5, size=14),
        legend.title = element_blank(),
        legend.position = c(0.15,0.25),
        legend.key.size = unit(0.45, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid = element_blank()
    ) -> p1


world_agri %>% 
    ggplot() +
    theme_light() +
    geom_sf(aes(fill = mean_prec)) +
    scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "white", guide = guide_colourbar(direction = "horizontal")) +
    ggtitle("Average precipitations (in meters)") +
    theme(
        plot.title = element_text(hjust = 0.5, size=14),
        legend.title = element_blank(),
        legend.position = c(0.15,0.25),
        legend.key.size = unit(0.45, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid = element_blank()
    ) -> p2

data%>%filter(agri_top20==1)%>%mutate(min_top20=min(avg_agri_emp))%>%select(min_top20)%>%distinct(min_top20)
data%>%filter(agri_top25==1)%>%mutate(min_top25=min(avg_agri_emp))%>%select(min_top25)%>%distinct(min_top25)
data%>%filter(agri_top10==1)%>%mutate(min_top10=min(avg_agri_emp))%>%select(min_top10)%>%distinct(min_top10)

data%>%distinct(country,.keep_all=T)%>%ggplot() + geom_density(aes(x=avg_agri_emp), fill="peachpuff1",color="peachpuff1", alpha=0.6)+ xlab("")+ ylab("Density")+
    ggtitle("Agricultural intensity (%)")+
    geom_vline(xintercept=63.5, color="#E58601") +
    geom_vline(xintercept=55.2, color="gold2")+
    geom_vline(xintercept=71.6, color="red")+
    theme_light()+ theme(plot.title = element_text(hjust = 0.5,size=14))+
    annotate("text", x=68, y=0.014, label= "Top 20%", color="#E58601")+
    annotate("text", x=50, y=0.017, label= "Top 25%", color="gold2") +
    annotate("text", x=80, y=0.01, label= "Top 10%", color="red")->p4


cowplot::plot_grid(p0,p2,p1,p4,ncol=2,labels=c("A","B","C","D")) -> descriptive #850X400

ggsave("sup_charts/descript.pdf", width = 38, height = 20, units = "cm")


#Table with list of countries in top agricultural quantiles and correspondence ISO2-countryname

check<-data%>%select(country, countryname, agri_top10, agri_top20, agri_top25)%>%distinct(country,.keep_all=T)
check%<>%filter(agri_top25==1)

#library(xtable)
#print(xtable(check, type = "latex"), file = "correspondence.tex")


#### Temp parabola ####
data_b <- read_csv("data/data_bottom_different_q.csv")
data_b$country[is.na(data_b$country)]<-"NA" #to include Nambia

temp_par<- read_csv("stata_data/est_response_gdp_temp_cont.csv") 

temp_gdp<-temp_par%>%filter(x<31)%>%ggplot() +
    geom_ribbon(aes(x=x, ymax=max90, ymin=min90), fill="pink", alpha=.5)+
    geom_line(aes(x=x , y=estimate), size=1.2, color="black") + xlab("Yearly Temperature (°)") + ylab("pc GDP g")+ theme_light()+
    geom_rug(data=data_b, aes(x = UDel_temp_popweight), inherit.aes = F, colour="darkred") 

ggsave("sup_charts/temp_par.pdf", temp_gdp, width = 10, height = 10, units = "cm")


#### parabolas cont. specification ####

data_par_cont_gdp<- read_csv("data/data_par_cont_gdp.csv")
data_par_cont_b50<- read_csv("data/data_par_cont_b50.csv")

for (i in unique(data_par_cont_gdp$agri_emp)){
    fig_gdp_i <-data_par_cont_gdp%>%filter(agri_emp==i)%>%ggplot()+geom_line(aes(x=x , y=y-0.025), size=1.2) + ylab("")+ xlab("")+
        scale_color_brewer(palette="Blues")+ylim(-0.2,0.14)+ theme_light()+
        theme(legend.justification=c(-0.1,-0.1),legend.position=c(0,0),plot.title = element_text(hjust = 0.5))
    fig_gdp_i<-fig_gdp_i + ggtitle(paste0(i, "% Agri. Intensity"))
    eval(parse(text=paste0("fig_gdp_",i,"<-fig_gdp_i")))
}

for (i in unique(data_par_cont_b50$agri_emp)){
    paste("cont_fig", i, sep = "_")
    fig_b50_i <-data_par_cont_b50%>%filter(agri_emp==i)%>%ggplot()+geom_line(aes(x=x , y=y+0.165), size=1.2) + ylab("")+ xlab("")+
        scale_color_brewer(palette="Reds") +ylim(0.06,0.22) + theme_light()+
        theme(legend.justification=c(-0.1,-0.1),legend.position=c(0,0),plot.title = element_text(hjust = 0.5))
    fig_b50_i<-fig_b50_i + ggtitle(paste0(i, "% Agri. Intensity"))
    eval(parse(text=paste0("fig_b50_",i,"<-fig_b50_i")))
}


fig_b50_40<- fig_b50_40+ylab("Bottom 50% share")+theme(axis.title.y=element_text(size=14))
cont_parabolas_b50<-cowplot::plot_grid(fig_b50_10,fig_b50_20,fig_b50_30,fig_b50_40,fig_b50_50,fig_b50_60,fig_b50_70,fig_b50_80,fig_b50_90,ncol = 3, nrow=3)
cont_parabolas_b50<-ggdraw(add_sub(cont_parabolas_b50, "Precipitations", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

ggsave("sup_charts/cont_par_b50.pdf", cont_parabolas_b50, width = 20, height = 15, units = "cm")

fig_gdp_40<- fig_gdp_40+ylab("GDP per capita growth")+theme(axis.title.y=element_text(size=14))
cont_parabolas_gdp<-cowplot::plot_grid(fig_gdp_10,fig_gdp_20,fig_gdp_30,fig_gdp_40,fig_gdp_50,fig_gdp_60,fig_gdp_70,fig_gdp_80,fig_gdp_90,ncol = 3, nrow=3)
cont_parabolas_gdp<-ggdraw(add_sub(cont_parabolas_gdp, "Precipitations", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

ggsave("sup_charts/cont_par_gdp.pdf", cont_parabolas_gdp, width = 20, height = 15, units = "cm")




#### marginal effects ####

#All
#pdf(file="sup_charts/mg1.pdf")

plot(1,xlim=c(0,4),ylim=c(-0.2,0.1),type="n",las=1,cex.axis=1.3, xlab="Precipitations", ylab=" ", main="Marginal effect")
lines(c(-1, 0, 1, 2, 3, 4, 5), c(0, 0, 0, 0, 0, 0, 0),type = "c", col="azure4",lwd=2)
resp <- read.csv("stata_data/mg_b50_top10_djossaf.csv")
lines(resp$x/10,resp$estimate,lwd=2)
resp <- read.csv("stata_data/mg_b50_top20_djossaf.csv") 
lines(resp$x/10,resp$estimate,lwd=2, col="red")
resp <- read.csv("stata_data/mg_b50_top25_djossaf.csv") 
lines(resp$x/10,resp$estimate,lwd=2, col="violet")
text(3, -0.15, "Top 10")
text(3.2, -0.08, "Top 20", col="red")
text(3.5, -0.02, "Top 25", col="violet")

#dev.off()

resp20 <- read.csv("stata_data/mg_b50_top20_djossaf.csv") 
resp25 <- read.csv("stata_data/mg_b50_top25_djossaf.csv") 

ggplot()+
    geom_ribbon(aes(x=resp25$x/10, ymax=resp25$max90, ymin=resp25$min90), fill="pink", alpha=.5) + xlab("")+ ylab("") + 
    geom_line(aes(x=resp25$x/10 , y=resp25$estimate), size=1.2, color="#dd1c77") +
    geom_ribbon(aes(x=resp20$x/10, ymax=resp20$max90, ymin=resp20$min90), fill="grey", alpha=.4) + xlab("")+ ylab("") +  labs(title="") +
    geom_line(aes(x=resp20$x/10 , y=resp20$estimate), size=1.2, color="red") +
    theme_light() +
    theme(plot.title = element_text(face="bold",hjust = 0.5),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y = element_text(face="bold", size=14))+
    annotate("text", x = 2.5, y = -0.08, label = "Top 20", col="red", size=10)+
    annotate("text", x = 3.5, y = 0, label = "Top 25", col="violet", size=10)

#ggsave("sup_charts/mg2.pdf", width = 15, height = 10, units = "cm")

###
#Excluding top 10
#pdf(file="sup_charts/mg3.pdf")

plot(1,xlim=c(0,4),ylim=c(-0.15,0.1),type="n",las=1,cex.axis=1.3, xlab="Precipitations", ylab=" ", main="Marginal effect")
lines(c(-1, 0, 1, 2, 3, 4, 5), c(0, 0, 0, 0, 0, 0, 0),type = "c", col="azure4",lwd=2)
resp <- read.csv("stata_data/mg_b50_top10_notop10_djossaf.csv")
lines(resp$x/10,resp$estimate,lwd=2)
resp <- read.csv("stata_data/mg_b50_top15_notop10_djossaf.csv") 
lines(resp$x/10,resp$estimate,lwd=2, col="red")
resp <- read.csv("stata_data/mg_b50_top20_notop10_djossaf.csv") 
lines(resp$x/10,resp$estimate,lwd=2, col="violet")
text(3, -0.07, "Top 10")
text(3.2, -0.03, "Top 15", col="red")
text(3.5, -0.01, "Top 20", col="violet")


#dev.off()

