rm(list = ls())
setwd("~/Google Drive/replication_clim_ineq_sub2")

scen <- data_frame(
  rcp = c("85","85","60","45","26"),
  ssp = c("3", "5", "2", "2", "1")
)

t_b50 <- list()
t_gdp <- list()
t_gdp_init <- list()

t_b50[[1]] <- read_delim("final_charts/tables/bot_main.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) 

t_gdp[[1]] <- read_delim("final_charts/tables/gdp_main.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) 

t_gdp_init[[1]] <- read_delim("final_charts/tables/gdp_init_main.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) 

colnames(t_gdp_init[[1]]) <- c("name", "ratio_gdp_prec_temp_init_min","ratio_gdp_prec_temp_init_median","ratio_gdp_prec_temp_init_max")

for(i in 1:nrow(scen)){
  t_b50[[i+1]] <- read_delim(paste0("final_charts/tables/bot_ssp",scen$ssp[i],"_rcp",scen$rcp[i],".csv"), 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  t_gdp[[i+1]] <- read_delim(paste0("final_charts/tables/gdp_ssp",scen$ssp[i],"_rcp",scen$rcp[i],".csv"), 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  t_gdp_init[[i+1]] <- read_delim(paste0("final_charts/tables/gdp_init_ssp",scen$ssp[i],"_rcp",scen$rcp[i],".csv"), 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
}


t_gdp <- map(t_gdp, function(x){
  x %>% 
    mutate(
      across(
        c(min, median, max),
        ~ case_when(. < -99 ~ "<-99", 
                    T  ~ as.character(.))
      ),
      across(
        c(min_, median_, max_),
        ~ case_when(. >1000 ~ ">1000", 
                    T  ~ as.character(.))
      )
    )
})

t_gdp_init <- map(t_gdp_init, function(x){
  x %>% 
    mutate(
      across(
        c(ratio_gdp_prec_temp_init_min, ratio_gdp_prec_temp_init_median, ratio_gdp_prec_temp_init_max),
        ~ case_when(. < -99 ~ "<-99", 
                    . >1000 ~ ">1000",
                    T  ~ as.character(.))
      )
    )
})



# Add empty row to separate
map(t_b50, function(x){
  x %>% add_row()
}) -> t_b50

map(t_gdp, function(x){
  x %>% add_row()
}) -> t_gdp

map(t_gdp_init, function(x){
  x %>% add_row()
}) -> t_gdp_init

#put toghether
t_b50 <- data.table::rbindlist(t_b50) %>% as.data.frame()
t_gdp <- data.table::rbindlist(t_gdp) %>% as.data.frame()
t_gdp_init <- data.table::rbindlist(t_gdp_init) %>% as.data.frame()

write.table(t_b50, "final_charts/tables/xl_bot.csv" , sep = ";", row.names = F)
write.table(t_gdp, "final_charts/tables/xl_gdp.csv" , sep = ";", row.names = F)
write.table(t_gdp_init, "final_charts/tables/xl_gdp_init.csv" , sep = ";", row.names = F)




