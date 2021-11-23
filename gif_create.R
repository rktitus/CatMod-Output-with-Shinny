# Installing required Packages
if (!require(dplyr)) install.packages('dplyr') ; library(dplyr)
if (!require(ggplot2)) install.packages('ggplot2') ; library(ggplot2)
if (!require(magick)) install.packages('magick') ; library(magick)
if (!require(purrr)) install.packages('purrr') ; library(purrr)
if (!require(lubridate)) install.packages('lubridate') ; library(lubridate)
if (!require(readxl)) install.packages('readxl') ; library(readxl)
if (!require(tidyr)) install.packages('tidyr') ; library(tidyr)
if (!require(maps)) install.packages('maps') ; library(maps)
if (!require(ggthemes)) install.packages('ggthemes') ; library(ggthemes)
if (!require(ggmap)) install.packages('ggmap') ; library(ggmap)
if (!require(mapdata)) install.packages('mapdata') ; library(mapdata)
if (!require(devtools)) install.packages('devtools') ; library(devtools)
if (!require(gganimate)) install.packages('gganimate') ; library(gganimate)
if (!require(tibble)) install.packages('tibble') ; library(tibble)
if (!require(gifski)) install.packages('gifski') ; library(gifski)
if (!require(viridis)) install.packages('viridis') ; library(viridis)


# Setting prediction start date
prediction_start_dt <- as.Date("01-01-2017","%d-%m-%Y")

#pull data from the file on DropBox
download.file('https://www.dropbox.com/s/vn2j49w16y7q8ay/export_dataframe2.xlsx?raw=1', destfile="input.xlsx", mode="wb")
input_data <- as.data.frame(read_xlsx("input.xlsx", sheet = 1, col_types = "text"))

min_year <- min(as.integer(substr(input_data$BEGIN_YEARMONTH,1,4)))
max_year <- max(as.integer(substr(input_data$BEGIN_YEARMONTH,1,4)))


#plot blank usa map
states <- map_data("state")

usa_map <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray34", color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

########################################################HISTORIC DATA

#flag years / events / kpis present in the data
presense_flags_frame <- input_data %>%
  mutate(
    dummy = 1,
    lat = as.numeric(BEGIN_LAT),
    lon = as.numeric(BEGIN_LON),
    date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
    year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
    INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
    INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
    DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
    DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
  ) %>%
  filter(date < !!prediction_start_dt) %>%
  filter(lon > -130,
         lon < -60,
         lat > 20,
         lat < 50) %>%
  group_by(
    EVENT_TYPE, year
  ) %>%
  summarise(
    Count = n(),
    INJURIES_DIRECT = sum(INJURIES_DIRECT, na.rm = T),
    INJURIES_INDIRECT = sum(INJURIES_INDIRECT, na.rm = T),
    DEATHS_DIRECT = sum(DEATHS_DIRECT, na.rm = T),
    DEATHS_INDIRECT = sum(DEATHS_INDIRECT, na.rm = T)
  ) %>%
  gather(kpi, value, Count:DEATHS_INDIRECT) %>%
  filter(value != 0)

#prepare data for static picture

create_static_pngs <- function(input_year, input_event, input_kpi){
  
  input_data_proc <- input_data %>%
    mutate(
      dummy = 1,
      lat = as.numeric(BEGIN_LAT),
      lon = as.numeric(BEGIN_LON),
      date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
      year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
      INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
      INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
      DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
      DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
    ) %>%
    filter(lon > -130,
           lon < -60,
           lat > 20,
           lat < 50) %>%
    filter(year == !!input_year,
           EVENT_TYPE == !!input_event
    )
  
  #enable different density kpi's
  if(input_kpi == "Injuries direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Injuries indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else {
    do_nothing <- 1 #do nothing for count
  }
  
  static_overlay <- usa_map + 
    stat_density2d(data = input_data_proc, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F,  alpha = .5) +
    scale_fill_viridis(option = 'inferno') +
    guides(fill=FALSE) +
    labs(title = paste0("Year:", input_year))
  
  ggsave(filename = paste0("pngs/", input_event, "_", input_kpi, "_", input_year,".png"),
         plot = static_overlay,
         width = 8,height=5,dpi = 80)
}

dir.create("pngs")

events_vec <- cbind.data.frame(Select_event = unique(input_data$EVENT_TYPE)) 

write.table(events_vec, "events_vec.csv", row.names = FALSE)
write.csv(presense_flags_frame, "presense_flags_frame.csv")

for(i in 1:nrow(presense_flags_frame)){
  create_static_pngs(input_year = presense_flags_frame$year[i], 
                     input_event = presense_flags_frame$EVENT_TYPE[i], 
                     input_kpi = presense_flags_frame$kpi[i]
  )
  print(paste0(i, "/", nrow(presense_flags_frame)))
}

#write empty frames
for(i in min_year:max_year){

  static_overlay <- usa_map + 
    guides(fill=FALSE) +
    labs(title = paste0("Year:", i))
  
  ggsave(filename = paste0("pngs/", "default_frame_", i,".png"),
         plot = static_overlay,
         width = 8,height=5,dpi = 80)
}

########################################################REPEAT FOR PREDICTION DATA
presense_flags_frame_prediction <- input_data %>%
  mutate(
    dummy = 1,
    lat = as.numeric(BEGIN_LAT),
    lon = as.numeric(BEGIN_LON),
    date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
    year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
    INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
    INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
    DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
    DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
  ) %>%
  filter(date >= !!prediction_start_dt) %>%
  mutate(
    mon = ifelse(month(date) < 10, paste0("0",month(date)), month(date)),
    wk = ifelse(week(date) < 10, paste0("0", week(date)), week(date))
  ) %>%
  mutate(
    mon_flag = paste0(year, "_", mon),
    wk_flag = paste0(year, "_", wk)
  ) %>%
  filter(lon > -130,
         lon < -60,
         lat > 20,
         lat < 50) 


presense_flags_frame_prediction_yr <- presense_flags_frame_prediction %>%
  group_by(
    EVENT_TYPE, year
  ) %>%
  summarise(
    min_date = min(date),
    Count = n(),
    INJURIES_DIRECT = sum(INJURIES_DIRECT, na.rm = T),
    INJURIES_INDIRECT = sum(INJURIES_INDIRECT, na.rm = T),
    DEATHS_DIRECT = sum(DEATHS_DIRECT, na.rm = T),
    DEATHS_INDIRECT = sum(DEATHS_INDIRECT, na.rm = T)
  ) %>%
  gather(kpi, value, Count:DEATHS_INDIRECT) %>%
  filter(value != 0)


presense_flags_frame_prediction_mon <- presense_flags_frame_prediction %>%
  group_by(
    EVENT_TYPE, mon_flag
  ) %>%
  summarise(
    min_date = min(date),
    Count = n(),
    INJURIES_DIRECT = sum(INJURIES_DIRECT, na.rm = T),
    INJURIES_INDIRECT = sum(INJURIES_INDIRECT, na.rm = T),
    DEATHS_DIRECT = sum(DEATHS_DIRECT, na.rm = T),
    DEATHS_INDIRECT = sum(DEATHS_INDIRECT, na.rm = T)
  ) %>%
  gather(kpi, value, Count:DEATHS_INDIRECT) %>%
  filter(value != 0)


presense_flags_frame_prediction_wk <- presense_flags_frame_prediction %>%
  group_by(
    EVENT_TYPE, wk_flag
  ) %>%
  summarise(
    min_date = min(date),
    Count = n(),
    INJURIES_DIRECT = sum(INJURIES_DIRECT, na.rm = T),
    INJURIES_INDIRECT = sum(INJURIES_INDIRECT, na.rm = T),
    DEATHS_DIRECT = sum(DEATHS_DIRECT, na.rm = T),
    DEATHS_INDIRECT = sum(DEATHS_INDIRECT, na.rm = T)
  ) %>%
  gather(kpi, value, Count:DEATHS_INDIRECT) %>%
  filter(value != 0)



create_static_pngs_pred_yr <- function(input_year, input_event, input_kpi){
  
  input_data_proc <- input_data %>%
    mutate(
      dummy = 1,
      lat = as.numeric(BEGIN_LAT),
      lon = as.numeric(BEGIN_LON),
      date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
      year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
      INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
      INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
      DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
      DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
    ) %>%
    mutate(
      mon = ifelse(month(date) < 10, paste0("0",month(date)), month(date)),
      wk = ifelse(week(date) < 10, paste0("0", week(date)), week(date))
    ) %>%
    mutate(
      mon_flag = paste0(year, "_", mon),
      wk_flag = paste0(year, "_", wk)
    ) %>%
    filter(lon > -130,
           lon < -60,
           lat > 20,
           lat < 50) %>%
    filter(year == !!input_year,
           EVENT_TYPE == !!input_event
    )
  
  #enable different density kpi's
  if(input_kpi == "Injuries direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Injuries indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else {
    do_nothing <- 1 #do nothing for count
  }
  
  static_overlay <- usa_map + 
    stat_density2d(data = input_data_proc, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F,  alpha = .5) +
    scale_fill_viridis(option = 'inferno') +
    guides(fill=FALSE) +
    labs(title = paste0("Year:", input_year))
  
  ggsave(filename = paste0("pngs_pred_yr/", input_event, "_", input_kpi, "_", input_year,".png"),
         plot = static_overlay,
         width = 8,height=5,dpi = 80)
}

create_static_pngs_pred_mon <- function(input_mon_flag, input_event, input_kpi){
  
  input_data_proc <- input_data %>%
    mutate(
      dummy = 1,
      lat = as.numeric(BEGIN_LAT),
      lon = as.numeric(BEGIN_LON),
      date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
      year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
      INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
      INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
      DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
      DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
    ) %>%
    mutate(
      mon = ifelse(month(date) < 10, paste0("0",month(date)), month(date)),
      wk = ifelse(week(date) < 10, paste0("0", week(date)), week(date))
    ) %>%
    mutate(
      mon_flag = paste0(year, "_", mon),
      wk_flag = paste0(year, "_", wk)
    ) %>%
    filter(lon > -130,
           lon < -60,
           lat > 20,
           lat < 50) %>%
    filter(mon_flag == !!input_mon_flag,
           EVENT_TYPE == !!input_event
    )
  
  #enable different density kpi's
  if(input_kpi == "Injuries direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Injuries indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else {
    do_nothing <- 1 #do nothing for count
  }
  
  static_overlay <- usa_map + 
    stat_density2d(data = input_data_proc, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F,  alpha = .5) +
    scale_fill_viridis(option = 'inferno') +
    guides(fill=FALSE) +
    labs(title = paste0("Year_Month:", input_mon_flag))
  
  ggsave(filename = paste0("pngs_pred_mon/", input_event, "_", input_kpi, "_", input_mon_flag,".png"),
         plot = static_overlay,
         width = 8,height=5,dpi = 80)
}

create_static_pngs_pred_wk <- function(input_wk_flag, input_event, input_kpi){
  
  input_data_proc <- input_data %>%
    mutate(
      dummy = 1,
      lat = as.numeric(BEGIN_LAT),
      lon = as.numeric(BEGIN_LON),
      date = as.Date(paste(BEGIN_DAY, substr(BEGIN_YEARMONTH,5,6), substr(BEGIN_YEARMONTH,1,4), sep = "-"),"%d-%m-%Y"),
      year = as.integer(substr(BEGIN_YEARMONTH,1,4)),
      INJURIES_DIRECT	= as.numeric(INJURIES_DIRECT),
      INJURIES_INDIRECT	= as.numeric(INJURIES_INDIRECT),	
      DEATHS_DIRECT	= as.numeric(DEATHS_DIRECT),	
      DEATHS_INDIRECT	= as.numeric(DEATHS_INDIRECT)
    ) %>%
    mutate(
      mon = ifelse(month(date) < 10, paste0("0",month(date)), month(date)),
      wk = ifelse(week(date) < 10, paste0("0", week(date)), week(date))
    ) %>%
    mutate(
      mon_flag = paste0(year, "_", mon),
      wk_flag = paste0(year, "_", wk)
    ) %>%
    filter(lon > -130,
           lon < -60,
           lat > 20,
           lat < 50) %>%
    filter(wk_flag == !!input_wk_flag,
           EVENT_TYPE == !!input_event
    )
  
  #enable different density kpi's
  if(input_kpi == "Injuries direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Injuries indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(INJURIES_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$INJURIES_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths direct" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_DIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_DIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else if (input_kpi == "Deaths indirect" ){
    
    input_data_proc <-input_data_proc %>%
      filter(DEATHS_INDIRECT > 0)
    
    input_data_proc_upd = as.data.frame(input_data_proc[0,])
    
    if(nrow(input_data_proc) > 0){
      for(i in 1:nrow(input_data_proc)){
        if(input_data_proc$INJURIES_DIRECT[i] > 0){
          input_data_proc_upd <- rbind.data.frame(input_data_proc_upd,
                                                  input_data_proc[rep(i,input_data_proc$DEATHS_INDIRECT[i]),])
        } else {don_thing <- 1}
      }
    } else {don_thing <- 1}
    
    input_data_proc <- input_data_proc_upd
  } else {
    do_nothing <- 1 #do nothing for count
  }
  
  static_overlay <- usa_map + 
    stat_density2d(data = input_data_proc, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F,  alpha = .5) +
    scale_fill_viridis(option = 'inferno') +
    guides(fill=FALSE) +
    labs(title = paste0("Year_Week:", input_wk_flag))
  
  ggsave(filename = paste0("pngs_pred_wk/", input_event, "_", input_kpi, "_", input_wk_flag,".png"),
         plot = static_overlay,
         width = 8,height=5,dpi = 80)
}

dir.create("pngs_pred_yr")
dir.create("pngs_pred_mon")
dir.create("pngs_pred_wk")

events_vec_pred <- cbind.data.frame(Select_event = unique(presense_flags_frame_prediction$EVENT_TYPE)) 

write.table(events_vec_pred, "events_vec_pred.csv", row.names = FALSE)
write.csv(presense_flags_frame_prediction_yr, "presense_flags_frame_prediction_yr.csv")
write.csv(presense_flags_frame_prediction_mon, "presense_flags_frame_prediction_mon.csv")
write.csv(presense_flags_frame_prediction_wk, "presense_flags_frame_prediction_wk.csv")

#year
for(i in 1:nrow(presense_flags_frame_prediction_yr)){
  create_static_pngs_pred_yr(input_year = presense_flags_frame_prediction_yr$year[i], 
                             input_event = presense_flags_frame_prediction_yr$EVENT_TYPE[i], 
                             input_kpi = presense_flags_frame_prediction_yr$kpi[i]
  )
  print(paste0(i, "/", nrow(presense_flags_frame_prediction_yr)))
}

#month
for(i in 1:nrow(presense_flags_frame_prediction_mon)){
  create_static_pngs_pred_mon(input_mon_flag = presense_flags_frame_prediction_mon$mon_flag[i], 
                             input_event = presense_flags_frame_prediction_mon$EVENT_TYPE[i], 
                             input_kpi = presense_flags_frame_prediction_mon$kpi[i]
  )
  print(paste0(i, "/", nrow(presense_flags_frame_prediction_mon)))
}

#week
for(i in 1:nrow(presense_flags_frame_prediction_wk)){
  create_static_pngs_pred_wk(input_wk_flag = presense_flags_frame_prediction_wk$wk_flag[i], 
                              input_event = presense_flags_frame_prediction_wk$EVENT_TYPE[i], 
                              input_kpi = presense_flags_frame_prediction_wk$kpi[i]
  )
  print(paste0(i, "/", nrow(presense_flags_frame_prediction_wk)))
}





















