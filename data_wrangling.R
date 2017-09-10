#load the data
library(tidyverse)
library(stringr)
library(lubridate)
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

r <- ext_tracks%>% 
    unite(date, c(year,month,day,hour)) %>% 
    select(storm_id, date, latitude, longitude, starts_with("radius_")) %>% 
    mutate(date = ymd_h(date))

data_ <- r %>% gather(key, value, -storm_id, -date, -latitude, -longitude) %>%
    extract(key, c("wind_speed", "direction"), "([[:alnum:]]+_[[:alnum:]]+)_([[:alnum:]]+)") %>% 
    spread(direction, value) %>% 
    mutate(wind_speed, wind_speed = str_sub(wind_speed, -2, -1))
    
katrina <- data_ %>% filter(storm_id == "AL1205") %>% 
    mutate(longitude = -longitude)

storm_observation <- katrina %>% filter(date == ymd_hms("2005-08-29 12:00:00"))













