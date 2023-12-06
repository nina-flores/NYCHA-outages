require(tidyverse)
require(dplyr)
require(lubridate)
require(fst)
require(gridExtra)

setwd("~/Desktop/projects/casey cohort/nycha-outages/additional article analyses/temperature/data")
elev <- read.fst("dev_tracts.fst")%>%
  filter(otg_typ == "elevator") %>%
  mutate(year1 = year(start_date)) %>%
  filter(year1 %in% c( 2020, 2021, 2022))


expanded_data <- elev %>%
  mutate(
    start_time = floor_date(start_date, unit = "hour"),
    stop_time = floor_date(end_date, unit = "hour")
  ) %>%
  mutate(outage_number = row_number()) %>%
  group_by( name,tds_num, start_time, stop_time, ttl_ppl) %>%
  mutate(date_time_et = map2(start_time, stop_time, ~seq.POSIXt(.x, .y, by = "1 hour"))) %>%
  unnest(date_time_et) %>%
  mutate(outage_indicator = 1)


elev_aggregate <- expanded_data %>%
  select(name, tds_num,date_time_et, outage_indicator, GEOID, ttl_ppl) %>%
  group_by(name,tds_num,date_time_et, GEOID, ttl_ppl) %>%
  summarize(outage_indicator = sum(outage_indicator)) %>%
  mutate(outage_indicator = if_else(outage_indicator >=1,1,0))

### join this with temperature data at the census tract
temperature <- read.fst("nldas-ct-nyc-et.fst") 

elev_temp <- left_join(elev_aggregate, temperature)

### get the number of times this happens at hot temperatures:

elev_temp_agg <- elev_temp %>%
  mutate(co_ind = if_else(temperature >= 32.222 & outage_indicator ==1,1,0)) %>%
  group_by( name, tds_num) %>%
  mutate(total_hour_co = sum(co_ind)) %>%
  select(tds_num,total_hour_co,ttl_ppl )%>%
  slice(1) %>%
  mutate(yearly_total_hour_co = round((total_hour_co/3),1)) %>%
  mutate(total_hour_co_per = round((total_hour_co/3)/ (ttl_ppl/1000),1)) %>%
  ungroup() %>%
  select(name, total_hour_co_per,yearly_total_hour_co ) %>%
  arrange(desc(total_hour_co_per)) 


setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(elev_temp_agg, "heat-elevator.csv")





