library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(DT)
library(jsonlite)
library(data.table)
library(shinyWidgets)
library(rsconnect)
library(sf)
library(here)




setwd("~/Desktop/projects/casey cohort/nycha-outages/dashboard building/nycha-outages/data")

data_build <- st_read("nycha_outages_building.shp") %>%
  mutate(start_date = as.POSIXct(rprt_dt, format = "%m/%d/%y\n\n%I:%M%p"),
         end_date = as.POSIXct(end_dat, format = "%m/%d/%y %I:%M %p"),
         duration = difftime(end_date, start_date, units = "hours"),
         duration = as.numeric(duration),
         ttl_ppl = gsub(",", "", ttl_ppl),
         ttl_ppl = as.numeric(ttl_ppl),
         units = as.numeric(units),
         resdnts = as.numeric(resdnts)) %>%
  filter(duration > 0 )%>%
  mutate(borough = case_when(borough == "BROOKLYN" ~ "Brooklyn",
                             borough == "BRONX" ~ "Bronx",
                             borough == "QUEENS" ~ "Queens",
                             borough == "STATEN ISLAND" ~ "Staten Island",
                             borough == "MANHATTAN" ~ "Manhattan")) %>%
  filter(!is.na(borough)) %>%
  mutate(snr_dvl = if_else(is.na(snr_dvl), "NON-SENIOR",snr_dvl )) %>%
  mutate(ct = as.factor(c__2010))%>%
  filter(start_date < "2023-06-01 00:00:00 EDT")%>%
  filter(end_date < "2023-06-01 00:00:00 EDT")%>%
  mutate(year1 = year(start_date)) %>%
  filter(year1 %in% c(2020, 2021, 2022)) %>%
  filter(is.na(trnsfr_) | trnsfr_ == "1/10/2023")


data_build 
unique(data_build$name)


#unique(data_build$name)

data_build_dt <- data.table(data_build)

# overall
summaryTable1 <- data_build_dt[, .(
  Frequency = round(as.numeric(.N/3),0),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 0)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 0)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 0)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 0)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 0)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 0)),
  Type = "All")]%>%
  select(Type, Frequency, `Average rate (N/total development population)`,`Average Duration in hours (sd)`,
         `Median Duration in hours (IQR)`,  `Average # of residents impacted`)


# by outage type

summaryTable_by_type <- data_build_dt[, .(
  Frequency = round(as.numeric(.N/3),0),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 0)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 0)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 0)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 0)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 0)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 0))), by = otg_typ] %>%
   select(Type = otg_typ, Frequency, `Average rate (N/total development population)`,`Average Duration in hours (sd)`, `Median Duration in hours (IQR)`, `Average # of residents impacted`) %>%
  arrange((Type))


# table 1, overall interuption information and by outage type

tab_1 <- rbind(summaryTable1, summaryTable_by_type )



setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(tab_1, "summaryTable1.csv")




summaryTable_by_type_b <- data_build_dt[, .(
  Frequency = round(as.numeric(.N/3),0),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 0)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 0)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 0)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 0)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 0)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 0))), by = c("otg_typ", "borough")] %>%
  select(Type = otg_typ, Borough = borough, Frequency,`Average rate (N/total development population)`, `Average Duration in hours (sd)`, `Median Duration in hours (IQR)`, `Average # of residents impacted`) %>%
  arrange(Borough) %>%
  arrange((Type)) 

setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(summaryTable_by_type_b, "summaryTable2.csv")


summaryTable_by_type_s <- data_build_dt[, .(
  Frequency = round(as.numeric(.N/3),0),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 0)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 0)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 0)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 0)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 0)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 0))), by = c("otg_typ","snr_dvl")]%>%
  rename("Senior development" = "snr_dvl")%>%
  select(Type = otg_typ, "Senior development", Frequency, `Average rate (N/total development population)`,`Average Duration in hours (sd)`, 
         `Median Duration in hours (IQR)`, `Average # of residents impacted`) %>%
  arrange((Type))



setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(summaryTable_by_type_s, "summaryTable3.csv")




summaryTable_by_type_p <- data_build_dt[, .(
  Frequency = as.numeric(.N/3),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 2)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 2)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 2)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 2)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 2))), by = c("otg_typ","planned")]%>%
  select(Type = otg_typ, Planned = planned, Frequency, `Average rate (N/total development population)`,`Average Duration in hours (sd)`, `Median Duration in hours (IQR)`, `Average # of residents impacted`) %>%
  arrange((Type))



setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(summaryTable_by_type_p, "summaryTable4.csv")





summaryTable_by_type_pb <- data_build_dt[, .(
  Frequency = as.numeric(.N/3),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Average Duration in hours (sd)` = paste0(as.numeric(round(mean(duration, na.rm = TRUE), 2)), " (", as.numeric(round(sd(duration, na.rm = TRUE), 2)), ")"),
  `Median Duration in hours (IQR)` = paste0(as.numeric(round(median(duration, na.rm = TRUE), 2)), " (", as.numeric(round(IQR(duration, na.rm = TRUE), 2)), ")"), 
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 2))), by = c("otg_typ","planned", "borough")]%>%
  select(Type = otg_typ, Borough = borough, Planned = planned, Frequency,`Average rate (N/total development population)`, `Average Duration in hours (sd)`, `Median Duration in hours (IQR)`, `Average # of residents impacted`) %>%
  arrange((Type)) %>%
  filter(Type == "electric")




setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(summaryTable_by_type_pb, "summaryTable5.csv")


#### table percent planned by borough

data_build_planned_p <- data_build %>%
  group_by(borough, otg_typ) %>%
  mutate(n_borough = n()) %>%
  group_by(borough, otg_typ, planned) %>%
  mutate(n_borough_planned = n()) %>%
  ungroup() %>%
  mutate(p_planned_borough = round((n_borough_planned / n_borough) *100,1)) %>%
  select(borough, otg_typ, planned, p_planned_borough) %>%
  group_by(borough, otg_typ, planned, p_planned_borough) %>%
  slice(1) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  filter(planned == "Unplanned") %>%
  select(-planned) %>%
  pivot_wider( names_from = otg_typ, values_from = p_planned_borough)


setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
write.csv(data_build_planned_p, "plannedPctTable.csv")


