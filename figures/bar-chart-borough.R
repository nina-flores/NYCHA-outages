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
  group_by(ADDRESS, otg_typ, name, start_date, end_date, bin) %>% 
  
  
  
  
  ### one more removal of duplicates
  filter(resdnts == max(resdnts)) %>%
  ungroup() %>%
  filter(start_date < "2023-06-01 00:00:00 EDT")%>%
  filter(end_date < "2023-06-01 00:00:00 EDT")%>%
  mutate(year1 = year(start_date)) %>%
  filter(year1 %in% c(2020, 2021, 2022)) %>%
  filter(is.na(trnsfr_) | trnsfr_ == "1/10/2023")

### 161487
##unique(dev_wo$development) ### 50 of them
##unique(df$development) ### 60 of them
##
##dev_wo <- df %>%
##  anti_join(data_build, by = c("development" = "name" )) 
##
##rem <- setdiff( unique(df$development), unique(dev_wo$development))
##




data_build_dt <- data.table(data_build)


summaryTable_by_type_b <- data_build_dt[, .(
  Frequency = round(as.numeric(.N/3),1),
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 1)), 
  `Average Duration in hours` = as.numeric(round(mean(duration, na.rm = TRUE), 1)),
  `Median Duration in hours` = as.numeric(round(median(duration, na.rm = TRUE), 1)),
  `Average # of residents impacted` = as.numeric(round(mean(resdnts, na.rm = TRUE), 1))), by = c("otg_typ", "borough")] %>%
  select(Type = otg_typ, Borough = borough, Frequency,`Average rate (N/total development population)`, `Average Duration in hours`, `Median Duration in hours`, `Average # of residents impacted`) %>%
  arrange(Borough) %>%
  arrange((Type))  %>%
  pivot_longer(!c(1:2), names_to = "sum_stat", values_to = "value")







require(forcats)

custom_palette<- c("Bronx" = "#9D7AAD", "Brooklyn" = "#EFA8B8", "Manhattan" = "#F4934E", "Queens" = "#B33D4E", "Staten Island" = "#59234E")

summaryTable_by_type_b$sum_stat <- factor(summaryTable_by_type_b$sum_stat, levels = unique(summaryTable_by_type_b$sum_stat))


# Grouped
ggplot(summaryTable_by_type_b, aes(fill=fct_rev(Borough), x=value, y=fct_rev(Borough))) + 
  geom_bar(stat="identity",alpha = .8)+ 
  theme_minimal()+
  scale_fill_manual(values = custom_palette, guide = guide_legend(reverse = TRUE))+
  labs(fill = " ")+
  ylab(" ")+
  xlab("")+
  facet_wrap(.~sum_stat, scales = "free", ncol = 1)




