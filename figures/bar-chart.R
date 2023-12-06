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
  group_by(ADDRESS, otg_typ, name, start_date, end_date, bin) %>% ### one more removal of duplicates
  filter(resdnts == max(resdnts)) %>%
  ungroup() %>%
  filter(start_date < "2023-06-01 00:00:00 EDT")%>%
  filter(end_date < "2023-06-01 00:00:00 EDT")%>%
  mutate(year1 = year(start_date)) %>%
  filter(year1 %in% c(2020, 2021, 2022))%>%
  filter(is.na(trnsfr_) | trnsfr_ == "1/10/2023")

data_build_dt <- data.table(data_build)


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
  filter(planned == "Unplanned") 


require(forcats)

custom_palette<- c("Bronx" = "#9D7AAD", "Brooklyn" = "#EFA8B8", "Manhattan" = "#F4934E", "Queens" = "#B33D4E", "Staten Island" = "#59234E")



# Grouped
setwd("/Users/ninaflores/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
pdf("barchart-bor.pdf")

ggplot(data_build_planned_p, aes(fill=fct_rev(borough), x=p_planned_borough, y=fct_rev(otg_typ))) + 
  geom_bar(position="dodge", stat="identity", alpha = .8)+ 
  theme_minimal()+
  scale_fill_manual(values = custom_palette, guide = guide_legend(reverse = TRUE))+
  scale_x_continuous(labels = scales::label_percent(scale = 1))+
  xlab("Percent unplanned")+
  ylab("Outage type")+
  labs(fill = "Borough")

dev.off()


