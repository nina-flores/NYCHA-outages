require(sf)
require(tidyverse)
require(dplyr)
require(lubridate)
require(gridExtra)
require(tigris)
require(stringr)
require(ggmap)
require(ggpubr)
require(wesanderson)
require(data.table)

setwd("~/Desktop/projects/casey cohort/nycha-outages/dashboard building/nycha-outages/data")
data_dev <- st_read( "nycha_outages_development.shp") %>%
  select(tds, name)%>%
  st_centroid() %>%
  st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs") %>%
  group_by(tds, name) %>%
  slice(1)

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

# overall
summaryTable1 <- data.table(data_build_dt)[, .(
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Average Duration in hours` = as.numeric(round(mean(duration, na.rm = TRUE), 2)),
  `Median Duration in hours` = as.numeric(round(median(duration, na.rm = TRUE), 2))
  
), by = tds] %>%
  mutate(Type = "any")


# by outage type
  


summaryTable_by_type <- data_build_dt[, .(
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Average Duration in hours` = as.numeric(round(mean(duration, na.rm = TRUE), 2)),
  `Median Duration in hours` = as.numeric(round(median(duration, na.rm = TRUE), 2))
), by = c("tds","otg_typ")] %>%
  select(tds, `Average rate (N/total development population)`, `Average Duration in hours`,`Median Duration in hours`, Type = otg_typ,) %>%
  arrange((Type))



# join 
tab_1 <- rbind(summaryTable1, summaryTable_by_type )


### join to spatial information
map_data <- left_join(tab_1, data_dev)



### now map



# 1. map the rate for each outage type


# Split the data into a list of data frames based on the outage type
data_filtered_list <- map_data %>%
  split(.$"Type")


# Convert each element in the list to an sf object 
for (i in 1:length(data_filtered_list)) {
  data_filtered_list[[i]] <- st_as_sf(data_filtered_list[[i]])
}


### read in county boundaries


custom_palette<- c("#ffb6c1",  "#F4934E",  "#B33D4E",  "#59234E")



# Get the county boundaries for New York City
nyc_counties <- counties(state = "NY", cb = TRUE) %>%
  filter(NAME %in% c("Kings", "New York", "Queens", "Bronx", "Richmond")) # Filtering for NYC counties


# Create a list of ggplot objects for each outage type
plot_list_rate <- map(data_filtered_list,function(data) {
  typ = unique(data$Type)
  
  ggplot()+
  geom_sf(data = nyc_counties, color = "black", fill = "#fdf5e6") +
   geom_sf(data = data, aes(color = `Average rate (N/total development population)`), size = .8) +
    labs(color = " ") +
    ggtitle((typ))+
    scale_color_gradientn(colors = custom_palette,
                          na.value = "grey50",
                          labels = scales::label_number(accuracy = 2))  +
    theme_void()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom", legend.text=element_text(angle=90,  hjust = .8))
  
  


})



setwd("/Users/ninaflores/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
pdf("frequency-map.pdf")
ggarrange(plotlist = plot_list_rate, nrow = 2, ncol = 3)
dev.off()




# Create a list of ggplot objects for each outage type
plot_list_duration <- map(data_filtered_list,function(data) {
  typ = unique(data$Type)
  
  ggplot()+
    geom_sf(data = nyc_counties, color = "black", fill = "#fdf5e6") +
    geom_sf(data = data, aes(color = `Average Duration in hours`), alpha = .7, size = .8) +
    labs(color = " ") +
    ggtitle((typ))+
    scale_color_gradientn(colors = custom_palette,
                          na.value = "grey50",
                          trans = "log", 
                          labels = scales::label_number(accuracy = 2)) +
    theme_void()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom", legend.text=element_text(angle=90,  hjust = .8))
  
})


setwd("/Users/ninaflores/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
pdf("duration-map.pdf")
ggarrange(plotlist = plot_list_duration, nrow = 2, ncol = 3)
dev.off()

###require(classInt)
###
#### Function to apply Jenks breaks to data
###apply_jenks <- function(data, num_breaks) {
###  jenks <- classIntervals(data, n = num_breaks, style = "jenks")
###  breaks <- jenks$brks
###  return(breaks)
###}
###
#### Create a list of ggplot objects for each outage type with Jenks breaks
###plot_list <- map(data_filtered_list, function(data) {
###  typ <- unique(data$Type)
###  breaks <- apply_jenks(data$`Average Duration in hours`, 5)  # Specify the number of breaks here
###  
###  ggplot() +
###    geom_sf(data = nyc_counties, color = "black", fill = NA) +
###    geom_sf(data = data, aes(color = `Average Duration in hours`), alpha = 0.7, size = 0.8) +
###    labs(color = " ") +
###    ggtitle((typ)) +
###    scale_color_gradientn(
###      colors = custom_palette,
###      breaks = breaks,trans = "log",
###      na.value = "grey50"
###    ) +
###    theme_void() +
###    theme(plot.title = element_text(hjust = 0.5)) +
###    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.text = element_text(size = 8, angle = 90,  hjust = 1))+
###    theme(
###      plot.title = element_text(hjust = 0.5),
###      legend.text = element_text(size = 8),  # Adjust the size as needed
###      legend.key.width = unit(1.2, "cm"),  # Adjust the key width to prevent overlap
###      legend.key.height = unit(0.4, "cm"),  # Adjust the key height to prevent overlap
###      legend.spacing.y = unit(0.2, "cm"),  # Adjust the vertical spacing between legend items
###      legend.margin = margin(0, 0, 0, 0)  # Adjust the margin of the legend
###    ) 
###})
###
###ggarrange(plotlist = plot_list, nrow = 2, ncol = 3)
