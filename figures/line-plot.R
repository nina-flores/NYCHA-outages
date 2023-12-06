require(sf)
require(tidyverse)
require(dplyr)
require(lubridate)
require(gridExtra)
require(tigris)
require(stringr)
require(ggmap)
require(ggpubr)
require(viridis)
require(RColorBrewer)
require(scales)
require(zoo)
require(data.table)

setwd("~/Desktop/projects/casey cohort/nycha-outages/dashboard building/nycha-outages/data")
data_dev <- st_read( "nycha_outages_development.shp") %>%
  select(tds)%>%
  st_centroid() %>%
  st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs") %>%
  group_by(tds) %>%
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
  filter(year1 %in% c(2020, 2021, 2022)) %>%
  mutate(year_month = format(start_date, "%Y-%m")) %>%
  filter(is.na(trnsfr_) | trnsfr_ == "1/10/2023")

unique(data_build$year_month)



unique_year_month <- unique(data_build$year_month)
group <- rep(1:ceiling(length(unique_year_month)/3), each = 3, length.out = length(unique_year_month))
result <- group[match(data_build$year_month, unique_year_month)] %>% as.data.frame()
data_build$year_qtr <- result[,1]

data_build <- data_build %>%
  mutate(year_qtr = case_when(year_qtr == 1 ~ "2020, Q1",
                              year_qtr == 2 ~ "2020, Q2",
                              year_qtr == 3 ~ "2020, Q3",
                              year_qtr == 4 ~ "2020, Q4",
                              year_qtr == 5 ~ "2021, Q1",
                              year_qtr == 6 ~ "2021, Q2",
                              year_qtr == 7 ~ "2021, Q3",
                              year_qtr == 8 ~ "2021, Q4",
                              year_qtr == 9 ~ "2022, Q1",
                              year_qtr == 10 ~ "2022, Q2",
                              year_qtr == 11~ "2022, Q3",
                              year_qtr == 12 ~ "2022, Q4",
                              ))


data_build_dt = data.table(data_build)

summaryTable_by_type <-  data.table(data_build_dt)[, .(
  `Frequency` =  as.numeric(.N), 
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Median Duration in hours` = as.numeric(round(median(duration, na.rm = TRUE), 2))), by = c("otg_typ","year_month")] %>%
  select( `Frequency`, `Average rate (N/total development population)`,`Median Duration in hours`, Type = otg_typ, year_month) %>%
  arrange((Type))


######
### stacked area plot
#####
summaryTable_by_type$year_month <- as.Date(as.yearmon(summaryTable_by_type$year_month), format = "%Y-%m")

# Reorder the levels of the Type variable for both Median Duration and Frequency
summaryTable_by_type$Type <- fct_reorder(summaryTable_by_type$Type, as.numeric(summaryTable_by_type$`Frequency`), sum)

custom_palette<- c("electric" = "#9D7AAD", "water" = "#EFA8B8", "heat" = "#F4934E", "hot water" = "#B33D4E", "elevator" = "#59234E")




#grandbudapest1_colors <- c("#F1BB7B", "#F4934E", "#D46A6A", "#B33D4E", "#59234E")
#grandbudapest2_colors <- c("#EFA8B8")

# Plot a with custom color palette
a <- summaryTable_by_type %>%
  ggplot( aes(x=year_month, y=Frequency, fill=Type)) +
  geom_area(alpha=0.8 , size=.5) +
  scale_fill_manual(values = custom_palette) +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))+
  ggtitle("a.")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  theme(text = element_text(size=14))

data_build$otg_typ <- factor(data_build$otg_typ, levels = c("electric", "water", "heat", "hot water", "elevator"))
options(scipen = 999)
b <- ggplot(data_build, aes(x = year_qtr, y = duration, fill = otg_typ)) +
  geom_boxplot(size =.3, outlier.size = .5, alpha = .8) +
  labs(title = "b.", y = "Duration (hours)") +
  theme_minimal() +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(.1, 1, 10, 100, 1000), 
                     labels = c("0.1", "1", "10", "100", "1000"))+
  scale_fill_manual(values = custom_palette)+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))+
  xlab("")+
  theme(text = element_text(size=14))

setwd("/Users/ninaflores/Desktop/projects/casey cohort/nycha-outages/manuscript-output")
pdf("time-plot.pdf")
ggarrange(a,b, nrow = 2, common.legend = TRUE)
dev.off()

################################################################################



summaryTable_by_type <-  data.table(data_build_dt)[, .(
  `Frequency` =  as.numeric(.N), 
  `Average rate (N/total development population)` =  as.numeric(round(mean(.N/ (((ttl_ppl/1000)*3)), na.rm = TRUE), 2)), 
  `Median Duration in hours` = as.numeric(round(median(duration, na.rm = TRUE), 2))), by = c("otg_typ","year_month", "planned")] %>%
  select( `Frequency`, `Average rate (N/total development population)`,`Median Duration in hours`, Type = otg_typ, year_month, planned) %>%
  arrange((Type))




summaryTable_by_type$year_month <- as.Date(as.yearmon(summaryTable_by_type$year_month), format = "%Y-%m")

# Reorder the levels of the Type variable for both Median Duration and Frequency
summaryTable_by_type$Type <- fct_reorder(summaryTable_by_type$Type, as.numeric(summaryTable_by_type$Frequency), sum)

custom_palette<- c("electric" = "#9D7AAD", "water" = "#EFA8B8", "heat" = "#F4934E", "hot water" = "#B33D4E", "elevator" = "#59234E")



# Plot a with custom color palette
a <- summaryTable_by_type %>%
  ggplot( aes(x=year_month, y=Frequency, fill=Type)) +
  geom_area(alpha=0.8 , size=.5) +
  scale_fill_manual(values = custom_palette) +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))+
  ggtitle("a.")+ facet_wrap(.~planned) + scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  theme(text = element_text(size=14))



data_build$otg_typ <- factor(data_build$otg_typ, levels = c("electric", "water", "heat", "hot water", "elevator"))


options(scipen = 999)
b <- ggplot(data_build, aes(x = year_qtr, y = duration, fill = otg_typ)) +
  geom_boxplot(size =.2, outlier.size = .3, alpha = .8) +
  labs(title = "b.", y = "Duration (hours)") +
  theme_minimal() +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(.1, 1, 10, 100, 1000), 
                     labels = c("0.1", "1", "10", "100", "1000"))+
  scale_fill_manual(values = custom_palette)+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))+
  xlab("")+ facet_wrap(.~planned)+
  theme(text = element_text(size=14))

pdf("time-plot-by-p.pdf")
ggarrange(a,b, nrow = 2, common.legend = TRUE)
dev.off()

