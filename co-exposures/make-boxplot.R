require(tidyverse)
require(dplyr)
require(lubridate)
require(fst)
require(gridExtra)
require(ggrepel)

setwd("~/Desktop/projects/casey cohort/nycha-outages/manuscript-output")

hc <- read.csv("heat-cold.csv") %>%
  select(-1) %>%
  mutate(type = "heat outage +\nextreme cold")
hel <- read.csv("heat-electric.csv")%>%
  select(-1) %>%
  mutate(type = "electric outage +\nextreme heat")
hcv <- read.csv("heat-elevator.csv")%>%
  select(-1) %>%
  mutate(type = "elevator outage +\nextreme heat")

data <- rbind(hc, hel, hcv)

custom_palette<- c("heat outage +\nextreme cold" = "#EFA8B8", "electric outage +\nextreme heat" = "#F4934E", "elevator outage +\nextreme heat" = "#B33D4E")


# Identify top 10 values for each facet
top_values <- data %>%
  group_by(type) %>%
  arrange(type, desc(yearly_total_hour_co)) %>%
  slice_head(n = 10) %>%
  ungroup()


# Calculate the average for each type
average_data <- data %>%
  group_by(type) %>%
  summarize(avg_yearly_total_hour_co = mean(yearly_total_hour_co))


text_color<- c("heat outage +\nextreme cold" = "#537072", "electric outage +\nextreme heat" = "#8b3d25", "elevator outage +\nextreme heat" = "#9a7252")


ggplot(data, aes(x = type, y = yearly_total_hour_co, fill = type, alpha = .8)) +
  geom_boxplot(width = .3) +
  geom_text_repel(data = top_values, 
                  aes(label = paste0(name, ": ", round(yearly_total_hour_co, 2)), x = type, y = yearly_total_hour_co, color = type),
                  size = 4,
                  max.overlaps = Inf, nudge_x = .5,
                  direction = "y",
                  vjust = .5,
                  hjust = .5,
                  segment.alpha = 2, alpha = 2) +
  geom_point(data = average_data, aes(x = type, y = avg_yearly_total_hour_co), color = "blue", size = 3, shape = 15) +
  scale_fill_manual(values = custom_palette)  +
  scale_color_manual(values = text_color)+
  ylab("Yearly total overlap hours") + xlab("") +
  theme_minimal() + theme(legend.position = "none") +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(.3, 1, 3, 10, 30), 
                     labels = c("0.3", "1", "3", "10", "30"))+
  theme(text = element_text(size=18))
  




# Identify top 10 values for each facet
top_values <- data %>%
  group_by(type) %>%
  arrange(type, desc(total_hour_co_per)) %>%
  slice_head(n = 10) %>%
  ungroup()
names(data)

# Calculate the average for each type
average_data <- data %>%
  group_by(type) %>%
  summarize(avg_yearly_total_hour_co = mean(total_hour_co_per))


ggplot(data, aes(x = type, y = total_hour_co_per, fill = type, alpha = .8)) +
  geom_boxplot(width = .3) +
  geom_text_repel(data = top_values, 
                  aes(label = paste0(name, ": ", round(total_hour_co_per, 2)), x = type, y = total_hour_co_per, color = type),
                  size =4,
                  max.overlaps = Inf, nudge_x = .5,
                  direction = "y",
                  vjust = .5,
                  hjust = .5,
                  segment.alpha = .5, alpha = 2) +
  geom_point(data = average_data, aes(x = type, y = avg_yearly_total_hour_co), color = "blue", size = 2, shape = 15) +
  scale_fill_manual(values = custom_palette) +
  scale_color_manual(values = text_color)+
  ylab("Yearly total overlap hours per 1,000 population") + xlab("") +
  theme_minimal() + theme(legend.position = "none")+
  scale_y_continuous(trans = 'log10', 
                     breaks = c(.1, 1, 10, 100), 
                     labels = c("0.1", "1", "10", "1000"))+
  theme(text = element_text(size=18))

