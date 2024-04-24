#installing packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("plotly")


library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)

#uploading files
unicef_metadata <- read.csv("unicef_metadata.csv") 
unicef_indicator_2 <- read.csv("unicef_indicator_2.csv")

#joining tables

data_join <- full_join(unicef_metadata, unicef_indicator_2, by = c("country", "alpha_2_code", "alpha_3_code"), 
                         relationship = "many-to-many")

#time_series_plot

options(scipen = 999)
timeseries_plot <- data_join %>%
  ggplot(aes(x = year, y = Population, color = country)) +
  geom_line() +
  guides(color = "none") +
  labs(
    title = "Population Dynamics",
    x = "Year",
    y = "Population",
    color = "Country"
  ) +
  theme_classic()

ggplotly(timeseries_plot)


#map_visual

install.packages("maps")
map_world <- (map_data("world"))
1

single <- unicef_indicator_2 %>%
  select(Deliveries_in_a_Health_Facility, country, time_period) %>%
  filter(time_period == 2015)

map_data_join <- left_join(map_world, single, by = c("region" = "country"))

# Continue with map visualization as before
map_vis <- map_data_join %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Deliveries_in_a_Health_Facility)) +
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "orange", na.value = "grey") +
  labs(
    title = "Global Deliveries Map",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    x = "Longitude",
    y = "Latitude",
    fill = "Deliveries_in_a_Health_Facility"
  ) +
  theme_classic()

ggplotly(map_vis) 



#scatter_plot2

scatter_plot<- data_join %>%
  filter((year == 2020)) %>%
  ggplot(
    aes(x= Deliveries_in_a_Health_Facility, y= GDP_per_capita, color = country)) +
  geom_point(alpha = 0.7, size=2) +
  guides(color = "none") +
  labs(
    title = "Maternal Health vs GDP per Capita in 2020",
    x = "Deliveries in a Health Facility",
    y = "GDP per Capita",
    color = "Country"
  ) +
  theme_classic()
ggplotly(scatter_plot)



#barchart

deliveries_2000<- data_join %>%
  filter(year == 2020)
deliveries_by_country <- deliveries_2000 %>%
  group_by(country) %>%
  summarise(total_deliveries = sum(Deliveries_in_a_Health_Facility)) %>%
  arrange(desc(total_deliveries))
top_countries <- head(deliveries_by_country, 15)
bar_chart <- top_countries %>%
  ggplot(aes(x = reorder(country, -total_deliveries), y = total_deliveries, fill = country)) +
  geom_bar(stat = "identity") + 
  guides(fill = "none") +
  labs(
    title = "Top 15 Countries by Total Deliveries in Health Facilities from 2000-2020",
    x = "Country",
    y = "Total Deliveries",
    fill = "Country"
  ) +
  theme_classic() 

ggplotly(bar_chart)





line_chart2<- data_join %>%
  filter(Continent!= "NA") %>%
  ggplot(
    aes(x = year, y = life_exp, color = country)) +
  geom_line() +
  guides(color = "none") +
  facet_wrap(~ Continent, nrow = 1) + 
  scale_x_continuous(breaks = c(2000,2010,2020)) +
  labs(
    x = "Year",
    y = "Life Expectancy",
    title = "Life Expectancy of Continents"
  ) +
  theme_classic()

ggplotly(line_chart2)

