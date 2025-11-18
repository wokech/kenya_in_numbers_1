# Hass Consult Nairobi Land Price Analysis
# Single images of each neighborhood - Map and Line Plot

library(tidyverse)
library(janitor)
library(readxl)
library(scales)
library(devtools)
library(ggrepel)
library(zoo)
library(here)
library(xlsx)
library(leaflet)
library(tmap)
library(sf)
library(rnaturalearth)
library(OpenStreetMap)

################################################################################
# Preliminary Data Wrangling
################################################################################

# 1) Load the required data

suburbs <- read_excel("sub_pro_12_land_prices_kenya/datasets/hass_suburbs_combined_2015_to_20XX.xlsx")
satellite <- read_excel("sub_pro_12_land_prices_kenya/datasets/hass_satellite_combined_2015_to_20XX.xlsx")
locations <- read_excel("sub_pro_12_land_prices_kenya/datasets/all_data_locations.xlsx")

# 2) Clean the data

suburbs <- suburbs |>
  clean_names()

satellite <- satellite |>
  clean_names()

all_data <- rbind(suburbs, satellite)

all_data <- all_data |>
  mutate(quarter_double = 2 * quarter) |>
  mutate(quarter_year = paste(year, quarter_double, sep = ".")) |>
  mutate(`Quarter and Year` = paste0("Q", quarter, " ", year))

all_data$average_price <- as.numeric(all_data$average_price)
all_data$x25th_percentile <- as.numeric(all_data$x25th_percentile)
all_data$x75th_percentile <- as.numeric(all_data$x75th_percentile)
all_data$quarter_year <- as.yearqtr(as.numeric(all_data$quarter_year))

# For the plot
all_data_avg_price <- all_data |>
  select(location, quarter_year, year, quarter, average_price, `Quarter and Year`)

all_data_percentile_price <- all_data |>
  select(location, quarter_year, year, x25th_percentile, x75th_percentile, `Quarter and Year`)

# For the data table
all_data_avg_price_data <- all_data |>
  select(Location = location, Year = year, Quarter = quarter, "Average Price (KShs)" = average_price, `Quarter and Year`)

# Check data types 

str(all_data_avg_price$quarter_year)
str(all_data_percentile_price)
str(all_data_avg_price_data)


################################################################################
# Visualization
################################################################################

################################################################################
# 1) Athi River
################################################################################

# Average Price Data

athi_river_avg_price <- all_data_avg_price |>
  filter(location %in% "Athi River") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

athi_river_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/athi_river/athi_river_avg_price.png", 
       athi_river_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

athi_river_percentile <- all_data_percentile_price |>
  filter(location %in% "Athi River") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),              
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

athi_river_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/athi_river/athi_river_percentile.png", 
       athi_river_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Athi River

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.42, 36.94),   
  lowerRight = c(-1.47, 36.99), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Athi River
points_df <- data.frame(
  lat = c(-1.450226), 
  lon = c(36.968365)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
athi_river_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

athi_river_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/athi_river/athi_river_map.png", 
       athi_river_map, width = 12, height = 12, dpi = 300)


################################################################################
# 2) Donholm
################################################################################

# Average Price Data

donholm_avg_price <- all_data_avg_price |>
  filter(location %in% "Donholm") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

donholm_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/donholm/donholm_avg_price.png", 
       donholm_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

donholm_percentile <- all_data_percentile_price |>
  filter(location %in% "Donholm") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

donholm_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/donholm/donholm_percentile.png", 
       donholm_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Donholm

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.27, 36.86),   
  lowerRight = c(-1.32, 36.91), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Donholm
points_df <- data.frame(
  lat = c(-1.300778), 
  lon = c(36.89031)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
donholm_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

donholm_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/donholm/donholm_map.png", 
       donholm_map, width = 12, height = 12, dpi = 300)


################################################################################
# 3) Eastleigh
################################################################################

# Average Price Data

eastleigh_avg_price <- all_data_avg_price |>
  filter(location %in% "Eastleigh") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

eastleigh_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/eastleigh/eastleigh_avg_price.png", 
       eastleigh_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

eastleigh_percentile <- all_data_percentile_price |>
  filter(location %in% "Eastleigh") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

eastleigh_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/eastleigh/eastleigh_percentile.png", 
       eastleigh_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Eastleigh

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.25, 36.82),   
  lowerRight = c(-1.30, 36.87), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Eastleigh
points_df <- data.frame(
  lat = c(-1.277936), 
  lon = c(36.849115)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
eastleigh_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

eastleigh_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/eastleigh/eastleigh_map.png", 
       eastleigh_map, width = 12, height = 12, dpi = 300)

################################################################################
# 4) Gigiri
################################################################################

# Average Price Data

gigiri_avg_price <- all_data_avg_price |>
  filter(location %in% "Gigiri") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

gigiri_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/gigiri/gigiri_avg_price.png", 
       gigiri_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

gigiri_percentile <- all_data_percentile_price |>
  filter(location %in% "Gigiri") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

gigiri_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/gigiri/gigiri_percentile.png", 
       gigiri_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Gigiri

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.21, 36.78),   
  lowerRight = c(-1.26, 36.83), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Gigiri
points_df <- data.frame(
  lat = c(-1.233554), 
  lon = c(36.807066)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
gigiri_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

gigiri_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/gigiri/gigiri_map.png", 
       gigiri_map, width = 12, height = 12, dpi = 300)


################################################################################
# 5) Juja
################################################################################

# Average Price Data

juja_avg_price <- all_data_avg_price |>
  filter(location %in% "Juja") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

juja_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/juja/juja_avg_price.png", 
       juja_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

juja_percentile <- all_data_percentile_price |>
  filter(location %in% "Juja") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

juja_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/juja/juja_percentile.png", 
       juja_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Juja

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.08, 36.98),   
  lowerRight = c(-1.13, 37.03), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Juja
points_df <- data.frame(
  lat = c(-1.104930), 
  lon = c(37.015869)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
juja_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

juja_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/juja/juja_map.png", 
       juja_map, width = 12, height = 12, dpi = 300)


################################################################################
# 6) Karen
################################################################################

# Average Price Data

karen_avg_price <- all_data_avg_price |>
  filter(location %in% "Karen") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

karen_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/karen/karen_avg_price.png", 
       karen_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

karen_percentile <- all_data_percentile_price |>
  filter(location %in% "Karen") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

karen_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/karen/karen_percentile.png", 
       karen_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Karen

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.30, 36.68),   
  lowerRight = c(-1.35, 36.73), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Karen
points_df <- data.frame(
  lat = c(-1.322960), 
  lon = c(36.706677)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
karen_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

karen_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/karen/karen_map.png", 
       karen_map, width = 12, height = 12, dpi = 300)


################################################################################
# 7) Kiambu
################################################################################

# Average Price Data

kiambu_avg_price <- all_data_avg_price |>
  filter(location %in% "Kiambu") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kiambu_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kiambu/kiambu_avg_price.png", 
       kiambu_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kiambu_percentile <- all_data_percentile_price |>
  filter(location %in% "Kiambu") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kiambu_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kiambu/kiambu_percentile.png", 
       kiambu_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kiambu

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.14, 36.80),   
  lowerRight = c(-1.19, 36.85), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kiambu
points_df <- data.frame(
  lat = c(-1.16724), 
  lon = c(36.8255)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kiambu_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kiambu_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kiambu/kiambu_map.png", 
       kiambu_map, width = 12, height = 12, dpi = 300)


################################################################################
# 8) Kileleshwa
################################################################################

# Average Price Data

kileleshwa_avg_price <- all_data_avg_price |>
  filter(location %in% "Kileleshwa") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kileleshwa_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kileleshwa/kileleshwa_avg_price.png", 
       kileleshwa_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kileleshwa_percentile <- all_data_percentile_price |>
  filter(location %in% "Kileleshwa") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kileleshwa_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kileleshwa/kileleshwa_percentile.png", 
       kileleshwa_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kileleshwa

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.26, 36.76),   
  lowerRight = c(-1.31, 36.81), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kileleshwa
points_df <- data.frame(
  lat = c(-1.277413), 
  lon = c(36.784903)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kileleshwa_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kileleshwa_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kileleshwa/kileleshwa_map.png", 
       kileleshwa_map, width = 12, height = 12, dpi = 300)


################################################################################
# 9) Kilimani
################################################################################

# Average Price Data

kilimani_avg_price <- all_data_avg_price |>
  filter(location %in% "Kilimani") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kilimani_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kilimani/kilimani_avg_price.png", 
       kilimani_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kilimani_percentile <- all_data_percentile_price |>
  filter(location %in% "Kilimani") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kilimani_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kilimani/kilimani_percentile.png", 
       kilimani_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kilimani

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.26, 36.76),   
  lowerRight = c(-1.31, 36.81), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kilimani
points_df <- data.frame(
  lat = c(-1.287856), 
  lon = c(36.784508)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kilimani_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kilimani_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kilimani/kilimani_map.png", 
       kilimani_map, width = 12, height = 12, dpi = 300)


################################################################################
# 10) Kiserian
################################################################################

# Average Price Data

kiserian_avg_price <- all_data_avg_price |>
  filter(location %in% "Kiserian") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kiserian_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kiserian/kiserian_avg_price.png", 
       kiserian_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kiserian_percentile <- all_data_percentile_price |>
  filter(location %in% "Kiserian") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kiserian_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kiserian/kiserian_percentile.png", 
       kiserian_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kiserian

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.40, 36.67),   
  lowerRight = c(-1.45, 36.72), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kiserian
points_df <- data.frame(
  lat = c(-1.430), 
  lon = c(36.687)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kiserian_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kiserian_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kiserian/kiserian_map.png", 
       kiserian_map, width = 12, height = 12, dpi = 300)


################################################################################
# 11) Kitengela
################################################################################

# Average Price Data

kitengela_avg_price <- all_data_avg_price |>
  filter(location %in% "Kitengela") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kitengela_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kitengela/kitengela_avg_price.png", 
       kitengela_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kitengela_percentile <- all_data_percentile_price |>
  filter(location %in% "Kitengela") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kitengela_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kitengela/kitengela_percentile.png", 
       kitengela_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kitengela

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.45, 36.94),   
  lowerRight = c(-1.5, 36.99), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kitengela
points_df <- data.frame(
  lat = c(-1.474469), 
  lon = c(36.959247)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kitengela_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kitengela_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kitengela/kitengela_map.png", 
       kitengela_map, width = 12, height = 12, dpi = 300)


################################################################################
# 12) Kitisuru
################################################################################

# Average Price Data

kitisuru_avg_price <- all_data_avg_price |>
  filter(location %in% "Kitisuru") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kitisuru_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/kitisuru/kitisuru_avg_price.png", 
       kitisuru_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

kitisuru_percentile <- all_data_percentile_price |>
  filter(location %in% "Kitisuru") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

kitisuru_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kitisuru/kitisuru_percentile.png", 
       kitisuru_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kitisuru

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.22, 36.75),   
  lowerRight = c(-1.27, 36.80), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Kitisuru
points_df <- data.frame(
  lat = c(-1.240689), 
  lon = c(36.771137)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
kitisuru_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

kitisuru_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/kitisuru/kitisuru_map.png", 
       kitisuru_map, width = 12, height = 12, dpi = 300)



################################################################################
# 13) Langata
################################################################################

# Average Price Data

langata_avg_price <- all_data_avg_price |>
  filter(location %in% "Langata") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

langata_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/langata/langata_avg_price.png", 
       langata_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

langata_percentile <- all_data_percentile_price |>
  filter(location %in% "Langata") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

langata_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/langata/langata_percentile.png", 
       langata_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Langata

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.31, 36.76),   
  lowerRight = c(-1.36, 36.81), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Langata
points_df <- data.frame(
  lat = c(-1.331581), 
  lon = c(36.781964)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
langata_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

langata_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/langata/langata_map.png", 
       langata_map, width = 12, height = 12, dpi = 300)



################################################################################
# 14) Lavington
################################################################################

# Average Price Data

lavington_avg_price <- all_data_avg_price |>
  filter(location %in% "Lavington") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

lavington_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/lavington/lavington_avg_price.png", 
       lavington_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

lavington_percentile <- all_data_percentile_price |>
  filter(location %in% "Lavington") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

lavington_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/lavington/lavington_percentile.png", 
       lavington_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Lavington

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.24, 36.75),   
  lowerRight = c(-1.29, 36.80), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Lavington
points_df <- data.frame(
  lat = c(-1.269687), 
  lon = c(36.773554)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
lavington_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

lavington_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/lavington/lavington_map.png", 
       lavington_map, width = 12, height = 12, dpi = 300)


################################################################################
# 15) Limuru
################################################################################

# Average Price Data

limuru_avg_price <- all_data_avg_price |>
  filter(location %in% "Limuru") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

limuru_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/limuru/limuru_avg_price.png", 
       limuru_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

limuru_percentile <- all_data_percentile_price |>
  filter(location %in% "Limuru") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

limuru_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/limuru/limuru_percentile.png", 
       limuru_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Limuru

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.09, 36.61),   
  lowerRight = c(-1.14, 36.66), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Limuru
points_df <- data.frame(
  lat = c(-1.115186), 
  lon = c(36.639495)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
limuru_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

limuru_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/limuru/limuru_map.png", 
       limuru_map, width = 12, height = 12, dpi = 300)


################################################################################
# 16) Loresho
################################################################################

# Average Price Data

loresho_avg_price <- all_data_avg_price |>
  filter(location %in% "Loresho") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

loresho_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/loresho/loresho_avg_price.png", 
       loresho_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

loresho_percentile <- all_data_percentile_price |>
  filter(location %in% "Loresho") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

loresho_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/loresho/loresho_percentile.png", 
       loresho_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Loresho

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.23, 36.72),   
  lowerRight = c(-1.28, 36.77), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Loresho
points_df <- data.frame(
  lat = c(-1.260874), 
  lon = c(36.745278)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
loresho_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

loresho_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/loresho/loresho_map.png", 
       loresho_map, width = 12, height = 12, dpi = 300)



################################################################################
# 17) Mlolongo
################################################################################

# Average Price Data

mlolongo_avg_price <- all_data_avg_price |>
  filter(location %in% "Mlolongo") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

mlolongo_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/mlolongo/mlolongo_avg_price.png", 
       mlolongo_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

mlolongo_percentile <- all_data_percentile_price |>
  filter(location %in% "Mlolongo") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

mlolongo_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/mlolongo/mlolongo_percentile.png", 
       mlolongo_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Mlolongo

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.36, 36.92),   
  lowerRight = c(-1.41, 36.97), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Mlolongo
points_df <- data.frame(
  lat = c(-1.394741), 
  lon = c(36.943375)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
mlolongo_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

mlolongo_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/mlolongo/mlolongo_map.png", 
       mlolongo_map, width = 12, height = 12, dpi = 300)


################################################################################
# 18) Muthaiga
################################################################################

# Average Price Data

muthaiga_avg_price <- all_data_avg_price |>
  filter(location %in% "Muthaiga") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

muthaiga_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/muthaiga/muthaiga_avg_price.png", 
       muthaiga_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

muthaiga_percentile <- all_data_percentile_price |>
  filter(location %in% "Muthaiga") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

muthaiga_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/muthaiga/muthaiga_percentile.png", 
       muthaiga_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Muthaiga

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.23, 36.80),   
  lowerRight = c(-1.28, 36.85), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Muthaiga
points_df <- data.frame(
  lat = c(-1.253366), 
  lon = c(36.829985)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
muthaiga_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

muthaiga_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/muthaiga/muthaiga_map.png", 
       muthaiga_map, width = 12, height = 12, dpi = 300)


################################################################################
# 19) Ngong
################################################################################

# Average Price Data

ngong_avg_price <- all_data_avg_price |>
  filter(location %in% "Ngong") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ngong_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/ngong/ngong_avg_price.png", 
       ngong_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

ngong_percentile <- all_data_percentile_price |>
  filter(location %in% "Ngong") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ngong_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/ngong/ngong_percentile.png", 
       ngong_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Ngong

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.34, 36.63),   
  lowerRight = c(-1.39, 36.68), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Ngong
points_df <- data.frame(
  lat = c(-1.362355), 
  lon = c(36.655291)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
ngong_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

ngong_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/ngong/ngong_map.png", 
       ngong_map, width = 12, height = 12, dpi = 300)


################################################################################
# 20) Nyari
################################################################################

# Average Price Data

nyari_avg_price <- all_data_avg_price |>
  filter(location %in% "Nyari") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

nyari_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/nyari/nyari_avg_price.png", 
       nyari_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

nyari_percentile <- all_data_percentile_price |>
  filter(location %in% "Nyari") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

nyari_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/nyari/nyari_percentile.png", 
       nyari_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Nyari

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.20, 36.76),   
  lowerRight = c(-1.25, 36.81), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Nyari
points_df <- data.frame(
  lat = c(-1.229872), 
  lon = c(36.78812)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
nyari_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

nyari_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/nyari/nyari_map.png", 
       nyari_map, width = 12, height = 12, dpi = 300)


################################################################################
# 21) Ongata Rongai
################################################################################

# Average Price Data

ongata_rongai_avg_price <- all_data_avg_price |>
  filter(location %in% "Ongata Rongai") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ongata_rongai_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/ongata_rongai/ongata_rongai_avg_price.png", 
       ongata_rongai_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

ongata_rongai_percentile <- all_data_percentile_price |>
  filter(location %in% "Ongata Rongai") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ongata_rongai_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/ongata_rongai/ongata_rongai_percentile.png", 
       ongata_rongai_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Ongata Rongai

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.38, 36.73),   
  lowerRight = c(-1.43, 36.78), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Ongata Rongai
points_df <- data.frame(
  lat = c(-1.396099), 
  lon = c(36.752015)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
ongata_rongai_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

ongata_rongai_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/ongata_rongai/ongata_rongai_map.png", 
       ongata_rongai_map, width = 12, height = 12, dpi = 300)


################################################################################
# 22) Parklands
################################################################################

# Average Price Data

parklands_avg_price <- all_data_avg_price |>
  filter(location %in% "Parklands") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

parklands_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/parklands/parklands_avg_price.png", 
       parklands_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

parklands_percentile <- all_data_percentile_price |>
  filter(location %in% "Parklands") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

parklands_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/parklands/parklands_percentile.png", 
       parklands_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Parklands

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.24, 36.78),   
  lowerRight = c(-1.29, 36.83), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Parklands
points_df <- data.frame(
  lat = c(-1.266065), 
  lon = c(36.809266)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
parklands_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

parklands_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/parklands/parklands_map.png", 
       parklands_map, width = 12, height = 12, dpi = 300)


################################################################################
# 23) Ridgeways
################################################################################

# Average Price Data

ridgeways_avg_price <- all_data_avg_price |>
  filter(location %in% "Ridgeways") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ridgeways_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/ridgeways/ridgeways_avg_price.png", 
       ridgeways_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

ridgeways_percentile <- all_data_percentile_price |>
  filter(location %in% "Ridgeways") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ridgeways_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/ridgeways/ridgeways_percentile.png", 
       ridgeways_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Ridgeways

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.20, 36.82),   
  lowerRight = c(-1.25, 36.87), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Ridgeways
points_df <- data.frame(
  lat = c(-1.225186), 
  lon = c(36.843057)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
ridgeways_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

ridgeways_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/ridgeways/ridgeways_map.png", 
       ridgeways_map, width = 12, height = 12, dpi = 300)


################################################################################
# 24) Riverside
################################################################################

# Average Price Data

riverside_avg_price <- all_data_avg_price |>
  filter(location %in% "Riverside") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

riverside_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/riverside/riverside_avg_price.png", 
       riverside_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

riverside_percentile <- all_data_percentile_price |>
  filter(location %in% "Riverside") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

riverside_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/riverside/riverside_percentile.png", 
       riverside_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Riverside

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.25, 36.77),   
  lowerRight = c(-1.30, 36.82), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Riverside
points_df <- data.frame(
  lat = c(-1.269312), 
  lon = c(36.79049)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
riverside_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

riverside_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/riverside/riverside_map.png", 
       riverside_map, width = 12, height = 12, dpi = 300)


################################################################################
# 25) Ruaka
################################################################################

# Average Price Data

ruaka_avg_price <- all_data_avg_price |>
  filter(location %in% "Ruaka") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ruaka_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/ruaka/ruaka_avg_price.png", 
       ruaka_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

ruaka_percentile <- all_data_percentile_price |>
  filter(location %in% "Ruaka") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ruaka_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/ruaka/ruaka_percentile.png", 
       ruaka_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Ruaka

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.18, 36.76),   
  lowerRight = c(-1.23, 36.81), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Ruaka
points_df <- data.frame(
  lat = c(-1.205633), 
  lon = c(36.784558)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
ruaka_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

ruaka_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/ruaka/ruaka_map.png", 
       ruaka_map, width = 12, height = 12, dpi = 300)


################################################################################
# 26) Ruiru
################################################################################

# Average Price Data

ruiru_avg_price <- all_data_avg_price |>
  filter(location %in% "Ruiru") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ruiru_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/ruiru/ruiru_avg_price.png", 
       ruiru_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

ruiru_percentile <- all_data_percentile_price |>
  filter(location %in% "Ruiru") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

ruiru_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/ruiru/ruiru_percentile.png", 
       ruiru_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Ruiru

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.12, 36.94),   
  lowerRight = c(-1.17, 36.99), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Ruiru
points_df <- data.frame(
  lat = c(-1.147352), 
  lon = c(36.960546)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
ruiru_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

ruiru_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/ruiru/ruiru_map.png", 
       ruiru_map, width = 12, height = 12, dpi = 300)



################################################################################
# 27) Runda
################################################################################

# Average Price Data

runda_avg_price <- all_data_avg_price |>
  filter(location %in% "Runda") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

runda_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/runda/runda_avg_price.png", 
       runda_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

runda_percentile <- all_data_percentile_price |>
  filter(location %in% "Runda") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

runda_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/runda/runda_percentile.png", 
       runda_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Runda

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.19, 36.78),   
  lowerRight = c(-1.24, 36.83), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Runda
points_df <- data.frame(
  lat = c(-1.217969), 
  lon = c(36.808569)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
runda_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

runda_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/runda/runda_map.png", 
       runda_map, width = 12, height = 12, dpi = 300)


################################################################################
# 28) Spring Valley
################################################################################

# Average Price Data

spring_valley_avg_price <- all_data_avg_price |>
  filter(location %in% "Spring Valley") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

spring_valley_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/spring_valley/spring_valley_avg_price.png", 
       spring_valley_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

spring_valley_percentile <- all_data_percentile_price |>
  filter(location %in% "Spring Valley") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

spring_valley_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/spring_valley/spring_valley_percentile.png", 
       spring_valley_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Spring Valley

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.23, 36.77),   
  lowerRight = c(-1.28, 36.82), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Spring Valley
points_df <- data.frame(
  lat = c(-1.253618), 
  lon = c(36.792591)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
spring_valley_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

spring_valley_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/spring_valley/spring_valley_map.png", 
       spring_valley_map, width = 12, height = 12, dpi = 300)


################################################################################
# 29) Syokimau
################################################################################

# Average Price Data

syokimau_avg_price <- all_data_avg_price |>
  filter(location %in% "Syokimau") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

syokimau_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/syokimau/syokimau_avg_price.png", 
       syokimau_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

syokimau_percentile <- all_data_percentile_price |>
  filter(location %in% "Syokimau") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

syokimau_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/syokimau/syokimau_percentile.png", 
       syokimau_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Syokimau

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.35, 36.90),   
  lowerRight = c(-1.40, 36.95), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Syokimau
points_df <- data.frame(
  lat = c(-1.378052), 
  lon = c(36.928755)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
syokimau_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

syokimau_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/syokimau/syokimau_map.png", 
       syokimau_map, width = 12, height = 12, dpi = 300)



################################################################################
# 30) Thika
################################################################################

# Average Price Data

thika_avg_price <- all_data_avg_price |>
  filter(location %in% "Thika") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

thika_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/thika/thika_avg_price.png", 
       thika_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

thika_percentile <- all_data_percentile_price |>
  filter(location %in% "Thika") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

thika_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/thika/thika_percentile.png", 
       thika_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Thika

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.01, 37.06),   
  lowerRight = c(-1.06, 37.11), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Thika
points_df <- data.frame(
  lat = c(-1.036442), 
  lon = c(37.07898)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
thika_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

thika_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/thika/thika_map.png", 
       thika_map, width = 12, height = 12, dpi = 300)


################################################################################
# 31) Tigoni
################################################################################

# Average Price Data

tigoni_avg_price <- all_data_avg_price |>
  filter(location %in% "Tigoni") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

tigoni_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/tigoni/tigoni_avg_price.png", 
       tigoni_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

tigoni_percentile <- all_data_percentile_price |>
  filter(location %in% "Tigoni") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

tigoni_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/tigoni/tigoni_percentile.png", 
       tigoni_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Tigoni

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.10, 36.66),   
  lowerRight = c(-1.15, 36.71), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Tigoni
points_df <- data.frame(
  lat = c(-1.12966), 
  lon = c(36.684083)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
tigoni_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

tigoni_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/tigoni/tigoni_map.png", 
       tigoni_map, width = 12, height = 12, dpi = 300)


################################################################################
# 32) Upper Hill
################################################################################

# Average Price Data

upper_hill_avg_price <- all_data_avg_price |>
  filter(location %in% "Upperhill") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

upper_hill_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/upper_hill/upper_hill_avg_price.png", 
       upper_hill_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

upper_hill_percentile <- all_data_percentile_price |>
  filter(location %in% "Upperhill") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

upper_hill_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/upper_hill/upper_hill_percentile.png", 
       upper_hill_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Upper Hill

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.28, 36.79),   
  lowerRight = c(-1.33, 36.84), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Upper Hill
points_df <- data.frame(
  lat = c(-1.297918), 
  lon = c(36.815052)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
upper_hill_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

upper_hill_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/upper_hill/upper_hill_map.png", 
       upper_hill_map, width = 12, height = 12, dpi = 300)


################################################################################
# 33) Westlands
################################################################################

# Average Price Data

westlands_avg_price <- all_data_avg_price |>
  filter(location %in% "Westlands") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=2.5, color = "black") +
  geom_point(size=5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Average Price (KShs)",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

westlands_avg_price 

ggsave("sub_pro_12_land_prices_kenya/images/westlands/westlands_avg_price.png", 
       westlands_avg_price, width = 12, height = 12, dpi = 300)

# Percentile Changes

westlands_percentile <- all_data_percentile_price |>
  filter(location %in% "Westlands") |>
  filter(year != c(2025)) |>
  ggplot() +
  geom_ribbon(aes(x=quarter_year, 
                  ymin = x25th_percentile, 
                  ymax = x75th_percentile),
              fill = "goldenrod2", alpha = 0.6) +
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 2.5) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 2.5) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 5,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=28),
        axis.ticks = element_line(linewidth = 1),       
        axis.ticks.length = unit(0.25, "cm"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

westlands_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/westlands/westlands_percentile.png", 
       westlands_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Westlands

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.23, 36.78),   
  lowerRight = c(-1.28, 36.83), 
  type = "osm",
  minNumTiles = 16,  # Increase number of tiles for higher resolution
  zoom = 14          # Higher zoom level for more detail
)

# Define points for Westlands
points_df <- data.frame(
  lat = c(-1.264831), 
  lon = c(36.802054)
)

# Project coordinates
projected_points <- projectMercator(points_df$lat, points_df$lon)
points_df$x <- projected_points[1]
points_df$y <- projected_points[2]
points_df$icon <- c("📍")  # Using emoji as simple icons

# Plot with emoji icons
westlands_map <- autoplot(nairobi_map) +
  geom_text(data = points_df, aes(x, y, label = icon), 
            size = 10, family = "Arial Unicode MS") +
  geom_text(data = points_df, aes(x, y, label = c("")), 
            vjust = 2.5, size = 3, fontface = "bold") +
  labs(title = "") +
  theme_void()

westlands_map

# Save with high resolution
ggsave("sub_pro_12_land_prices_kenya/images/westlands/westlands_map.png", 
       westlands_map, width = 12, height = 12, dpi = 300)


