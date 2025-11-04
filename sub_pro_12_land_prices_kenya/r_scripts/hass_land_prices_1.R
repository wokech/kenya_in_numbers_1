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
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

athi_river_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/athi_river/athi_river_percentile.png", 
       athi_river_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Athi River

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       athi_river_map, width = 12, height = 8, dpi = 300)


################################################################################
# 2) Donholm
################################################################################

# Average Price Data

donholm_avg_price <- all_data_avg_price |>
  filter(location %in% "Donholm") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

donholm_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/donholm/donholm_percentile.png", 
       donholm_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Donholm

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       donholm_map, width = 12, height = 8, dpi = 300)


################################################################################
# 3) Eastleigh
################################################################################

# Average Price Data

eastleigh_avg_price <- all_data_avg_price |>
  filter(location %in% "Eastleigh") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

eastleigh_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/eastleigh/eastleigh_percentile.png", 
       eastleigh_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Eastleigh

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       eastleigh_map, width = 12, height = 8, dpi = 300)

################################################################################
# 4) Gigiri
################################################################################

# Average Price Data

gigiri_avg_price <- all_data_avg_price |>
  filter(location %in% "Gigiri") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

gigiri_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/gigiri/gigiri_percentile.png", 
       gigiri_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Gigiri

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       gigiri_map, width = 12, height = 8, dpi = 300)


################################################################################
# 5) Juja
################################################################################

# Average Price Data

juja_avg_price <- all_data_avg_price |>
  filter(location %in% "Juja") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

juja_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/juja/juja_percentile.png", 
       juja_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Juja

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.08, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       juja_map, width = 12, height = 8, dpi = 300)


################################################################################
# 6) Karen
################################################################################

# Average Price Data

karen_avg_price <- all_data_avg_price |>
  filter(location %in% "Karen") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

karen_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/karen/karen_percentile.png", 
       karen_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Karen

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
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
       karen_map, width = 12, height = 8, dpi = 300)


################################################################################
# 7) Kiambu
################################################################################

# Average Price Data

kiambu_avg_price <- all_data_avg_price |>
  filter(location %in% "Kiambu") |>
  filter(year != c(2025)) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line(size=1, color = "black") +
  geom_point(size=2.5, color = "black") +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
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
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

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
  geom_point(aes(x = quarter_year, y = x25th_percentile),size=2.5, color = "black") +
  geom_point(aes(x = quarter_year, y = x75th_percentile),size=2.5, color = "black") +
  geom_line(aes(x = quarter_year, y = x25th_percentile), color = "black", linewidth = 1) +
  geom_line(aes(x = quarter_year, y = x75th_percentile), color = "black", linewidth = 1) +
  theme_classic() + 
  scale_x_yearqtr(format = "Q%q %Y", n = 10,
                  minor_breaks = seq(from = min(all_data_avg_price$quarter_year), 
                                     to = max(all_data_avg_price$quarter_year), 
                                     by = 0.25)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Price (25th vs 75th Percentile) in KShs\nInterquartile Range",
       x = "Year") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "azure2"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"))

kiambu_percentile 

ggsave("sub_pro_12_land_prices_kenya/images/kiambu/kiambu_percentile.png", 
       kiambu_percentile, width = 12, height = 12, dpi = 300)

# Map of Location

# Kiambu

# Get a map for the Nairobi area
# The upperLeft and lowerRight are given as c(latitude, longitude)
nairobi_map <- openmap(
  upperLeft = c(-1.1, 36.6),   
  lowerRight = c(-1.5, 37.2), 
  type = "osm",
  minNumTiles = 12,  # Increase number of tiles for higher resolution
  #zoom = 13          # Higher zoom level for more detail
)

# Define points for Kiambu
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
       kiambu_map, width = 12, height = 8, dpi = 300)




# FUTURE WORK #

# use max to figure out groupings by average price

max <- all_data_avg_price |> 
  group_by(location) |>
  summarize(max = max(average_price, na.rm = TRUE))

location_1 <- c("Kiserian", "Kitengela", "Athi River", "Juja", "Thika")
location_2 <- c("Limuru", "Ongata Rongai", "Syokimau", "Ruiru", "Ngong")

location_3 <- c("Tigoni", "Mlolongo", "Kiambu", "Karen", "Langata")
location_4 <- c("Donholm", "Ridgeways", "Runda", "Loresho", "Kitisuru", "Ruaka")

location_5 <- c("Nyari", "Muthaiga", "Spring Valley", "Lavington", "Gigiri", "Eastleigh")
location_6 <- c("Kileleshwa", "Riverside", "Parklands", "Kilimani", "Westlands", "Upper Hill")

all_data_avg_price |>
  filter(location %in% location_1) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_2) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_3) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_4) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_5) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 