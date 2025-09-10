# Kenya IEBC
# Census data analyzed at the subcounty level

## Load the required libraries

library(ggplot2)
library(sf)
library(rKenyaCensus)
library(janitor)
library(tidyverse)

## Load the shapefile and plot the county map

kenya_counties <- st_read("sub_pro_11_shapefile_review/shapefiles/iebc/counties/counties.shp")

ggplot(kenya_counties) + 
  geom_sf(fill = "bisque", linewidth = 0.3, color = "black") + 
  theme_void()


## Load the shapefile and plot the constituency map

kenya_constituencies <- st_read("sub_pro_11_shapefile_review/shapefiles/iebc/constituencies/constituencies.shp")

ggplot(kenya_constituencies) + 
  geom_sf(fill = "bisque", linewidth = 0.15, color = "red") + 
  theme_void()


## Load the shapefile and plot the ward map

kenya_wards <- st_read("sub_pro_11_shapefile_review/shapefiles/iebc/ward_results/ward.results.formatted.shp")

ggplot(kenya_wards) + 
  geom_sf(fill = "bisque", linewidth = 0.05, color = "blue") + 
  theme_void()
