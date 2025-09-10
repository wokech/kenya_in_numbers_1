# Kenya Subcounty Analysis
# Census data analyzed at the subcounty level

## Load the required libraries

library(ggplot2)
library(sf)
library(rKenyaCensus)
library(janitor)
library(tidyverse)

## Load the shapefile and plot the subcounty map

kenya_subcounties <- st_read("sub_pro_11_shapefile_review/shapefiles_subcounty/ke_subcounty.shp")

ggplot(kenya_subcounties) + 
  geom_sf(fill = "bisque", linewidth = 0.3, color = "black") + 
  theme_void()

# Clean the data

kenya_subcounties$subcounty <- gsub("Sub County", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("sub-county", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("Sub- County", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- str_trim(kenya_subcounties$subcounty) 

kenya_subcounties$subcounty[str_detect(kenya_subcounties$subcounty, "ounty")]

# Get census data

df <- V4_T1.9

df_clean <- df |>
  clean_names()

df_clean_new <- df_clean |>
  mutate(county = tools::toTitleCase(tolower(county))) |>
  mutate(sub_county = tools::toTitleCase(tolower(sub_county))) |>
  rename(subcounty = sub_county)

df_clean_new$subcounty[str_detect(df_clean_new$subcounty, "  ")]

########################################################################
# FIND MISMATCHES
########################################################################

setdiff(kenya_subcounties$subcounty, df_clean_new$subcounty)

setdiff(df_clean_new$subcounty, kenya_subcounties$subcounty)

# Find mismatches

census <- df_clean_new |>
  group_by(county) |>
  summarise(count = n())
  
shapefile <- kenya_subcounties |>
  group_by(county) |>
  select(country, county, subcounty) |> 
  summarise(count = n())

#############
setdiff(census$county, shapefile$county)

census$county <- gsub("Elgeyo/Marakwet", "Elgeyo Marakwet", census$county)
census$county <- gsub("Taita/Taveta", "Taita Taveta", census$county)

setdiff(census$county, shapefile$county)
#############

#############
setdiff(shapefile$county, census$county)

shapefile$county <- gsub("Elgeyo-Marakwet", "Elgeyo Marakwet", shapefile$county)
shapefile$county <- gsub("Muranga", "Murang'a", shapefile$county)
shapefile$county <- gsub("Nairobi", "Nairobi City", shapefile$county)

setdiff(shapefile$county, census$county)
#############

merged_cen_shape <- left_join(census, shapefile,
                              by = "county")


merged_cen_shape_similar <- merged_cen_shape |>
  filter(count.x == count.y)

merged_cen_shape_different <- merged_cen_shape |>
  filter(count.x != count.y)

ggplot(kenya_subcounties) + 
  geom_sf(fill = "salmon", linewidth = 0.1, color = "black") + 
  theme_void()
