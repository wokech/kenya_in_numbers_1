# Wikipedia - GDP per Capita for Kenyan Counties
# https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_Gross_County_Product

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(treemapify)
library(scales)
library(ggrepel)

# (C) Get the data from Wikipedia

# link <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_Gross_County_Product"
# kenya_county_gdp_per_cap <- link %>%
#   read_html("[class='wikitable sortable']") %>%
#   html_table(fill = TRUE)

# kenya_county_gdp_per_cap_table <- kenya_county_gdp_per_cap[[2]]

# write_csv(kenya_county_gdp_per_cap_table, "sub_pro_5_kenya_wiki_data/datasets/kenya_county_gdp_per_cap_table.csv")

kenya_county_gdp_per_cap_table <- read_csv("sub_pro_5_kenya_wiki_data/datasets/kenya_county_gdp_per_cap_table.csv")


