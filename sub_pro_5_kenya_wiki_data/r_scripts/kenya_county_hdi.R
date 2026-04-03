# Wikipedia - Kenya Counties decided using HDI

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

link <- "https://en.wikipedia.org/wiki/List_of_Kenyan_counties_by_Human_Development_Index"
kenya_county_hdi <- link %>%
  read_html("[class='wikitable sortable']") %>%
  html_table(fill = TRUE)

kenya_county_hdi_table <- kenya_county_hdi[[1]]

write_csv(kenya_county_hdi_table, "sub_pro_5_kenya_wiki_data/datasets/kenya_county_hdi_table.csv")

kenya_county_hdi_table <- read_csv("sub_pro_5_kenya_wiki_data/datasets/kenya_county_hdi_table.csv")

