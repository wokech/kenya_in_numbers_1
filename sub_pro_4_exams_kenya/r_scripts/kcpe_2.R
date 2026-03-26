# KCPE candidate grades (2006 to 2022)

# (A) Load the relevant libraries

library(readr)
library(readxl)
library(tidyverse)
library(janitor)

# (B) Load the required data

kcpe_2 <- read_excel('sub_pro_11_kcpe_kcse/processed_tables/kcpe_2.xlsx')
head(kcpe_2)
str(kcpe_2)

kcpe_2_long <- kcpe_2 %>%
  pivot_longer(!Year, names_to = "Years", values_to = "Count")

kcpe_2_long_wide <- kcpe_2_long %>%
  pivot_wider(names_from = "Year", values_from = "Count")
