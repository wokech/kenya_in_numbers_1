# KCPE candidates (2006 to 2022)

# (A) Load the relevant libraries

library(readr)
library(readxl)
library(tidyverse)
library(janitor)

# (B) Load the required data

kcpe_1 <- read_excel('sub_pro_11_kcpe_kcse/processed_tables/kcpe_1.xlsx')
head(kcpe_1)
str(kcpe_1)

kcpe_1_long <- kcpe_1 %>%
  pivot_longer(!Year, names_to = "Years", values_to = "Count")

kcpe_1_long_wide <- kcpe_1_long %>%
  pivot_wider(names_from = "Year", values_from = "Count")
