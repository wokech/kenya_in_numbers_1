# KCSE candidate numbers (2006 to 2022)

# (A) Load the relevant libraries

library(readr)
library(readxl)
library(tidyverse)
library(janitor)

# (B) Load the required data

kcse_2 <- read_excel('sub_pro_11_kcpe_kcse/processed_tables/kcse_2.xlsx')
head(kcse_2)
str(kcse_2)

kcse_2_long_wide <- kcse_2 %>%
  pivot_longer(!1, names_to = "Years", values_to = "Count") %>%
  pivot_wider(names_from = Year, values_from = Count) 

kcse_2_long_wide_clean <- kcse_2_long_wide %>%
  clean_names()

kcse_2_long_wide_clean <- kcse_2_long_wide_clean %>% 
  mutate(years = gsub("\\.{3}[0-9]*$","", years)) %>%
  rename(year = years) %>%
  mutate(year = as.numeric(year),
         number = as.numeric(number))

kcse_2_long_wide_clean_male <- kcse_2_long_wide_clean %>%
  filter(gender == "Male")

kcse_2_long_wide_clean_female <- kcse_2_long_wide_clean %>%
  filter(gender == "Female")

kcse_2_long_wide_clean_total <- kcse_2_long_wide_clean %>%
  filter(gender == "Total")

# Bind the male and female tables together

kcse_2_long_wide_clean_male_female <- rbind(kcse_2_long_wide_clean_male, kcse_2_long_wide_clean_female)

# Make a table for Flourish

kcse_2_long_wide_clean_male_female_wide <- kcse_2_long_wide_clean_male_female %>%
  pivot_wider(names_from = year, values_from = number)
