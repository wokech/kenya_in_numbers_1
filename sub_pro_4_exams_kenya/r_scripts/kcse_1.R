# KCSE candidates and grades (2006 to 2022)

# (A) Load the relevant libraries

library(readr)
library(readxl)
library(tidyverse)
library(janitor)

# (B) Load the required data

kcse_1 <- read_excel('sub_pro_11_kcpe_kcse/processed_tables/kcse_1.xlsx')
head(kcse_1)
str(kcse_1)

kcse_1_long_wide <- kcse_1 %>%
  pivot_longer(!1, names_to = "Years", values_to = "Count") %>%
  pivot_wider(names_from = Year, values_from = Count) 

kcse_1_long_wide_clean <- kcse_1_long_wide %>%
  clean_names() 

kcse_1_long_wide_clean <- kcse_1_long_wide_clean %>% 
  mutate(years = gsub("\\.{3}[0-9]*$","", years)) %>%
  rename(year = years, 
         a_minus = a_2, 
         b_plus = b, 
         b = b_2, 
         b_minus = b_3, 
         c_plus = c,
         c = c_2, 
         c_minus = c_3,
         d_plus = d,
         d = d_2, 
         d_minus = d_3) %>%
  mutate(year = as.numeric(year),
         a = as.numeric(a),
         a_minus = as.numeric(a_minus), 
         b_plus = as.numeric(b_plus),
         b = as.numeric(b),
         b_minus = as.numeric(b_minus),
         c_plus = as.numeric(c_plus),
         c = as.numeric(c),
         c_minus = as.numeric(c_minus),
         d_plus = as.numeric(d_plus),
         d = as.numeric(d),
         d_minus = as.numeric(d_minus),
         e = as.numeric(e),
         total_a_c = as.numeric(total_a_c),
         total_c_e = as.numeric(total_c_e),
         total_students = as.numeric(total_students),
         percentage_qualified = round(as.numeric(percentage_qualified, 0)))

# Rename with appropriate grades

kcse_1_long_wide_clean <- kcse_1_long_wide_clean %>%
  rename(Year = year,
         A = a,
         A_minus = a_minus, 
         B_plus = b_plus, 
         B = b, 
         B_minus = b_minus, 
         C_plus = c_plus,
         C = c, 
         C_minus = c_minus,
         D_plus = d_plus,
         D = d, 
         D_minus = d_minus,
         E = e,
         Total_A_to_C_plus = total_a_c,
         Total_C_to_E = total_c_e,
         Total_Students = total_students, 
         Percent_Students_Qualified = percentage_qualified)

kcse_1_long_wide_clean_male <- kcse_1_long_wide_clean %>%
  filter(gender == "Male")

kcse_1_long_wide_clean_female <- kcse_1_long_wide_clean %>%
  filter(gender == "Female")

kcse_1_long_wide_clean_total <- kcse_1_long_wide_clean %>%
  filter(gender == "Total")

# Flourish Data

kcse_1_long_wide_clean_2 <- kcse_1_long_wide_clean %>%
  pivot_longer(!c(Year, gender), names_to = "Grades", values_to = "Number") %>% 
  pivot_wider(names_from = Year, values_from = Number) 

# 1) Filter Rows with Percent Students Qualified

percent_students_qualified <- kcse_1_long_wide_clean_2 %>%
  filter(grepl('Percent_Students_Qualified', Grades))

