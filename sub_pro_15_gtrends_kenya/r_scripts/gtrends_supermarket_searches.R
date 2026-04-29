# Kenyan supermarket searches in Google Trends (2004 - Present)

# Load the required libraries
# install.packages("ggauto")
library(tidyverse)
library(janitor)
library(ggauto)

# Load the required data

df_supermarket <- read_csv("sub_pro_15_gtrends_kenya/datasets/test_datasets/gtrends_kenyan_supermarkets_apr_2026.csv")

df_supermarket_tidy <- df_supermarket %>%
  pivot_longer(
    cols = -Time,           # Pivot everything EXCEPT the Date column
    names_to = "Category",  # The name of the new column for your 5 headers
    values_to = "Value"     # The name of the new column for the numbers
  ) |>
  clean_names() |>
  mutate(time = dmy(time))

str(df_supermarket_tidy$time)



ggplot(df_supermarket_tidy, aes(x = time, y = value, color = category)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Searches Over Time", y = "Value")



ggplot(df_supermarket_tidy, aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~category, scales = "free_y") + # 'free_y' allows each plot its own scale
  theme_light()

df_supermarket_tidy |>
  ggauto(time, value, category)
