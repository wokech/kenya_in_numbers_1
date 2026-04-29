# Radio searches in Google Trends (2004 - Present)

# Load the required libraries
# install.packages("ggauto")
library(tidyverse)
library(janitor)
library(ggauto)

# Load the required data

df_radio <- read_csv("sub_pro_15_gtrends_kenya/datasets/test_datasets/gtrends_kenyan_radio_search_apr_2026.csv")

df_radio_tidy <- df_radio %>%
  pivot_longer(
    cols = -Time,           # Pivot everything EXCEPT the Date column
    names_to = "Category",  # The name of the new column for your 5 headers
    values_to = "Value"     # The name of the new column for the numbers
  ) |>
  clean_names() |>
  mutate(time = ymd(time))

str(df_radio_tidy$time)



ggplot(df_radio_tidy, aes(x = time, y = value, color = category)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Searches Over Time", y = "Value")



ggplot(df_radio_tidy, aes(x = time, y = value)) +
  geom_line(linewidth = 1) +
  facet_wrap(~category, scales = "free_y") + # 'free_y' allows each plot its own scale
  theme_light()

df_radio_tidy |>
  ggauto(time, value, category)
