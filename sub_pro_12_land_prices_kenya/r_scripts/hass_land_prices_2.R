
# FUTURE WORK #

# use max to figure out groupings by average price

max <- all_data_avg_price |> 
  group_by(location) |>
  summarize(max = max(average_price, na.rm = TRUE))

location_1 <- c("Kiserian", "Kitengela", "Athi River", "Juja", "Thika")
location_2 <- c("Limuru", "Ongata Rongai", "Syokimau", "Ruiru", "Ngong")

location_3 <- c("Tigoni", "Mlolongo", "Kiambu", "Karen", "Langata")
location_4 <- c("Donholm", "Ridgeways", "Runda", "Loresho", "Kitisuru", "Ruaka")

location_5 <- c("Nyari", "Muthaiga", "Spring Valley", "Lavington", "Gigiri", "Eastleigh")
location_6 <- c("Kileleshwa", "Riverside", "Parklands", "Kilimani", "Westlands", "Upper Hill")

all_data_avg_price |>
  filter(location %in% location_1) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_2) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_3) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_4) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price |>
  filter(location %in% location_5) |>
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 