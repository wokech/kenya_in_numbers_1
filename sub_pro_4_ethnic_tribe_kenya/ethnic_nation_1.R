# An analysis of small ethnic poulations in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

eth_nat <- V4_T2.31

eth_nat <- eth_nat %>%
  clean_names()

eth_nat_totals <- eth_nat %>%
  slice(1:4)

eth_nat_kenyans <- eth_nat %>%
  slice(5:133)

eth_nat_non_kenyans <- eth_nat %>%
  slice(134:148)

eth_nat_large_tribe <- eth_nat %>%
  filter(ethnicity_nationality == "KIKUYU" |
         ethnicity_nationality == "KALENJIN" |
         ethnicity_nationality == "LUHYA" |
         ethnicity_nationality == "LUO" |
         ethnicity_nationality == "KAMBA" |
         ethnicity_nationality == "KENYAN SOMALI" | 
         ethnicity_nationality == "KISII" |
         ethnicity_nationality == "MIJIKENDA" | 
         ethnicity_nationality == "MERU" |
         ethnicity_nationality == "MAASAI" | 
         ethnicity_nationality == "TURKANA")

eth_nat_large_tribe_totals <- eth_nat_large_tribe %>%
  filter(subgroup == "")

eth_nat_large_tribe_subtribe <- eth_nat_large_tribe %>%
  filter(subgroup != "")
  
eth_nat_small_tribe <- eth_nat %>%
  filter(ethnicity_nationality != "KIKUYU" &
           ethnicity_nationality != "KALENJIN" &
           ethnicity_nationality != "LUHYA" &
           ethnicity_nationality != "LUO" &
           ethnicity_nationality != "KAMBA" &
           ethnicity_nationality != "KENYAN SOMALI" & 
           ethnicity_nationality != "KISII" &
           ethnicity_nationality != "MIJIKENDA" & 
           ethnicity_nationality != "MERU" &
           ethnicity_nationality != "MAASAI" & 
           ethnicity_nationality != "TURKANA") %>%
  slice(6:65)

# remember that the totals are included
eth_nat_small_tribe_subtribe <- eth_nat_small_tribe %>%
  filter(ethnicity_nationality == "DASENACH" |
           ethnicity_nationality == "ORMA" |
           ethnicity_nationality == "SWAHILI")
  

eth_nat_small_tribe_no_subtribe <- eth_nat_small_tribe %>%
  filter(ethnicity_nationality != "DASENACH" &
           ethnicity_nationality != "ORMA" &
           ethnicity_nationality != "SWAHILI")

eth_nat_east_af <- eth_nat_non_kenyans %>%
  filter(subgroup != "")

eth_nat_other_national <- eth_nat_non_kenyans %>%
  slice(8:15)

# C) EDA

# 1) Total Population - STACKED BAR CHART

# 2) Large tribe totals - TREEMAP WITH NUMBERS INSIDE

# 3) Large tribe and subtribes (bar plot, maybe wordcloud)

# i)  Subtribes in a large tribe that are less than 100k (~22 tribes)

# ii)  Subtribes in a large tribe that are greater than 100k and less than 1M (~21 tribes)

# iii)  Subtribes in a large tribe that are greater than 1M (~5 tribes)

# 4) Small tribes and subtribes (bar plot,  maybe wordcloud)

# i) Subtribes in small tribes

# ii) Small tribes with no subtribes

# 5) Non-kenyans

# i) East Africans

# ii) Other nationals



