## Create final RectifHyd file
## Author: Sean Turner sean.turner@pnnl.gov
## 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
library(tidyverse)

read_csv("RectifHydv1.0_Q90_Spill_smoothed_SMOOTH_update_2022.csv") %>% #
  # filter(EIA_ID == 54678, year == 2018) %>% mutate(x = hydro923plus_MWh / (24*35))
  group_by(EIA_ID, year) %>%
  summarise(
    EIA = sum(EIA_MWh),
    RectifHyd = sum(hydro923plus_MWh)
  ) %>%
  ungroup() %>%
  mutate(x = abs((RectifHyd - EIA) / EIA)) %>%
  arrange(-x) %>%
  print(n = 10)


read_csv("RectifHydv1.0_Q90_Spill_smoothed_SMOOTH_update_2022.csv") %>%
  rename(
    RectifHyd_fraction = hydro923plus_fraction,
    RectifHyd_method = hydro923plus_method,
    RectifHyd_MWh = hydro923plus_MWh
  ) %>%
  select(
    EIA_ID, plant, state, year, month, EIA_obs_freq,
    RectifHyd_method, EIA_fraction, EIA_MWh,
    RectifHyd_fraction, RectifHyd_MWh,
    recommended_data
  ) %>%
  mutate(recommended_data = if_else(recommended_data == "hydro923plus",
    "RectifHyd", recommended_data
  )) %>%
  mutate(
    EIA_ID = as.integer(EIA_ID),
    year = as.integer(year)
  ) %>%
  mutate_if(is.double, function(x) round(x, 4)) %>%
  write_csv("RectifHyd_v1.2.0.csv")

#
# rectifhyd1 %>%
#   filter(EIA_ID == 57690, year == 2016)
#
