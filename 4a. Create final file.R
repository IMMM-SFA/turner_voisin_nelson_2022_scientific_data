## Create final RectifHyd file
## Author: Sean Turner sean.turner@pnnl.gov
## 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
## 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
library(tidyverse)

final_file <- "RectifHyd_v1.3.csv"

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
    recommended_data, smoothed, scaled, imputed
  ) %>%
  mutate(recommended_data = if_else(recommended_data == "hydro923plus",
    "RectifHyd", recommended_data
  )) %>%
  mutate(
    EIA_ID = as.integer(EIA_ID),
    year = as.integer(year)
  ) %>%
  mutate_if(is.double, function(x) round(x, 4)) ->
rectifhyd

rectifhyd |> write_csv(final_file)

rectifhyd_long <- bind_rows(
  read_csv("RectifHyd_v1.3.csv") |> mutate(version = "1.3.0"),
  read_csv("~/projects/energy-exemplar/reproduceB1/RectifHyd_v1.2.1.csv") |> mutate(version = "1.2.1")
)
"rectifhyd_diag.pdf" |> pdf(width = 10, height = 10)
rectifhyd_long |>
  group_by(EIA_ID) |>
  group_split() |>
  map(function(plant) {
    p <- plant |>
      mutate(date = as.POSIXct(fast_strptime(sprintf("%s-%s", 2000, month), "%Y-%b"))) |>
      ggplot() +
      geom_line(aes(date, RectifHyd_MWh, linetype = version, color = version)) +
      scale_color_manual(values = c("darkblue", "darkorange")) +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      facet_wrap(~year) +
      scale_x_datetime(date_labels = "%b") +
      labs(title = sprintf("%s - %s", plant$EIA_ID[1], plant$plant[1]))
    print(p)
  }, .progress = T) -> shh
dev.off()
