## 2a. Prepare data to disaggregate annual hydropower using reservoir releases from ResOpsUS
## Author: Sean Turner sean.turner@pnnl.gov
## Hydrosource (HILLARI) data downloaded from https://doi.org/10.21951/HILARRI/1781642 (2022-03-17)
## 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov

library(starfit) # read ResOpsUS data
library(tidyverse)
library(missRanger)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

# resops has some data in 2021 but for the most part it ends in 2020
output_years <- 2001:2022

eha_fn <- "Data/ORNL_EHAHydro_Plant_FY2023_rev/ORNL_EHAHydroPlant_FY2023_rev.xlsx"
hillari_fn <- "Data/HILARRI_v1_1/HILARRI_v1_1_Public_SubsetHydropowerDams_Plants.csv"

# pull in eha data to map eia id to eha id
eha <- readxl::read_xlsx(eha_fn, sheet = "Operational") |>
  select(EIA_ID = EIA_PtID, eha_ptid = EHA_PtID) |>
  filter(!is.na(EIA_ID))

HILARRI <- read_csv(hillari_fn) %>%
  filter(
    !is.na(grand_id),
    !is.na(eha_ptid)
  ) |>
  mutate(grand_id = as.character(grand_id)) |>
  # CB added this to prevent a many-to-many join,
  # HILARRI has multiple plants per grand_id, dont want to remove them since it has some
  # really big projects like dworshak and hoover
  # will this cause issues elsewhere?
  distinct(grand_id, .keep_all = TRUE)

grand_to_eha <- HILARRI %>%
  select(eha_ptid, grand_id)

grand_in_ResOpsUS <-
  list.files("Data/ResOpsUS/time_series_all/") %>%
  substr(., 10, nchar(.) - 4) %>%
  as.integer()

# pull in resops data
resops_release_data <-
  grand_to_eha %>%
  pull(grand_id) %>%
  unique() %>%
  map(function(grand) {
    if (!grand %in% grand_in_ResOpsUS) {
      return(tibble())
    }

    read_reservoir_data(USRDATS_path = "Data/ResOpsUS", dam_id = grand) %>%
      mutate(
        year = year(date),
        month = month(date) # ,label = T)
      ) %>%
      filter(year %in% output_years) %>%
      mutate(r_cumecs_Q90 = quantile(r_cumecs, 0.9, na.rm = T)) %>%
      # TODO find a better way to do this
      mutate(r_cumecs_capped = if_else(r_cumecs > r_cumecs_Q90, r_cumecs_Q90, r_cumecs)) %>%
      mutate(r_cumecs_capped = ifelse(r_cumecs_capped < 0, 0, r_cumecs_capped)) %>%
      group_by(year, month) %>%
      summarise(av_release_cumecs = mean(r_cumecs_capped, na.rm = T), .groups = "drop") ->
    release_data

    expand.grid(
      month = 1:12, # ordered(month.abb, levels = month.abb),
      year = output_years
    ) %>%
      left_join(release_data, by = c("month", "year")) ->
    release_data_full_period

    # filter for cases with significant gaps (< 5 years of data)
    if (release_data_full_period |> na.omit() |> nrow() < 12 * 5) {
      return(tibble())
    }

    return(release_data_full_period |> mutate(grand_id = !!grand))
  }, .progress = TRUE) |>
  bind_rows()

# impute missing data
resops_release_fraction_filled <- resops_release_data |>
  pivot_wider(id_cols = c(month, year), names_from = grand_id, values_from = av_release_cumecs) |>
  missRanger() |>
  pivot_longer(-c(year, month), names_to = "grand_id", values_to = "fraction") |>
  group_by(grand_id, year) |>
  mutate(fraction = fraction / sum(fraction)) |>
  na.omit() |>
  left_join(
    resops_release_data |>
      filter(is.na(av_release_cumecs)) |>
      select(-av_release_cumecs) |>
      mutate(
        imputed = TRUE,
        grand_id = as.character(grand_id)
      ),
    by = join_by(month, year, grand_id)
  ) |>
  ungroup() |>
  left_join(HILARRI, by = "grand_id") |>
  select(eha_ptid, year, month, fraction, imputed)

# plot to show where data was imputed
p_resops <- resops_release_fraction_filled |>
  mutate(date = ISOdate(year, month, 1)) |>
  ggplot() +
  geom_line(aes(date, fraction)) +
  facet_wrap(~eha_ptid) +
  geom_line(aes(date, fraction),
    color = "red",
    data = resops_release_fraction_filled |>
      mutate(date = ISOdate(year, month, 1)) |>
      filter(imputed == TRUE)
  ) +
  theme_bw() +
  scale_x_datetime(date_labels = "%y") +
  labs(x = "Year", y = "Release Fraction")
p_resops
ggsave("plots/resops.png", p_resops, width = 16, height = 12)

# pull in gauge based release data
gauge_flow_fraction <- "Data/flow/proc/flow_all_td.csv" |>
  read_csv() |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(EIA_ID, year, month) |>
  summarise(value = mean(value, na.rm = T), .groups = "drop") |>
  group_by(EIA_ID, year) |>
  mutate(
    fraction = value / sum(value),
    # the fraction will be NaN in years where all the values are 0,
    # so replace those fractions with zero, its not many points
    fraction = ifelse(is.na(fraction), 0, fraction)
  ) |>
  ungroup() %>%
  left_join(eha, by = "EIA_ID") %>%
  select(eha_ptid, year, month, fraction)

# only use resops is gauge based fraction is not available
release_based_fractions <- gauge_flow_fraction |>
  full_join(resops_release_fraction_filled |> select(-imputed),
    by = join_by(eha_ptid, year, month)
  ) |>
  mutate(fraction = case_when(
    !is.na(fraction.x) ~ fraction.x,
    !is.na(fraction.y) ~ fraction.y,
    .default = NA
  )) |>
  select(-fraction.x, -fraction.y)

release_based_fractions %>%
  write_csv("Output_2a_release_fractions.csv")
