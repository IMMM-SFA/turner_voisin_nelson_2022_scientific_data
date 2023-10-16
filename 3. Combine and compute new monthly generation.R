## 3. Combine flow fraction tables and perform disaggregation of annual to monthly flow
## Author: Sean Turner sean.turner@pnnl.gov
## 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov

library(tidyverse)
library(viridis)

# year of the latest EIA data to use
end_year <- 2022

# get hours per month for converting MWh to MW average
tibble(date = seq(ISOdate(2001, 1, 1), to = ISOdate(end_year, 12, 31), by = "day")) %>%
  mutate(
    date = lubridate::date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(n_days = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(n_hours = n_days * 24) %>%
  left_join(tibble(month = 1:12, month_abb = month.abb), by = "month") %>%
  mutate(month = factor(month_abb, levels = month.abb)) %>%
  select(year, month, n_hours) -> hours_per_month

expand.grid(
  year = 2001:end_year,
  month = factor(month.abb, levels = month.abb, ordered = T)
) ->
year_month_seq

read_csv("Output_2b_huc4_fractions_xSpill.csv",
  col_types = c(
    "eha_ptid" = "c",
    "year" = "i",
    "month" = "c",
    "fraction" = "d"
  )
) ->
huc4_based_fractions



read_csv("Output_2a_release_fractions_xSpill.csv",
  col_types = c(
    "eha_ptid" = "c",
    "year" = "i",
    "month" = "c",
    "fraction" = "d"
  )
) ->
release_based_fractions

# fill out missing years in release based fractions using huc_4 based fractions

release_based_fractions %>%
  split(.$eha_ptid) %>% # .[[20]] -> plant
  map_dfr(function(plant) {
    plant %>%
      split(.$year) %>% # .[[20]] -> yr
      map_dfr(function(yr) {
        if (!any(is.na(yr[["fraction"]]))) {
          return(yr %>% mutate(f_type = "release"))
        }

        huc4_based_fractions %>%
          filter(
            year == unique(yr[["year"]]),
            eha_ptid == unique(yr[["eha_ptid"]])
          ) %>%
          left_join(yr %>% select(-fraction),
            by = c("eha_ptid", "year", "month")
          ) %>%
          mutate(f_type = "huc4")
      })
  }) ->
release_huc4_mix

release_huc4_mix %>% filter(eha_ptid == "hc0115_p01")

huc4_based_fractions %>%
  filter(!(eha_ptid %in% release_huc4_mix[["eha_ptid"]])) %>%
  mutate(f_type = "huc4") %>%
  bind_rows(release_huc4_mix) -> all_fractions

# read in annual EIA data
read_csv("Output_1_EIA_MWh.csv") %>%
  filter(netgen_annual != 0) %>%
  gather(month, EIA_MWh, -eia_id, -year, -freq, -plant, -EHA_PtID, -state, -nameplate_MW, -netgen_annual) %>%
  mutate(month = str_to_title(substr(month, 8, 10))) %>%
  arrange(year, month) %>%
  left_join(hours_per_month, c("year", "month")) %>%
  group_by(eia_id, year) %>%
  mutate(EIA_fraction = EIA_MWh / sum(EIA_MWh)) %>%
  ungroup() ->
EIA_data

# FIX FOR CASES WHERE EIA IS DUPLICATED IN HYDROSOURCE
EIA_data %>%
  count(eia_id, year) %>%
  filter(n > 12) %>%
  pull(eia_id) %>%
  unique() ->
remove_eia_HS_duplicates


### NEED TO GO BACK TO PART 2a/b TO CHECK CASES WHERE THE FRACTION IS NA FOR SEVERAL MONTHS (e.g., EIA_ID == 16)

EIA_data %>%
  filter(!(eia_id %in% remove_eia_HS_duplicates)) %>%
  left_join(all_fractions, by = c("year", "month", "EHA_PtID" = "eha_ptid")) %>%
  mutate(fraction = if_else(is.na(fraction), EIA_fraction, fraction)) %>%
  replace_na(list(fraction = 0, freq = "not noted", EIA_MWh = 0)) %>%
  group_by(EHA_PtID, year) %>% # count() %>% filter(n==24)
  mutate(
    fraction = if_else(fraction == 0, 0.005, fraction),
    fraction = fraction / sum(fraction)
  ) %>%
  ungroup() %>%
  filter(!is.na(EIA_MWh), !is.na(nameplate_MW)) %>% # filter(eia_id == 54678)
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(eia_id, year, month) %>%
  mutate(
    PNNL_MWh = netgen_annual * fraction,
    PNNL_MW = PNNL_MWh / n_hours,
    EIA_MW = EIA_MWh / n_hours
  ) %>% # .[] -> combined_data_with_PNNL_calcs
  filter(!state %in% c("AK", "HI")) %>%
  # mutate(eia_plant = paste0(eia_id, plant)) %>%
  # filter(eia_id == 48, year == 2020) -> x_yr
  split(.$eia_id) %>% # .[[8]] -> x
  map_dfr(function(x) {
    x %>% split(.$year) %>% # .[[13]] -> x_yr
      map_dfr(function(x_yr) {

        # message(unique(x$eia_id))
        # message(unique(x_yr$year))

        smoothed <- FALSE

        counter <- 0

        repeat{
          max(x_yr[["fraction"]]) -> max_fraction
          any(x_yr[["PNNL_MW"]] > (x_yr[["nameplate_MW"]])) -> np_test

          counter <- counter + 1

          if (max_fraction > 0.25 | np_test == TRUE) {
            smoothed <- TRUE

            message(unique(x$eia_id))
            message(unique(x_yr$year))
            # message(counter)

            bind_rows(x_yr, x_yr, x_yr) %>%
              mutate(
                mth = 1:36,
                fraction_smoothed = predict(loess(fraction ~ mth, span = 0.3, data = .))
              ) %>%
              # ggplot(aes(mth, fraction)) +
              # geom_line() + geom_line(aes(y = fraction_smoothed), col = "red") + ylim(0, 0.15)
              .[13:24, ] %>%
              mutate(fraction_smoothed = if_else(fraction_smoothed < 0, 0, fraction_smoothed)) %>%
              mutate(fraction = fraction_smoothed / sum(fraction_smoothed)) %>%
              select(-mth, -fraction_smoothed) %>%
              mutate(PNNL_MWh = fraction * netgen_annual, PNNL_MW = PNNL_MWh / n_hours) ->
            x_yr
          }

          if (counter >= 50) {
            x_yr %>%
              mutate(fraction = 1 / 12) %>%
              mutate(PNNL_MWh = fraction * netgen_annual, PNNL_MW = PNNL_MWh / n_hours) -> x_yr
          }


          if ((!max(x_yr[["fraction"]]) > 0.25 & !any(x_yr[["PNNL_MW"]] > (x_yr[["nameplate_MW"]]))) | counter >= 50) {
            break
          }
        }

        return(x_yr %>% mutate(smoothed = !!smoothed))
      })
  }) -> combined_data_with_PNNL_calcs



combined_data_with_PNNL_calcs %>%
  filter(eia_id == 57690, year == 2015) %>%
  pull(plant)

combined_data_with_PNNL_calcs %>%
  filter(
    year %in% 2020:2022,
    freq == "M"
  ) %>%
  pull(eia_id) %>%
  unique() -> recommended_EIA_switch

combined_data_with_PNNL_calcs %>%
  # correct for smoothed factors
  mutate(
    PNNL_MWh = netgen_annual * fraction,
    PNNL_MW = PNNL_MWh / n_hours
  ) %>%
  # filter(PNNL_MW > nameplate_MW) %>%
  select(
    EIA_ID = eia_id,
    EHA_PtID,
    plant,
    state,
    year, month,
    nameplate_MW,
    EIA_fraction,
    EIA_MWh,
    EIA_obs_freq = freq,
    hydro923plus_fraction = fraction,
    hydro923plus_MWh = PNNL_MWh,
    hydro923plus_method = f_type
  ) %>%
  arrange(EHA_PtID, year, month) %>%
  tidyr::replace_na(list(hydro923plus_method = "NO FLOW DATA")) %>%
  mutate(
    hydro923plus_MWh = if_else(hydro923plus_method == "NO FLOW DATA", NA_real_, hydro923plus_MWh),
    hydro923plus_fraction = if_else(hydro923plus_method == "NO FLOW DATA", NA_real_, hydro923plus_fraction)
  ) %>%
  mutate(
    recommended_data = if_else(
      EIA_ID %in% recommended_EIA_switch | hydro923plus_method == "NO FLOW DATA",
      "EIA-923", "hydro923plus"
    )
  ) %>%
  mutate(
    EIA_ID = as.character(EIA_ID),
    year = as.integer(year)
  ) %>%
  mutate_if(is.double, function(x) round(x, 4)) -> hydro923plus

hydro923plus %>%
  count(year) %>%
  tail()

hydro923plus %>%
  write_csv("RectifHydv1.0_Q90_Spill_smoothed_SMOOTH_update_2022.csv")



# filter(! state %in% c("AK", "HI")) %>%
# select(plant, hydro923plus_method) %>% unique() %>% count(hydro923plus_method)

# filter(is.na(hydro923plus_method)) %>% group_by(plant) %>% mutate(n = n()) %>% filter(n==240) %>%
# select(plant, nameplate_MW) %>% unique() %>% arrange(-nameplate_MW)
# tidyr::replace_na(list(hydro923plus_method = "Adopt EIA-923 observed"))
# filter(EIA_fraction != hydro923plus_fraction) %>%
# mutate()

# mutate()



write_csv(combined_data_with_PNNL_calcs, "combined_data_with_PNNL_calcs.csv")



# compare fractions by region
read_csv("RectifHydv1.0_Q90_Spill_smoothed_SMOOTH_update_2022.csv") %>%
  filter(
    year == 2020,
    EIA_obs_freq != "M"
  ) %>%
  mutate(state = case_when(
    EIA_ID == 297 ~ "CA",
    TRUE ~ state
  )) %>%
  filter(
    !EIA_ID %in% c(57690, 61217),
    !state %in% c("AK", "HI")
  ) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  select(EIA_ID, state, month, `EIA-923` = EIA_fraction, hydro923plus = hydro923plus_fraction) %>%
  # filter(state == "CA", month == "Jan") %>% arrange(`EIA-923`)
  # gather(data, fraction, -eia_id, -state, -month) %>%
  arrange(EIA_ID, month) %>%
  left_join(tibble(state = state.abb, division = state.division)) %>%
  mutate(division = gsub(" ", "\n", division)) %>%
  # mutate(eia_region = case_when(
  #   state %in% c("OR", "WA", "CA", "MA", "VT", "GA", "WV") ~ state,
  #   TRUE ~ division
  #   # division == "Mountain" ~ "Moutain (MT, ID, WY, NV, UT, CO, AZ, NM)",
  #   # division == "West South Central" ~ "West South Central (OK, TX, AR, LA)",
  #   # division == "West North Central" ~ "West North Central (ND, SD, NE, KS, MN, IA, MO)",
  #   # division == "Middle Atlantic" ~ ,
  # )) %>%
  # filter(eia_region == "Mountain", month == "Jan") %>% arrange(`EIA-923`)
  # filter(`EIA-923` == 0.0759, month == "Jan")
  # filter(division == "South Atlantic") %>%
  ggplot(aes(month, hydro923plus, group = EIA_ID)) +
  geom_line(col = "grey", alpha = 0.5) +
  geom_line(aes(y = `EIA-923`), col = "black", lwd = 0.8, linetype = 2) +
  facet_wrap(~division, nrow = 3) +
  # ylim(0, 0.3) +
  theme_classic() +
  scale_x_discrete(breaks = c(
    "Jan",
    "Apr", "Jul", "Oct"
  )) +
  theme(
    strip.background = element_blank(),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    strip.placement = "inside",
    strip.text = element_text(size = 12, vjust = 0)
  ) +
  labs(
    y = NULL, title = "RectifHyd generation allocation compared to EIA-923",
    x = NULL
  )

read_csv("RectifHydv1.0_Q90_Spill_smoothed_SMOOTH_update_2022.csv") %>%
  left_join(tibble(state = state.abb, division = state.division)) %>%
  filter(EIA_obs_freq == "M", division == "Middle Atlantic", year == 2018) %>%
  # filter(EIA_obs_freq == "M", state == "CA", year == 2020) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  mutate(plant = if_else(plant ==
    "Chester Diversion Hydroelectric Project", "Chester Diversion", plant)) %>%
  select(plant, state, month, EIA_MWh, hydro923plus_MWh) %>%
  mutate(plant_state = paste0(plant, " (", state, ")")) %>%
  arrange(state) %>%
  mutate(plant_state = if_else(grepl("Hoover", plant_state), substr(plant_state, 1, 15), plant_state)) %>%
  group_by(month) %>%
  mutate(total_monthly = sum(EIA_MWh)) %>%
  ungroup() %>%
  mutate(implied_fraction = total_monthly / sum(EIA_MWh)) %>%
  mutate(EIA_MWh = if_else(EIA_MWh < 0, 0, EIA_MWh)) %>%
  select(-total_monthly) %>%
  group_by(plant_state) %>%
  mutate(EIA_method_MWh = implied_fraction * sum(EIA_MWh)) %>%
  select(-implied_fraction, -hydro923plus_MWh) %>%
  rename(`EIA-923 Observation` = EIA_MWh, `If imputed using EIA-923 method` = EIA_method_MWh) %>%
  gather(method, MWh, -plant_state, -plant, -state, -month) %>%
  ungroup() %>%
  mutate(MWh = MWh / 1000) %>%
  ggplot(aes(month, MWh, col = method, group = method)) +
  geom_line() +
  geom_line() +
  facet_wrap(~plant_state, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("dodgerblue", "black")) +
  # scale_linetype_manual(values = c(1, 2)) +
  expand_limits(y = 0) +
  theme_classic() +
  scale_x_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    strip.background = element_blank()
  ) +
  labs(
    y = NULL, title = "Monthly Generation (Thousand MWh)",
    col = NULL, x = NULL
  )






combined_data_with_PNNL_calcs %>%
  mutate(gen_MWh = if_else(freq == "M", EIA_MWh, PNNL_MWh)) %>%
  mutate(gen_MWh = round(gen_MWh, 3)) %>% # filter(gen_MWh > NP_MWh * 1.1)
  mutate(month = factor(month, levels = month.abb, ordered = T)) %>%
  arrange(eia_id, year, month) %>%
  select(EIA_ID = eia_id, year, month, gen_MWh) %>%
  mutate(EIA_ID = as.integer(EIA_ID), year = as.integer(year)) %>%
  # CHECK THIS AGAIN!!
  group_by(EIA_ID, year, month) %>%
  summarise(gen_MWh = sum(gen_MWh, na.rm = T)) %>%
  ungroup() %>%
  # filter(EIA_ID == 3437)
  split(.$EIA_ID) %>% # .[[32]] -> x
  map_dfr(function(x) {
    x %>% right_join(year_month_seq %>% mutate(EIA_ID = x[["EIA_ID"]] %>% unique()),
      by = c("year", "month", "EIA_ID")
    )
  }) -> combined_data_filled

combined_data_filled %>%
  spread(month, gen_MWh) %>%
  readr::write_csv("NetGen_provisional_MWh.csv")


combined_data_with_PNNL_calcs %>%
  # filter(PNNL_MWh > NP_MWh)
  group_by(eia_id, year) %>%
  filter(freq != "M") %>%
  mutate(PNNL_MWh = sum(PNNL_MWh)) %>%
  ungroup() %>%
  mutate(fraction_fixed = PNNL_MWh / mean(netgen_annual)) %>%
  mutate(fraction_fixed_ = PNNL_MWh / sum(PNNL_MWh)) %>%
  # filter(abs(fraction_fixed - fraction_fixed_) > 0.001)
  # mutate(gen_MWh = if_else(freq == "M", EIA_MWh, PNNL_MWh)) %>%
  # mutate(gen_MWh = round(gen_MWh, 3))

  mutate(PNNL_MWh = fraction * netgen_annual) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(eia_id, year, month) %>%
  filter(!(is.na(EIA_MWh) & is.na(PNNL_MWh))) %>%
  filter(!is.na(PNNL_MWh)) %>%
  group_by(state, year, month) %>%
  mutate(cap_MWh = nameplate_MW * n_hours) %>%
  # filter(cap_MWh <PNNL_MWh)
  summarise(PNNL = sum(PNNL_MWh), EIA = sum(EIA_MWh), cap_MWh = sum(cap_MWh)) %>%
  gather(data, MWh, -state, -year, -month, -cap_MWh) %>%
  mutate(cap_factor = MWh / cap_MWh) %>%
  filter(state %in% c("VT", "ID", "WY", "CA")) %>%
  ggplot(aes(year, month, fill = cap_factor)) +
  geom_tile() +
  facet_grid(state ~ data) +
  scale_fill_viridis()
