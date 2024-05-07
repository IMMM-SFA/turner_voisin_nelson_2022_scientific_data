## 3. Combine flow fraction tables and perform disaggregation of annual to monthly flow
## Author: Sean Turner sean.turner@pnnl.gov
## 2023 1.2.1 Update - Cameron Bracken cameron.bracken@pnnl.gov
## 2024 1.3.0 Update - Cameron Bracken cameron.bracken@pnnl.gov

library(tidyverse)
library(viridis)
library(missRanger)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

# year of the latest EIA data to use
end_year <- 2022

# hours per month for converting MWh to MW average
hours_per_month <- tibble(date = seq(ISOdate(2001, 1, 1), to = ISOdate(end_year, 12, 31), by = "day")) %>%
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
  select(year, month, n_hours)

year_month_seq <- expand.grid(
  year = 2001:end_year,
  month = factor(month.abb, levels = month.abb, ordered = T)
)


# disag fractions based on actual turbine release
# these are preferred where available
read_csv("Output_2a_release_fractions.csv",
  col_types = c(
    "eha_ptid" = "c",
    "year" = "i",
    "month" = "c",
    "fraction" = "d"
  )
) ->
release_based_fractions

# disag fractions based on huc4 flows,
# these are a fallback if release based fractions are not available
read_csv("Output_2b_huc4_fractions.csv",
  col_types = c(
    "eha_ptid" = "c",
    "year" = "i",
    "month" = "c",
    "fraction" = "d"
  )
) ->
huc4_based_fractions

# eia_id, nameplate_MW
# 253, 3.1, http://www.kilarc.info/Pictures/Kilarc%20Powerhouse/Photos_For_Kilarc_Powerhouse.htm
# 262, 25.5, https://en.wikipedia.org/wiki/Narrows_Dam
# 293, 128, https://en.wikipedia.org/wiki/Wishon_Dam
# 437, 644, https://hydroreform.org/hydro-project/edward-c-hyatt-p-2100/



# fill out missing years in release based fractions using huc_4 based fractions
# CB May 2023 We used imputation to fill in this data so this code is obsolete
# leaving it in as a chack, no ids should be printed out
release_based_fractions %>%
  split(.$eha_ptid) %>% # .[[270]] -> plant
  map_dfr(function(plant) {
    plant %>%
      split(.$year) %>% # .[[20]] -> yr
      map_dfr(function(yr) {
        if (!any(is.na(yr[["fraction"]]))) {
          return(yr %>% mutate(f_type = "release"))
        }
        message(yr$eha_ptid[1])

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

huc4_based_fractions %>%
  filter(!(eha_ptid %in% release_huc4_mix[["eha_ptid"]])) %>%
  mutate(f_type = "huc4") %>%
  bind_rows(release_huc4_mix) ->
all_fractions

# raw eia data with NAs
eia_raw <- read_csv("Output_1_EIA_MWh.csv")

eia_missing <- eia_raw |>
  pivot_longer(-c(eia_id, EHA_PtID, year, plant, state, nameplate_MW, freq, netgen_annual)) |>
  filter(is.na(value))

month_num <- 1:12 |> `names<-`(month.abb)

# impute all missing monthly values
eia_imputed_values <- eia_raw |>
  select(eia_id, year, nameplate_MW, all_of(month.abb)) |>
  pivot_longer(-c(eia_id, year, nameplate_MW), names_to = "month") |>
  mutate(month = month_num[month]) |>
  pivot_wider(id_cols = c(year, month), names_from = eia_id) |>
  missRanger() |>
  pivot_longer(-c(year, month), names_to = "eia_id") |>
  mutate(month = month.abb[month]) |>
  pivot_wider(id_cols = c(year, eia_id), names_from = month) |>
  mutate(netgen_annual = Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec)

eia_imputed <- eia_raw |>
  select(-all_of(month.abb), -netgen_annual) |>
  inner_join(eia_imputed_values |> mutate(eia_id = as.numeric(eia_id)),
    by = join_by(eia_id, year)
  )


EIA_data <- eia_imputed %>%
  # filter(netgen_annual != 0) %>%
  pivot_longer(
    cols = -c(eia_id, year, freq, plant, EHA_PtID, state, nameplate_MW, netgen_annual),
    names_to = "month", values_to = "EIA_MWh"
  ) |>
  # mutate(month = str_to_title(substr(month, 8, 10))) %>%
  # filter(EIA_MWh > 0) %>%
  arrange(year, month) %>%
  left_join(hours_per_month, c("year", "month")) %>%
  group_by(eia_id, year) %>%
  mutate(EIA_fraction = EIA_MWh / sum(EIA_MWh)) %>%
  ungroup()


### NEED TO GO BACK TO PART 2a/b TO CHECK CASES WHERE THE FRACTION IS NA FOR SEVERAL MONTHS (e.g., EIA_ID == 16)

EIA_data %>%
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
  group_by(eia_id, year) |>
  # some plants have generation and nameplate vales that dont line up
  # so assume the nameplate is bad
  # mutate(nameplate_MW = ifelse(nameplate_MW max(EIA_MW))) %>%
  filter(!state %in% c("AK", "HI")) %>%
  split(.$eia_id) %>% # .[[8]] -> x
  map_dfr(function(x) {
    x %>% split(.$year) %>% # .[[13]] -> x_yr
      map_dfr(function(x_yr) {
        #
        smoothed <- FALSE
        scaled <- FALSE
        counter <- 0

        message(unique(x$eia_id), " ", unique(x_yr$year))
        # we apply a loess smoothing spline to the provisional allocation factors
        # (low degree of smoothing; span = 0.2), repeating until generation is more
        # reasonable with nameplate capacity not exceeded and no monthly factor
        # greater than 0.25, which would imply a quarter of annual generation
        # occurring in a single month. This threshold was selected based on an
        # analysis of existing monthly generating data across observed plants
        # (0.25 is very rarely exceeded). Final adjusted factors are then multiplied
        # by observed annual generation to create a rectified set of monthly generation
        # estimates that sum to observed annual generation at each plant.
        repeat{
          max_fraction <- max(x_yr[["fraction"]])
          np_test <- any(x_yr[["PNNL_MW"]] > (x_yr[["nameplate_MW"]]))

          counter <- counter + 1

          if (max_fraction > 0.25 | np_test == TRUE) {
            # browser()
            smoothed <- TRUE

            # if (counter == 1) message("Smoothing fractions")

            # message(counter)
            if (nrow(x_yr) < 12) browser()

            # stick together the same year on the back and the front for smoothing
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
            # bail out if the smoothing is taking too long
            # browser()
            message("Scaling monthly distribution")
            x_yr %>%
              mutate(fraction = 1 / 12) %>%
              # mutate(PNNL_MWh = fraction * netgen_annual, PNNL_MW = PNNL_MWh / n_hours) -> x_yr
              # instead of assigning a flat profile, shift the monthly distribution down so the max
              # is the nameplate capacity, preserving the monthly shape,
              # this happens only rarely
              mutate(
                PNNL_MW = fraction * netgen_annual / n_hours,
                PNNL_MWh = PNNL_MW / max(PNNL_MW) * nameplate_MW * n_hours,
                PNNL_MW = PNNL_MWh / n_hours
              ) -> x_yr
            scaled <- TRUE
          }


          if ((!max(x_yr[["fraction"]]) > 0.25 &
            !any(x_yr[["PNNL_MW"]] > (x_yr[["nameplate_MW"]]))) |
            counter >= 50) {
            # message("Counter: ", counter)
            break
          }
        }

        return(x_yr %>% mutate(smoothed = !!smoothed, scaled = !!scaled))
      })
  }) -> combined_data_with_PNNL_calcs


combined_data_with_PNNL_calcs %>%
  filter(
    year %in% 2020:2022,
    freq == "M"
  ) %>%
  pull(eia_id) %>%
  unique() -> recommended_EIA_switch

combined_data_with_PNNL_calcs %>%
  ungroup() %>%
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
  print(n = 30)

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
