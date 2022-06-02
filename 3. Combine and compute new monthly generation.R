## 3. Combine flow fraction tables and perform disaggregation of annual to monthly flow
## Author: Sean Turner sean.turner@pnnl.gov

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(viridis)

# get hours per month for converting MWh to MW average
tibble(date = seq(ISOdate(2001,1,1), to = ISOdate(2020,12,31), by = "day")) %>%
  mutate(date = lubridate::date(date),
         year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  group_by(year, month) %>% summarise(n_days = n()) %>%
  ungroup() %>% mutate(n_hours = n_days * 24) %>%
  left_join(tibble(month = 1:12, month_abb = month.abb), by = "month") %>%
  mutate(month = factor(month_abb, levels = month.abb)) %>%
  select(year, month, n_hours) -> hours_per_month

expand.grid(year = 2001:2020,
            month = factor(month.abb, levels = month.abb, ordered = T)) ->
  year_month_seq

read_csv("Output_2b_huc4_fractions.csv",
         col_types = c("eha_ptid" = "c",
                       "year" = "i",
                       "month" = "c",
                       "fraction" = "d")) ->
  huc4_based_fractions



read_csv("Output_2a_release_fractions.csv",
           col_types = c("eha_ptid" = "c",
                         "year" = "i",
                         "month" = "c",
                         "fraction" = "d")) ->
  release_based_fractions

# fill out missing years in release based fractions using huc_4 based fractions
release_based_fractions %>%
  split(.$eha_ptid) %>% #.[[20]] -> plant
  map_dfr(function(plant){
    plant %>%
      split(.$year) %>% #.[[20]] -> yr
      map_dfr(function(yr){
        if(!any(is.na(yr[["fraction"]]))){
          return(yr %>% mutate(f_type = "release"))
        }

        huc4_based_fractions %>%
          filter(year == unique(yr[["year"]]),
                 eha_ptid == unique(yr[["eha_ptid"]])) %>%
          left_join(yr %>% select(-fraction),
                    by = c("eha_ptid", "year", "month")) %>%
          mutate(f_type = "huc4")

      })
        }) ->
  release_huc4_mix

huc4_based_fractions %>%
  filter(!(eha_ptid %in% release_huc4_mix[["eha_ptid"]])) %>%
  mutate(f_type = "huc4") %>%
  bind_rows(release_huc4_mix) -> all_fractions

# read in annual EIA data
read_csv("Output_1_EIA_MWh.csv") %>%
  filter(netgen_annual != 0) %>%
  gather(month, EIA_MWh, -eia_id, -year, -freq, -EHA_PtID, -plant, -state, -nameplate_MW, -netgen_annual) %>%
  mutate(month = str_to_title(substr(month, 8, 10))) %>%
  arrange(EHA_PtID, year, month) %>%
  left_join(hours_per_month, c("year", "month")) %>%
  group_by(eia_id, year) %>% mutate(EIA_fraction = EIA_MWh / sum(EIA_MWh)) %>%
  ungroup() ->
  EIA_data

# FIX FOR CASES WHERE EIA IS DUPLICATED IN HYDROSOURCE
EIA_data %>% count(eia_id) %>% filter(n>240) %>% pull(eia_id) ->
  remove_eia_HS_duplicates

EIA_data %>%
  filter(!(eia_id %in% remove_eia_HS_duplicates)) %>%
  left_join(all_fractions, by = c("year", "month", "EHA_PtID" = "eha_ptid")) %>%
  mutate(fraction = if_else(is.na(fraction), EIA_fraction, fraction)) %>%
  replace_na(list(fraction = 0, freq = "not noted", EIA_MWh = 0)) %>%
  group_by(eia_id, year) %>%
  mutate(fraction = if_else(fraction == 0, 0.005, fraction),
         fraction = fraction/sum(fraction)) %>% ungroup() %>%
  filter(!is.na(EIA_MWh), !is.na(nameplate_MW)) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(eia_id, year, month) %>%
  mutate(PNNL_MWh = netgen_annual * fraction,
         PNNL_MW = PNNL_MWh / n_hours,
         EIA_MW = EIA_MWh / n_hours) %>%
  split(.$eia_id) %>%
  map_dfr(function(x){
    x %>% split(.$year) %>%
      map_dfr(function(x_yr){

        smoothed <- FALSE

         repeat{
           max(x_yr[["fraction"]]) -> max_fraction
           any(x_yr[["PNNL_MW"]] > (x_yr[["nameplate_MW"]])) -> np_test

           if(max_fraction > 0.25 | np_test == TRUE){

             smoothed <- TRUE

             message(unique(x$eia_id))
             message(unique(x_yr$year))

             bind_rows(x_yr, x_yr, x_yr) %>%
               mutate(mth = 1:36,
                      fraction_smoothed =  predict(loess(fraction ~ mth, span = 0.2, data=.))) %>%
               # ggplot(aes(mth, fraction)) +
               # geom_line() + geom_line(aes(y = fraction_smoothed), col = "red")
               .[13:24,] %>%
               mutate(fraction_smoothed = if_else(fraction_smoothed < 0, 0, fraction_smoothed)) %>%
               mutate(fraction = fraction_smoothed / sum(fraction_smoothed)) %>%
               select(-mth, -fraction_smoothed) ->
               x_yr

           }

           if(!max(x_yr[["fraction"]]) > 0.25){
             break
           }
         }

         return(x_yr %>% mutate(smoothed = !!smoothed))

      })

  }) -> combined_data_with_PNNL_calcs


combined_data_with_PNNL_calcs %>%
  filter(year == 2020,
         freq == "M") %>%
  pull(eia_id) %>% unique() -> recommended_EIA_switch

combined_data_with_PNNL_calcs %>%
  # correct for smoothed factors
  mutate(PNNL_MWh = netgen_annual * fraction,
         PNNL_MW = PNNL_MWh / n_hours) %>%
  select(EIA_ID = eia_id,
         EHA_PtID,
         plant,
         state,
         year, month,
         nameplate_MW,
         EIA_fraction,
         EIA_MWh,
         EIA_obs_freq = freq,
         RectifHyd_fraction = fraction,
         RectifHyd_MWh = PNNL_MWh,
         RectifHyd_method = f_type
         ) %>% arrange(EHA_PtID, year, month) %>%
  tidyr::replace_na(list(RectifHyd_method = "NO FLOW DATA")) %>%
  mutate(RectifHyd_MWh = if_else(RectifHyd_method == "NO FLOW DATA", NA_real_, RectifHyd_MWh),
         RectifHyd_fraction = if_else(RectifHyd_method == "NO FLOW DATA", NA_real_, RectifHyd_fraction)) %>%
  mutate(recommended_data = if_else(EIA_ID %in% recommended_EIA_switch | RectifHyd_method == "NO FLOW DATA", "EIA-923", "RectifHyd")) %>%
  mutate(EIA_ID = as.character(EIA_ID),
         year = as.integer(year)) %>%
  mutate_if(is.double, function(x) round(x, 4)) -> RectifHyd

RectifHyd %>% write_csv("RectifHydv1.0.csv")

