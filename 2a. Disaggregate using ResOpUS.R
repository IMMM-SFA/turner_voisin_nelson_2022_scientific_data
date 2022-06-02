## 2a. Prepare data to disaggregate annual hydropower using reservoir releases from ResOpsUS
## Author: Sean Turner sean.turner@pnnl.gov
## Hydrosource (HILLARI) data downloaded from https://doi.org/10.21951/HILARRI/1781642 (2022-03-17)

library(starfit)    # read ResOpsUS data
library(readr)      # read data from csv files
library(dplyr)      # data wrangling
library(tidyr)      # data wrangling
library(purrr)      # mapping though files processing
library(lubridate)  # time/date tools

# read EIA data
read_csv("Output_1_EIA_MWh.csv") -> eia_netgen

read_csv("Data/HILARRI_v1_1/HILARRI_v1_1_Public_SubsetHydropowerDams_Plants.csv") %>%
  filter(!is.na(grand_id),
         !is.na(eha_ptid)) ->
  HILARRI

HILARRI %>%
  select(eha_ptid, grand_id) ->
  grand_to_eha

grand_in_ResOpsUS <-
  list.files("Data/ResOpsUS/time_series_all/") %>%
  substr(., 10, nchar(.) - 4) %>%
  as.integer()

grand_to_eha %>% pull(grand_id) %>% unique() %>%
  map_dfr(function(grand){

    if(!grand %in% grand_in_ResOpsUS){
      return(tibble())
    }

    read_reservoir_data(USRDATS_path = "Data/ResOpsUS", dam_id = grand) %>%
      mutate(year = year(date),
             month = month(date, label = T)) %>%
      filter(year %in% 2001:2020) %>%
      mutate(r_cumecs_Q90 = quantile(r_cumecs, 0.9, na.rm = T)) %>%
      mutate(r_cumecs_capped = if_else(r_cumecs > r_cumecs_Q90, r_cumecs_Q90, r_cumecs)) %>%
      group_by(year, month) %>%
      summarise(av_release_cumecs = mean(r_cumecs_capped, na.rm = T), .groups = "drop") ->
      release_data

    expand.grid(month = ordered(month.abb, levels = month.abb), year = 2001:2020) %>%
      left_join(release_data, by = c("month", "year")) ->
      release_data_full_period

    release_data_full_period %>%
      group_by(year) %>%
      mutate(fraction = av_release_cumecs / sum(av_release_cumecs)) %>%
      mutate(fraction = round(fraction, 5)) %>%
      mutate(grand_id = !!grand) %>%
      select(grand_id, year, month, fraction) ->
      fractions

    return(fractions)

  }) -> reservoir_release_data

reservoir_release_data %>%
  left_join(HILARRI, by = "grand_id") %>%
  select(eha_ptid, year, month, fraction) %>%
  write_csv("Output_2a_release_fractions.csv")
