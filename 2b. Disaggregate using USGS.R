## 2b. Prepare data to disaggregate annual hydropower using downstream flows
## Author: Sean Turner sean.turner@pnnl.gov
## Hydrosource (HILLARI) data downloaded from https://doi.org/10.21951/HILARRI/1781642 (2022-03-17)
## 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov

library(dataRetrieval)
library(tidyverse)
library(readxl)

# year of the latest EIA data to use
end_year <- 2022

# read in HILARRI database
read_csv("Data/HILARRI_v1_1/HILARRI_v1_1_Public_SubsetHydropowerDams_Plants.csv") %>%
  filter(!is.na(eha_ptid)) -> HILARRI

read_xlsx("Data/ORNL_EHAHydroPlant_FY2023/ORNL_EHAHydroPlant_PublicFY2023.xlsx", sheet = "Operational") %>%
  filter(!(EHA_PtID %in% HILARRI$eha_ptid), !is.na(HUC)) %>%
  # HUCs in the 2022 and 2023 data are formatted as numbers not text
  # so we have to pull some funny business to prevent scientific notation
  # getting converted to text and to preserve the leading zeros
  mutate(HUC = format(HUC, scientific = FALSE)) %>%
  mutate(HUC = gsub(" ", "0", HUC)) %>%
  # mutate(HUC = if_else(nchar(HUC) == 11, paste0("0", HUC), as.character(HUC))) %>%
  # filter(nchar(HUC) == 12) %>%
  mutate(HUC4 = substr(HUC, 1, 4)) %>%
  select(eha_ptid = EHA_PtID, HUC4) %>%
  unique() ->
additional_HUC

# read_csv("Output_2a_release_fractions_xSpill.csv") %>%
#   split(.$eha_ptid) %>% #.[[2]] -> plant
#   map_dfr(function(plant){
#     plant %>% mutate(is_na = is.na(fraction)) %>%
#       group_by(year) %>% summarise(na_per_year = sum(is_na)) ->
#       na_per_year
#
#     if(any(na_per_year[["na_per_year"]])) return(tibble())
#
#     return(plant)
#
#   }) %>%
#   pull(eha_ptid) %>% unique() ->
#   release_based_cases

# get HUC4 and remove cases reservoir release have it covered!
HILARRI %>%
  mutate(HUC4 = substr(huc_12, 1, 4)) %>%
  select(eha_ptid, HUC4) %>%
  bind_rows(additional_HUC) ->
# filter(!eha_ptid %in% release_based_cases) ->
plants_to_disag_with_flow

# desired data sequence
expand.grid(
  year = 2001:end_year,
  month = factor(month.abb, levels = month.abb, ordered = T)
) ->
year_month_seq

# read required HUC4s and associated gages
# read_csv("Data/USGS_Streamgage_HUC4.csv") %>%
read_csv("Data/USGS_00060_HUC4.csv") %>%
  # mutate(USGS_ID = if_else(nchar(USGS_ID) == 7, paste0("0", USGS_ID), as.character(USGS_ID))) %>%
  # mutate(HUC4 = if_else(nchar(HUC4) == 3, paste0("0", HUC4), as.character(HUC4))) %>%
  # filter(!is.na(USGS_ID)) %>%
  select(HUC4, USGS_ID) %>%
  # mutate(USGS_ID = if_else(USGS_ID == "03085000", "03072655", USGS_ID),
  #        USGS_ID = if_else(USGS_ID == "08162500", "08159200", USGS_ID),
  #        USGS_ID = if_else(USGS_ID == "02420000", "02428400", USGS_ID),
  #        USGS_ID = if_else(HUC4 == "0415", "04266500", USGS_ID),
  #        USGS_ID = if_else(HUC4 == "0106", "01059000", USGS_ID),
  #        USGS_ID = if_else(HUC4 == "1602", "10141000", USGS_ID),
  #        USGS_ID = if_else(HUC4 == "1707", "14103000", USGS_ID)) %>%
  pmap_dfr(function(HUC4, USGS_ID) {
    message(HUC4)
    message(USGS_ID)

    dataRetrieval::readNWISdata(
      sites = USGS_ID, service = "dv",
      parameterCd = "00060",
      asDateTime = FALSE,
      startDate = "2001-01-01",
      endDate = "2022-12-31"
    ) -> data_dl
    data_dl %>%
      as_tibble() %>%
      select(
        av_flow_cfs = X_00060_00003,
        date = dateTime
      ) %>%
      mutate(day = day(date), month = month(date, label = T), year = year(date)) %>%
      mutate(flow_cfs_Q90 = quantile(av_flow_cfs, 0.9, na.rm = T)) %>%
      mutate(flow_cfs_capped = if_else(av_flow_cfs > flow_cfs_Q90, flow_cfs_Q90, av_flow_cfs)) %>%
      group_by(day, month, year) %>%
      summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T), .groups = "drop") %>%
      group_by(month, year) %>%
      summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T), .groups = "drop") %>%
      right_join(year_month_seq, by = c("month", "year")) %>%
      mutate(HUC4 = as.character(!!HUC4), USGS_ID = !!USGS_ID)
  }) -> HUC4_average_flows_all



plants_to_disag_with_flow %>%
  left_join(
    HUC4_average_flows_all %>%
      group_by(year, HUC4) %>% arrange(HUC4, year, month) %>%
      mutate(fraction = av_flow_cfs / sum(av_flow_cfs)) %>%
      ungroup() %>%
      select(year, month, HUC4, fraction),
    relationship = "many-to-many"
  ) %>%
  select(-HUC4) %>%
  group_by(eha_ptid, year) %>% # filter(eha_ptid == "hc0141_p01") %>%
  mutate(
    fraction = if_else(fraction == 0, 0.005, fraction),
    fraction = fraction / sum(fraction)
  ) %>%
  ungroup() -> initial_fractions

initial_fractions %>%
  split(.$eha_ptid) %>% # .[[1]] -> site
  map_dfr(function(site) {
    if (!any(is.na(site[["fraction"]]))) {
      return(site)
    }

    site %>%
      group_by(month) %>%
      summarise(fraction = mean(fraction, na.rm = T)) %>%
      mutate(fraction = fraction / sum(fraction)) -> replacement_fractions

    site %>% split(.$year) %>% # .[[2]] -> yr
      map_dfr(function(yr) {
        if (!any(is.na(yr$fraction))) {
          return(yr)
        }

        yr$fraction <- replacement_fractions$fraction
        return(yr)
      })
  }) -> fractions_final

fractions_final %>% # filter(eha_ptid == "hc0384_p01")
  write_csv("Output_2b_huc4_fractions_xSpill.csv")



# problem_sites <- HUC4_average_flows_all %>% filter(is.na(av_flow_cfs)) %>%
#   pull(HUC4) %>% unique()
#
# read_csv("Data/USGS_Streamgage_HUC4.csv") %>%
#   mutate(HUC4 = if_else(nchar(HUC4) == 3, paste0("0", HUC4), as.character(HUC4))) %>%
#   #filter(!is.na(StreamGage_FEA1)) %>%
#   rename(USGS_ID = StreamGage_FEA1) %>%
#   mutate(USGS_ID = if_else(USGS_ID == "03085000", "03072655", USGS_ID),
#          USGS_ID = if_else(USGS_ID == "08162500", "08159200", USGS_ID),
#          USGS_ID = if_else(USGS_ID == "02420000", "02428400", USGS_ID),
#          USGS_ID = if_else(HUC4 == "0415", "04266500", USGS_ID),
#          USGS_ID = if_else(HUC4 == "0106", "01059000", USGS_ID),
#          USGS_ID = if_else(HUC4 == "1602", "10141000", USGS_ID),
#          USGS_ID = if_else(HUC4 == "1707", "14103000", USGS_ID)) %>%
#   mutate(gap_in_record = if_else(HUC4 %in% problem_sites, "Yes", "No")) %>%
#   write_csv("Data/USGS_Steamgage_HUC4_STgaps.csv")
#
# read_csv("Data/USGS_Steamgage_HUC4_STgaps.csv") %>% View()
