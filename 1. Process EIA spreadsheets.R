## 1. Process raw EIA spreadsheets EIA-923, EIA-906/920
## Author: Sean Turner sean.turner@pnnl.gov
## EIA downloaded from: https://www.eia.gov/electricity/data/eia923/ (2022-03-17)
## Hydrosource data downloaded from https://hydrosource.ornl.gov/dataset/EHA2021 (2022-03-17)

library(readxl)     # read data from excel spreadsheets
library(dplyr)      # data wrangling
library(tidyr)      # data wrangling
library(purrr)      # mapping though files processing
library(lubridate)  # time/date tools

# Use HydroSource EHA (2021) to identify desired EIA_IDs
read_xlsx("Data/ORNL_EHAHydro_Plant_FY2021/ORNL_EHAHydroPlant_FY2021_revised.xlsx",
          sheet = "Operational") %>%
  select(EHA_PtID, plant = PtName, eia_id = EIA_PtID, State, CH_MW) %>%
  mutate(eia_id = as.integer(eia_id)) %>%
  # filter for plants with known EIA ID
  filter(!is.na(eia_id)) ->
  hydrosource_EIA

EIA_IDs_desired <- hydrosource_EIA[["eia_id"]] %>% unique()

2001:2020 %>%
  map_dfr(function(yr){

    message(yr)

    if(yr %in% 2001:2002){

      file_name <- paste0("Data/EIA-923 2001 - 2020/f906920_", yr, "/f906920_", yr, ".xls")

      # read EIA file
      read_xls(file_name, skip = 7) %>%
        rename(eia_id = `Plant ID`) %>%
        mutate(eia_id = as.integer(eia_id)) -> EIA_data

      # filter EIA file for hydro
      EIA_data %>%
        filter(eia_id %in% EIA_IDs_desired,
               `AER Fuel Type Code` == "HYC") %>%
        select(eia_id, contains("Netgen"),
               `NET GENERATION (megawatthours)`) -> EIA_hydro

      # fix col names
      names(EIA_hydro) <- str_to_lower(names(EIA_hydro)) %>% str_replace("\r\n", "_") %>%
        substr(1,10)

      # make netgen numeric and add frequency of reporting "true_monthly?"
      return(
        tibble(eia_id = EIA_IDs_desired) %>%
          left_join(EIA_hydro, by = "eia_id") %>%
          mutate_if(is.character, as.numeric) %>%
          mutate(freq = NA_character_,
                 year = !!yr)
      )
    }


    if(yr %in% 2003:2007){

      file_name <- paste0("Data/EIA-923 2001 - 2020/f906920_", yr, "/f906920_", yr, ".xls")

      # read EIA file
      read_xls(file_name, skip = 7) %>%
        rename(eia_id = `Plant ID`) ->
        EIA_data

      # filter EIA file for hydro
      EIA_data %>%
        select(eia_id, contains("Netgen"), type = contains("mover"),
               `NET GENERATION (megawatthours)`) %>%
        filter(eia_id %in% EIA_IDs_desired, type == "HY") %>%
        select(-type) ->
        EIA_hydro

      # fix col names
      names(EIA_hydro) <- str_to_lower(names(EIA_hydro)) %>% str_replace("\r\n", "_") %>%
        substr(1,10)

      # make netgen numeric and add frequency of reporting "true_monthly?"
      return(
        tibble(eia_id = EIA_IDs_desired) %>%
          left_join(EIA_hydro, by = "eia_id") %>%
          mutate_if(is.character, as.numeric) %>%
          mutate(freq = NA_character_,
                 year = !!yr)
      )
    }

    if(yr %in% 2008:2020){

      if(yr %in% 2008:2010){
        file_name <- paste0("Data/EIA-923 2001 - 2020/f923_", yr, "/EIA923 SCHEDULES ", yr, ".xls")
        read_xls(file_name, skip = 7) %>%
          rename(eia_id = `Plant ID`) ->
          EIA_data

        EIA_rep_freq <- tibble(eia_id = EIA_IDs_desired, freq = NA_character_)

      }else{
        file_name <- paste0("Data/EIA-923 2001 - 2020/f923_", yr, "/EIA923 SCHEDULES ", yr, ".xlsx")
        read_xlsx(paste0("Data/EIA-923 2001 - 2020/f923_", yr, "/EIA923 SCHEDULES ", yr, ".xlsx"),
                  skip = 5) %>%
          rename(eia_id = `Plant Id`) ->
          EIA_data

        if(yr %in% c(2011, 2013)) id_col_name <- "EIA Plant Id"; rf_name <- "Reporting Frequency (Annual Or Monthly)"
        if(yr %in% c(2012, 2014:2019)) id_col_name <- "Plant Id"; rf_name <- "Reporting\r\nFrequency"
        if(yr == 2020){
          id_col_name <- "Plant Id"
          rf_name <- "Respondent\r\nFrequency"
        }
        read_xlsx(file_name, skip = 4, sheet = "Page 6 Plant Frame") %>%
          select(eia_id = one_of(id_col_name),
                 freq = one_of(rf_name)) ->
          EIA_rep_freq
      }

      # filter EIA file for hydro
      EIA_data %>%
        select(eia_id, contains("Netgen"), type = contains("mover"),
               one_of(c("NET GENERATION (megawatthours)",
                        "Net Generation (Megawatthours)",
                        "Net Generation\r\n(Megawatthours)"))) %>%
        filter(eia_id %in% EIA_IDs_desired, type == "HY") %>%
        select(-type) -> EIA_hydro

      # fix col names
      names(EIA_hydro) <- str_to_lower(names(EIA_hydro)) %>% str_replace("\r\n", "_") %>%
        substr(1,10)

      # make netgen numeric and add frequency of reporting "true_monthly?"
      return(
        tibble(eia_id = EIA_IDs_desired) %>%
          left_join(EIA_hydro, by = "eia_id") %>%
          mutate_if(is.character, as.numeric) %>%
          left_join(EIA_rep_freq, by = "eia_id") %>%
          mutate(year = !!yr)
      )
    }
  }) -> EIA_hydro_netgen_and_freq

# check that annual totals correspond to reported annual
EIA_hydro_netgen_and_freq %>%
  mutate(
    netgen_total = netgen_jan + netgen_feb +
      netgen_mar + netgen_apr + netgen_may +
      netgen_jun + netgen_jul + netgen_aug +
      netgen_sep + netgen_oct + netgen_nov + netgen_dec
  ) %>%
  ggplot(aes(`net genera`, netgen_total)) + geom_point() +
  facet_wrap(~year) +
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")

EIA_hydro_netgen_and_freq %>%
  left_join(hydrosource_EIA, by = "eia_id") %>%
  rename(netgen_annual = `net genera`,
         state = State, nameplate_MW = CH_MW) %>%
  readr::write_csv("Output_1_EIA_MWh.csv")
