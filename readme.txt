RectifHyd v1.3.0 (released 2024-05-07)

Creators:
- Sean Turner (1) - turnersw@ornl.gov
- Cameron Bracken (2) - cameron.bracken@pnnl.gov

1. Oak Ridge National Laboratory
2. Pacific Northwest National Laboratory

Scripts for reproducing RectifHyd:
https://github.com/swd-turner/turner_voisin_nelson_2022_scientific_data

See Zenodo for DOI and citation

Column descriptions:
EIA_ID: Plant ID used in Energy Information Administration.
plant: Plant name.
state: State in which the plant is located (two letter abbreviation).
year: Year of generation.
month: Month of generation.
EIA_obs_freq: denotes whether observations Annual ("A") or monthly ("M") resolution.
RectifHyd_method: denotes data used to disaggregate annual generation (release => reservoir release; flow => downstream flow gage).
EIA_fraction: fraction used by EIA for annual to monthly disaggregation.
EIA_MWh: monthly net energy estimate in EIA-923.
RectifHyd_fraction: updated fraction for annual to monthly generation
RectifHyd_MWh: ""rectified"" monthly net energy estimate."
recommended_data: suggests whether user should adopt RectifHyd or original EIA-923 for specific plant/year.
smoothed: Flag indicating that a monthly data value was smoothed during the QA/QC process. Smoothing is applied if any monthly generation exceeds the nameplate capacity of a plant or if it exceeds 25% of the annual generation.
scaled: Flag indicating that a monthly data value was scaled during the QA/QC process. Scaling is applied only when smoothing is not sufficient to ensure nameplate capacity of a plant is not exceeded or if it exceeds 25% of the annual generation. Scaling preserved the monthly genration shape but does not exceed nameplate capacity. Introduced in version 1.3.0
imputed: Flag indicating that a monthly data value was imputed and was originally missing from the EIA data or determined to be erronious during the QA/QC process. Imputation of monthly EIA generation values was introduced in version 1.3.
