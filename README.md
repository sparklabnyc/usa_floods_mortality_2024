# Large floods drive changes in cause-specific mortality in the United States
Victoria D Lynch, Johnathan Sullivan, Aaron Flores, Sarika Aggarwal, Rachel C Nethery, Marianthi-Anna Kioumourtzoglou, Anne E Nigra, Robbie M Parks. Nature Medicine. 2025

## Project description

This dataset and code is used for the paper

https://www.nature.com/articles/s41591-024-03358-z

## 1. Data
1a_exposure_data: county-level flood data from Global Flood Database and flood cause type from ('FloodArchive.csv') from Dartmouth Flood Obsevatory

1b_outcome_data: mortality data - only on local computer. Cause-specific mortality data with geographic information can be requested through the National Center for Health Statistics (NCHS) at https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm

1c_supportive_datasets: files used to help analysis (e.g. population weights, fips-to-state etc.)

## 2.Code 

### 2a.Data prep code
a_00_get_county_pop: code to read-in county- and month-specific population estimates from Robbie Parks' CDC Monthly Population Inference project; also includes alternative approach for annual population estimates from US Census Bureau and SEER data. Output saved in 1c. supportive datasets folder; no need to run 

a_00_process_nchs_mortality: do not run locally; code to process mortality data 

a_01_prep_exposure_data: add flood type to Global Flood Database data; identify floods missing from GFD

a_02_create_exposure_variables: create flood exposure variables by population thresholds 

a_03_add_lags_to_floods: create lagged flood exposure variables 

### 2b.Data exploration code
b_00_compare_flood_datasets: identify flood events in DFO and NCEI that are not currently in GFD; no need to run unless specifically comparing flood datasets

b_01_flood_event_eda: barplots and histograms to assess the number and duration of flood events by flood cause 

b_02_flood_eda_maps: maps of flood count by county and by flood cause (cyclonic storms, heavy rain, rain and snow, ice jams and dam breaks)

b_03_mortality_eda: code for mortality plots

b_04_exposure_histogram: code to make exposure threshold histogram

b_05_manuscript_values: code to show how specific values in manuscript are calculated

### 2c.Modeling code
c_00_create_subcause_group_datasets: creates separate .rds for each mortality cause and subgroup (age,sex) including refined and coarse (over/under 65-yo) age categories

c_01_model_comparison: compare models with different dfs, family options, hyper priors

c_02_run_model: run model by mortality cause group, flood cause, exposure threshold, and subgroup (age, sex)

c_03_run_all_cause_model: run model for all mortality causes together by flood cause

c_04_DPM: calculate monthly deaths per million (DPM) attributable to flood exposure by flood cause, severity, and lag 

### 2d.Model plotting
d_01_plot_model_output: plot results

d_02_plot_model_all_cause: plot results for all mortality cause analysis 

### 20.Functions
01_data_processing_functions: functions to process mortality data and create separate .rds for mortality causes and subgroups

02_eda_functions: currently empty - plan to add function for mapping 

03_model_development_functions: function for model comparison

04_model_functions: functions to run model

05_model_plotting_functions: function to plot model output 

script_initiate.R

## 3.Output
3a_eda_output: initial tables and figures 

3b_model_output: .csv of model results 

## 4. Tables
Supplementary tables

## 5. Figures
All figures for manuscript and supplement 

note: please run create_folder_structure.R first to create folders which may not be there when first loaded.

## Directory structure

```md
.
├── 01_data
│   ├── 1a_exposure_data
│   │   ├── FloodArchive.csv
│   │   ├── dfo_usa_county_panel_20220629.csv
│   │   ├── floods_not_in_gfd.csv
│   │   ├── gfd_county_panel.csv
│   │   ├── gfd_usa_county_panel_20230224.csv
│   │   ├── gfd_with_flood_type.csv
│   │   └── ncei_usa_county_panel_20230222.csv
│   ├── 1b_outcome_data
│   │   └── mortality_cs_fips_sex_age_2001_2018.csv
│   ├── 1c_supportive_datasets
│   │   └── fips_to_state.csv
│   ├── map_objects.R
│   └── objects.R
├── 02_code
│   ├── 20_functions
│   │   ├── 01_data_processing_functions.R
│   │   ├── 02_eda_functions.R
│   │   ├── 03_model_development_functions.R
│   │   ├── 04_model_functions.R
│   │   ├── 05_model_plotting_functions.R
│   │   └── script_initiate.R
│   ├── 2a_data_prep
│   │   ├── a_00_get_county_pop.R
│   │   ├── a_00_process_nchs_mortality.R
│   │   ├── a_01_prep_exposure_data.R
│   │   ├── a_02_create_exposure_variables.R
│   │   ├── a_03_add_lags_to_floods.R
│   │   └── load_data.R
│   ├── 2b_data_exploration
│   │   ├── b_00_compare_flood_datasets.R
│   │   ├── b_01_floods_eda.R
│   │   ├── b_02_flood_eda_maps.R
│   │   ├── b_03_mortality_eda.R
│   │   ├── b_04_exposure_histogram.R
│   │   └── b_05_manuscript_values.R
│   ├── 2c_models
│   │   ├── c_00_create_subcause_group_datasets.R
│   │   ├── c_01_model_comparison.R
│   │   └── c_02_run_model.R
│   ├── 2d_model_plotting
│   │   └── d_01_plot_model_output.R
│   └── packages_to_load.R
├── 03_output
│   ├── 3a_eda_output
│   │   ├── maps_flood_exposure_area.jpeg
│   │   ├── maps_flood_exposure_pop.jpeg
│   │   ├── maps_flood_type_any_pop_expo.jpeg
│   │   ├── seasonal_mortality.jpeg
│   │   └── ts_data.jpeg
│   ├── 3b_model_output
│   └── model_comparison_table.csv
├── 04_tables
├── 05_figures
├── 06_literature
├── 07_drafts
```

## Data Availability 
Flood data used in this analysis are available via https://github.com/vdl2103/usa_floods_mortality/tree/main/01_data/1a_exposure_data

Mortality data is available from https://www.cdc.gov/nchs/nvss/bridged_race.htm


