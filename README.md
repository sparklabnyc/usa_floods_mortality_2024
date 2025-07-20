# Large floods drive changes in cause-specific mortality in the United States
Victoria D Lynch, Johnathan Sullivan, Aaron Flores, Sarika Aggarwal, Rachel C Nethery, Marianthi-Anna Kioumourtzoglou, Anne E Nigra, Robbie M Parks. Nature Medicine. 2025

## Project description

This dataset and code is used for the paper

https://www.nature.com/articles/s41591-024-03358-z

## 1. Data
1a_exposure_data: flood data from Global Flood Database 

1b_outcome_data: mortality data - only on local computer 

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
Supplmentary tables

## 5. Figures
All figures for manuscript and supplement 

note: please run create_folder_structure.R first to create folders which may not be there when first loaded.

## Directory structure

```md
.
├── 01_data
│   ├── 1a_death_raw
│   │   └── mort1959_template.csv
│   ├── 1b_population_raw
│   │   ├── Bridged-Race Population Estimates 1990-1995.txt
│   │   ├── Bridged-Race Population Estimates 2001-2005.txt
│   │   ├── Bridged-Race Population Estimates 2006-2010.txt
│   │   ├── Bridged-Race Population Estimates 2011-2015.txt
│   │   ├── Bridged-Race Population Estimates 2016-2020.txt
│   │   ├── Single-Race Population Estimates 2020-2022 by State and Single-Year Age_2020-2022.txt
│   │   ├── co-asr-7079.xlsx
│   │   └── pe-02.xlsx
│   └── 1c_icd
│       ├── icd7.csv
│       ├── icd8.csv
│       ├── icd9.csv
│       └── icd10.csv
├── 02_code
│   ├── 2a_functions
│   │   └── 01_data_formatting
│   │       └── 02_data_processing.R
│   ├── 2b_data_prep
│   │   ├── 01_population_estimation.R
│   │   ├── 02_yearly_summary_without_population.R
│   │   ├── 03_all_years_summary_with_population.R
│   │   ├── 04_all_cause_summary.R
│   │   └── 05_icd_decode.R
│   └── 2c_data_exploration
│       └── 01_plot.R
├── 03_output
│   ├── 3a_death_processed
│   │   ├── summary_1959.rds
│   │   ├── summary_1960.rds
│   │   ├── ...
│   │   └── summary_2022.rds
│   ├── 3b_population_processed
│   │   ├── year_1970.rds
│   │   ├── ...
│   │   └── year_2022.rds
│   └── 3c_combined_summary
│       ├── 01_cause_not_specified
│       │   ├── summary_1970_1989_with_population.rds
│       │   └── summary_1970_1989_without_population.rds
│       └── 02_cause_specified
│           └── all_causes_summary.rds
├── 04_figure
│   └── Summary Plots.pdf
├── renv
├── renv.lock
├── README.md
├── nber_mortality_process_2024.Rproj
└── workflow_diagram.jpg
```

## Data Availability 
Flood data used in this analysis are available via https://github.com/vdl2103/usa_floods_mortality/tree/main/01_data/1a_exposure_data

Mortality data is available from https://www.cdc.gov/nchs/nvss/bridged_race.htm


