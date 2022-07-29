# Measuring the Impacts of Time-of-Use Electricity Pricing: Evidence from Spain
Authors: Mar Reguant, Jacint Enrich, Ruoyi Li, and Alejandro Mizrahi

Description
--------
This repository replicates the results for the paper "Measuring the Impacts of Time-of-Use Electricity Pricing: Evidence from Spain". The code is written mostly in Julia, but also in R and Python. The file "main_windows" or "main_macOS" runs all of the code to generate the figures and tables in the paper.  

Data Availability and Provenance Statements
----------------------------

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 

### Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

### Details on each Data Source

#### Electricity demand 

> Spanish daily electricity consumption data are obtained from the public Archives of the National Network Operator (REE) between 2018 and 2021. There is hourly frequency and at the level of each market Programming Unit.

Datafiles: `build/input/demand/Spain/P48Cierre_REE_YYYYMMDD_1_OK_1.xml`.

> To link Spanish Programming Units codes with their Market Subjects, and their corresponding retailing companies, we use two structural files from ESIOS REE. Sources: https://www.esios.ree.es/es/unidades-de-programacion and .https://www.esios.ree.es/es/sujetos-del-mercado.

Datafiles: `build/input/demand/Spain/market_subjects.csv`.
Datafiles: `build/input/demand/Spain/programming_units.csv`.

> Portuguese electricity consumption data are obtained from National Market Operator (OMIE) between 2018 and 2021 on an hourly basis and aggregated for all clients under the regulated tariff. Originial dat file is converted into csv for faster loading.

Datafiles: `build/input/demand/Portugal/data_aggregate_iberian_market_mar22.csv`.

#### Consumers

> Quarterly data on Spanish consumers comes from several reports annexes by CNMC, Spanish Regulation Authority: IS MERCADO MINORISTA DE ELECTRICIDAD. Sources: https://www.cnmc.es/expedientes/isde02719 and https://www.cnmc.es/expedientes/isde02721. Most recent data for 2021, which are not yet published, have been kindly provided by CNMC.

Datafile: `build/input/consumers/consumer_raw_11_19.csv`.
Datafile: `build/input/consumers/consumer_raw_16_20.csv`.
Datafile: `build/input/consumers/consumer_raw_21.csv`.

> Data on Spanish consumers comes from several reports by CNMC (Spanish Regulation Authority). 

Datafile: `build/input/consumers/traditional_retailers_list.csv`.

> Monthly data on Portuguese consumers comes from several digitised reports by ERSE, Portuguese Regulation Authority: Boletim do Mercado Liberalizado de eletricidade. Original pdf files can be found at https://www.erse.pt/eletricidade/supervisao-do-mercado/boletins/.

Datafile: `build/input/consumers/PT_cons_ML_MR.csv`.

#### Temperature

> Temperature data come from MERRA for Spain.

Datafile: `build/input/EStemp.csv`.

> Temperature data come from MERRA for Portugal.

Datafile: `build/input/temp/Portugal/merra2_csv_Portugal/merra_YYYY.csv`.

> Contains the coordinates of Portugal's concelhos and distritos. All files with different formats (shp, cpg, dbf, prj, qpj, shx) are used. Source: https://dados.gov.pt/es/datasets/concelhos-de-portugal

Datafile: `build/input/temp/Portugal/concelhos.*`.

> Contains number of population by administrative level NUTS I, NUTS II, NUTS III and municipalities (concelhos). Source: https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_destaques&DESTAQUESdest_boui=133409945&DESTAQUESmodo=2&xlang=en

Datafile: `build/input/temp/Portugal/PTpopulation.csv`.

> Contains the statisticals levels NUTS I, NUTS II and NUTS III of all EU countries. Source: https://ec.europa.eu/eurostat/web/nuts/history

Datafile: `build/input/temp/Portugal/NUTS_2003_2006.xls`.

#### Google Trends

> Weekly historic data on searches in Google concerning new regulation in Spain are obtained webscraping through R library `gtrendsR`.

Datafile: `build/input/gtrends_dist_long`.

Computational requirements
---------------------------

### Software Requirements

- Julia (code was last run with version 1.6.2)
- The code "`0_setup_julia.jl`" will install all dependencies. It should be run once.

- R (code was last run with version 4.1.3)
- The code "`0_setup_R.R`" will install all dependencies. It should be run once.
- To execute code in R, it is recommended to set "UTF-8" as defaulted encoding to correctly read characters of some variables.

- Python (code was last run with version 30.10.5)

### Memory and Runtime Requirements

#### Summary

Approximate time needed to reproduce the analyses on a standard 2022 desktop machine:

- [ ] <10 minutes
- [x] 10-60 minutes
- [ ] 1-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days
- [ ] Not feasible to run on a desktop machine, as described below.

#### Details

The code was last run on a **11th Gen Intel(R) Core(TM) i5-1135G with Windows 10 Pro**. 


Description of programs/code
----------------------------

- The code `0_setup.jl` installs all required dependencies in Julia.
- The code `0_setup.R` installs all required dependencies in R.
- The code `0_setup.py` installs all required dependencies in Python.
- The code `build/code/1_ES_demand_xml_to_csv.py` transforms raw xml files of Spanish electricity demand at Programming Unit (UP, in Spanish) level to more tractable csv files, which are saved in `build/output/demand/UPSalida_YYYY-MM-DD`.
- The code `build/code/2_ES_demand_UP_retailers.R` links the UP codes of the demand files with their corresponing retailers.
- The code `build/code/3_ES_demand_create_UP_clear.R` selects the regulated retailers from the demand files, adds temperature and other controls and creates `build/output/UP_clean.csv`. This file uses aggregated temperature data `build/input/ES_temp.csv` that will be assigned to each distribution area weighted on population. 
- The code `build/code/4_PTtemp.jl` computes average temperature for Portugal based on concelhos data and weighted on population. It saves final file `build/output/PTtemp.csv`.
- The code `build/code/5_create_PT_cons_MR_ML_clean.jl` cleans Portugal data on consumers and builds `build/output/PT_cons_MR_ML_clean.csv`.
- The code `build/code/6_create_ES_reg_consumer_by_dist.jl` combines all data about Spanish consumers, interpolates quarterly data to monthly level for each distribution area and creates final file `build/output/ES_reg_consumers_by_dist.csv`.
- The code `build/code/99_create_ES_PT_demand_by_dist.jl` combines electricity demand data both for Spain and Portugal, adds data on consumers, temperature for Portugal and Google Trends index. It creates final file ready for analysis `analysis/input/ES_PT_demand_by_dist.csv`.
- The code `1_descriptive_analysis` carries out some descriptive analysis using `analysis/input/ES_PT_demand_by_dist.csv`. It generates tables `1_price_variation.tex` and `A1_summary.tex`, and figures `1_consumers.pdf` and `A1_timeseries_demand_per_capita.pdf`.
- The code `2_lasso_first_stage` it conducts first step of LASSO analysis, that is, it computes prediction error that will be used in second step. It generates `analysis/input/df_pred.csv`?
- The code `3_panel_FE_lasso` conducts the panel fixed effects regressions and the second step in the LASSO method. It generates tables `2_DID_placebo_panel_FE.tex` (extended in Appendix as `A2_DID_placebo_panel_FE.tex`), `3_TD_placebo_panel_FE.tex` (extended in Appendix as `A3_TD_placebo_panel_FE.tex`), `4_DID_placebo_LASSO.tex` (extended in Appendix as `A4_DID_placebo_LASSO.tex`), `3_TD_placebo_LASSO.tex` (extended in Appendix as `A3_TD_placebo_LASSO.tex`). It generates figures `2_TD_week_panel_FE.pdf`, `3_TD_weekend_panel_FE.pdf`, `4_TD_week_LASSO.pdf`, `5_TD_weekend_LASSO.pdf`, and `A2_TD_week_dist_panel_FE.pdf`.

Instructions to Replicators
---------------------------

- Run `main_windows.bat` (for Windows users) or `main_macOS.sh` (for MAC OS users) to generate all the outputs sequencially. To run Julia from the command line, add Julia to the PATH environment variable. See https://julialang.org/downloads/platform. 

