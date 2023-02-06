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

Datafiles: `build/input/1_ES_demand_create_up_clean/demand_Spain/UPSalida_2018-01-01.

> To link Spanish Programming Units codes with their Market Subjects, and their corresponding retailing companies, we use the following dataset constructed from two structural files from ESIOS REE. Sources: https://www.esios.ree.es/es/unidades-de-programacion and .https://www.esios.ree.es/es/sujetos-del-mercado.

Datafiles: `build/input/1_ES_demand_create_up_clean/UP_market_subject_links.csv`.

> Portuguese electricity consumption data are obtained from National Market Operator (OMIE) between 2018 and 2021 on an hourly basis and aggregated for all clients under the regulated tariff. Originial dat file is converted into csv for faster loading.

Datafiles: `build/input/4_create_ES_PT_demand_by_dist/data_aggregate_iberian_market_mar22.csv`.


> Electricity regulated prices for Spain are obtained from the public Archives of the National Network Operator (REE) between 2018 and 2021. There is hourly frequency at the country level.

Datafiles: 'analysis/input/hourly_pvpc_prices_and_charges_sept_2021'.

#### Consumers

> Quarterly data on Spanish consumers comes from several reports annexes by CNMC, Spanish Regulation Authority: IS MERCADO MINORISTA DE ELECTRICIDAD. Sources: https://www.cnmc.es/expedientes/isde02719 and https://www.cnmc.es/expedientes/isde02721. Most recent data for 2021, which are not yet published, have been kindly provided by CNMC.

Datafile: `build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_11_19.csv`.
Datafile: `build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_16_20.csv`.
Datafile: `build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_21.csv`.

We use the following files to link regulated distribution names with distribution groups

Datafile: `build/input/3_create_ES_reg_consumer_by_dist/traditional_retailers_list.csv`.

> Monthly data on Portuguese consumers comes from several digitised reports by ERSE, Portuguese Regulation Authority: Boletim do Mercado Liberalizado de eletricidade. Original pdf files can be found at https://www.erse.pt/eletricidade/supervisao-do-mercado/boletins/.

Datafile: `build/input/2_create_PT_consumers_clean/PT_consumers.csv`.

#### Temperature

> Temperature data come from MERRA for Spain.

Datafile: `build/input/1_ES_demand_create_up_clean/EStemp.csv`.

> Temperature data come from MERRA for Portugal.

Datafile: `build/input/4_create_ES_PT_demand_by_dist/PTtemp.csv`.

#### Google Trends

> Weekly historic data on searches in Google concerning new regulation in Spain are obtained webscraping through R library `gtrendsR`.

Datafile: `build/input/4_create_ES_PT_demand_by_dist/gtrends_dist_long`.

Computational requirements
---------------------------

### Software Requirements

- Julia (code was last run with version 1.6.3)
- The code "`0_setup_julia.jl`" will install all dependencies. It should be run once.

- R (code was last run with version 4.2.2)
- The code "`0_setup_R.R`" will install all dependencies. It should be run once.
- To execute code in R, it is recommended to set "UTF-8" as defaulted encoding to correctly read characters of some variables.


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

* build folder

- The code `0_setup.jl` installs all required dependencies in Julia.
- The code `0_setup.R` installs all required dependencies in R.
- The code `build/code/1_ES_demand_create_UP_clean.R` links demand at UP level (Unidad de Programación) with their corresponing retailers and adds temperature and other controls and creates `build/output/up_clean.csv`. 
		This file uses aggregated temperature data `ES_temp.csv` that will be assigned to each distribution area weighted on population. 
- The code `build/code/2_create_PT_consumers_clean.jl` cleans Portugal data on consumers and builds `build/output/PT_consumers_clean.csv`.
- The code `build/code/3_create_ES_reg_consumer_by_dist.jl` combines all data about Spanish consumers, interpolates quarterly data to monthly level for each distribution area and creates final file `build/output/ES_reg_consumers_by_dist.csv`.
- The code `build/code/4_create_ES_PT_demand_by_dist.jl` combines electricity demand data both for Spain and Portugal, adds data on consumers, temperature for Portugal and Google Trends index. 
		It creates final file ready for analysis `analysis/input/ES_PT_demand_by_dist.csv`.

* Analysis folder

- The code `1_descriptive_analysis` carries out some descriptive analysis.
- The code `2_lasso_rf_estimation` it conducts the first step by estimating a LASSO (and Random Forest) model, and computes prediction error that will be used in second step. It generates `analysis/input/data/df_lasso_rf.csv` as well as some validity checks
- The code `3_main_regressions` conducts the panel fixed effects regressions, the second step in the LASSO method and the price elasticity analysis

Instructions to Replicators
---------------------------

- Run `main_windows.bat` (for Windows users) or `main_macOS.sh` (for MAC OS users) to generate all the outputs sequencially. To run Julia from the command line, add Julia to the PATH environment variable. See https://julialang.org/downloads/platform. 

