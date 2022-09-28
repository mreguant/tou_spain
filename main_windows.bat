@ECHO OFF
ECHO =====================================================================
ECHO = 									 	
ECHO =   This file will generate all the outputs included in the paper.  
ECHO = 									 
ECHO =====================================================================

ECHO The path of the "tou_spain" repository is defined as current directory:
ECHO Current directory: %~dp0
CD %~dp0

ECHO =
ECHO =====================================================================
SET PATH = %PATH%
ECHO Julia files: type the path were the Julia program is located.
ECHO Example: C:\Users\Jacint Enrich\AppData\Local\Programs\Julia-1.6.3\bin
ECHO Or C:\Users\JacintE\AppData\Local\Programs\Julia-1.6.2\bin
SET /p JULIA_PATH= Path to Julia program (ommit this step by pressing Enter): 
SET PATH=%PATH%;%JULIA_PATH%

ECHO R files: type the path were the R program is located.
ECHO Example: C:\Program Files\R\R-4.2.1\bin
ECHO Or C:\Program Files\R\R-4.1.1\bin
SET /p R_PATH= Path to R program (ommit this step by pressing Enter): 
SET PATH=%PATH%;%R_PATH%


ECHO =
ECHO =====================================================================
ECHO Required packages in Julia are being installed.
ECHO 0_setup_julia.jl

ECHO =
ECHO =====================================================================
ECHO Required packages in R are being installed.
ECHO Rscript 	0_setup_R.R



ECHO =
ECHO =====================================================================
ECHO Cleaning datasets . . .
Rscript build\code\1_ES_demand_create_up_clean.R
JULIA	 build\code\2_create_PT_consumers_clean.jl
JULIA	 build\code\3_create_ES_reg_consumer_by_dist.JL
JULIA  build\code\99_create_ES_PT_demand_by_dist.jl

ECHO =
ECHO =====================================================================
ECHO Running analysis to generate tables and figures . . .
JULIA 	analysis\code\1_descriptive_analysis.jl
Rscript 	analysis\code\2_lasso_model.R
JULIA		analysis\code\3_panel_FE_lasso.jl
Rscript 	analysis\code\4_google_trends.R

ECHO END.


ECHO =
ECHO =====================================================================
ECHO =					
ECHO =			REPLICATION HAS BEEN SUCCESSFUL		
ECHO =					
ECHO =====================================================================
PAUSE

