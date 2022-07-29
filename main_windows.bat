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
ECHO Add Julia to the PATH to run Julia from the command line. See: https://julialang.org/downloads/platform/ 
SET PATH = %PATH%
ECHO Alternatively, type the path were the Julia program is located.
ECHO Example: C:\Users\Alejandro\AppData\Local\Programs\Julia-1.6.2\bin
SET /p JULIA_PATH= Path to Julia's program (ommit this step by pressing Enter): 
SET PATH=%PATH%;%JULIA_PATH%

ECHO =
ECHO =====================================================================
ECHO Required packages in Julia are being installed.
JULIA 0_setup_julia.jl

ECHO =
ECHO =====================================================================
ECHO Required packages in R are being installed.
0_setup_R.R??????????????????

ECHO =
ECHO =====================================================================
ECHO Required packages in Python are being installed.
pip uninstall DataTime
pip uninstall numpy
pip uninstall pandas
pip install DataTime
pip install numpy
pip install pandas

ECHO =
ECHO =====================================================================
ECHO Cleaning datasets . . .
JULIA build\code\4_PTtemp.jl
JULIA build\code\5_create_PT_cons_MR_ML_clean.jl
JULIA build\code\6_create_ES_reg_consumer_by_dist.jl
JULIA build\code\99_create_ES_PT_demand_by_dist.jl

ECHO =
ECHO =====================================================================
ECHO Running analysis to generate tables and figures . . .
JULIA analysis\code\1_descriptive_analysis.jl
JULIA analysis\code\3_panel_FE_lasso.jl
ECHO END.

ECHO =
ECHO =====================================================================
ECHO =					
ECHO =			REPLICATION HAS BEEN SUCCESSFUL		
ECHO =					
ECHO =====================================================================
PAUSE

