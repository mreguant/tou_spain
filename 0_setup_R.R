# Install R libraries

packages <- c("dplyr", "lubridate", "tidyverse", "foreign", "haven", "ggplot2",
              "magrittr", "HistogramTools", "ncdf4")


install.packages(setdiff(packages, rownames(installed.packages()))) 
