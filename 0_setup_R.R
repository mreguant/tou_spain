# Install R libraries

dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

packages <- c("dplyr", "lubridate", "tidyverse", "foreign", "haven", "ggplot2",
              "magrittr", "HistogramTools", "ncdf4","magrittr","glmnet","fastDummies","lfe")

install.packages(packages, lib = Sys.getenv("R_LIBS_USER"), repos = "https://cran.rstudio.com/")

