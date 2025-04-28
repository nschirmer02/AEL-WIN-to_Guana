#install pacman package for streamlined package management
#install.packages(pacman)

##load all packages required for script
#readr, readxl, openxlsx: all handle excel workbook files
#dplyr: streamlined dataframe mutation, home to the pipe operator "%>%"
#lubridate: easily manipulate date and time data 
#arsenal: enables comparison of data frame formatting and values prior, very handy when attempting to reformat and bind two dataframes
pacman::p_load(here, readr, readxl, openxlsx, dplyr, lubridate, tidyr, tidyverse, arsenal)


