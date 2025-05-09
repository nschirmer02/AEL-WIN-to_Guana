#install pacman package for streamlined package management
#install.packages(pacman)

##load all packages required for script
#readr, readxl, openxlsx: all handle excel workbook files
#dplyr: streamlined dataframe mutation, home to the pipe operator "%>%"
#lubridate: easily manipulate date and time data 
#arsenal: enables comparison of data frame formatting and values prior, very handy when attempting to reformat and bind two dataframes
pacman::p_load(here, readr, readxl, openxlsx, dplyr, lubridate, tidyr, tidyverse, arsenal)




field <- readxl::read_excel(mstr_output, "2025")

dt <- field$SampleDate[853:nrow(field)]

dt <- as.POSIXct(dt, 
                 tz = "America/New_York", 
                 format = "%Y/%m/%d %H:%M:%S")
print(dt[1])

xl_dt <- field$SampleDate[1:852]
posix_date <- as.POSIXct(as.numeric(xl_dt) * 86400, 
                         origin = "1899-12-30", 
                         tz = "UTC", 
)



posix_date <- lubridate::round_date(posix_date, unit = "second")
posix_date <- lubridate::force_tz(posix_date, tzone = "America/New_York")

dates <- list(c(posix_date, dt))
field$SampleDate <- dates[[1]]

class(win_format$SampleDate)

class(field$SampleDate)
format(field$SampleDate)
print(field$SampleDate)
