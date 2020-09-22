# Libraries 
library(tidyverse)
library(readxl)
library(lubridate)
# File path ----
xl_file_path <- "00_data/test_data.xlsx"

data_tbl <- readxl::read_excel(
    path  = xl_file_path,
    sheet = "data"
)

data_tbl %>% glimpse()


fn_aggr <- function(
    data,
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31"),
    ...,
    col_aggr = total_price
) {
    startdate <- lubridate::as_date(startdate)
    enddate <- lubridate::as_date(enddate)
    
    groupbyvar <- quos(...)
    summvar <- enquo(col_aggr)
    
    
    output_tbl <- data %>% 
        filter(between(lubridate::as_date(date), startdate, enddate)) %>% 
        group_by(!!! groupbyvar) %>% 
        summarise(sum = sum(!! summvar, na.rm = T),
                  .groups = "drop")
    
    return(output_tbl)
}



data_tbl %>% 
    fn_aggr(
        startdate = "2019-01-01",
        enddate = "2019-12-31",
        date,biketype,
        col_aggr = total_price
    )


data_tbl %>% 
    filter(between(lubridate::as_date(date), 
                   startdate,
                   enddate
    )
) %>% 
    group_by( date,biketype) %>% 
    summarise(sum = sum(total_price, na.rm = T),
              .groups = "drop")

