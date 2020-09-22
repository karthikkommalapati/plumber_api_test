#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
# Libraries 
library(tidyverse)
library(readxl)
library(lubridate)

# MAIN FUNCTION ----

get_data <- function(
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31")
    
) {
    startdate <- lubridate::as_date(startdate)
    enddate <- lubridate::as_date(enddate)
    
    xl_file_path <- "00_data/test_data.xlsx"
    
    data_tbl <- readxl::read_excel(
        path  = xl_file_path,
        sheet = "data"
    )
    
    output_tbl <- data_tbl %>% 
        filter(between(lubridate::as_date(date), startdate, enddate)) 
    
    return(output_tbl)
    
}



#* @apiTitle Test API

#* Get the Raw Data
#* @param startdate The start date
#* @param enddate The end date

#*
#* @get /testdata
function(
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31")
    
) {
    
    list(
        data = get_data(startdate, enddate)
    )
}

# * /test/summary ----
#* Get the Summary Data
#* @param startdate The start date
#* @param enddate The end date
#* @param ... group by columns
#* @param col_aggr aggregate column

#*
#* @get /testsummary
function(
    data,
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31"),
    ...,
    col_aggr = total_price
    
) {
    
    data_tbl <- get_data(startdate, enddate)
    
    groupbyvar <- quos(...)
    summvar <- enquo(col_aggr)
    
    
    output_tbl <- data_tbl %>% 
        group_by(!!! groupbyvar) %>% 
        summarise(sum = sum(!! summvar, na.rm = T),
                  .groups = "drop")
    
    return(output_tbl)
}


# * /test/summaryby one col ----
#* Get the Summary Data
#* @param startdate The start date
#* @param enddate The end date
#* @param grpcol group by columns
#* @param col_aggr aggregate column

#*
#* @get /testsummaryonecol
function(
    data,
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31"),
    grpcol = date,
    col_aggr = total_price
    
) {
    
    data_tbl <- get_data(startdate, enddate)
    
    groupbyvar <- enquo(grpcol)
    summvar <- enquo(col_aggr)
    
    
    output_tbl <- data_tbl %>% 
        group_by(!! groupbyvar) %>% 
        summarise(total_prc = sum(!! summvar, na.rm = T),
                  .groups = "drop")
    
    return(output_tbl)
}



# * /test/summaryby manual col ----
#* Get the Summary Data
#* @param startdate The start date
#* @param enddate The end date
#* @param grp groupby columns
#* @param col_aggr aggregate column

#*
#* @get /testsummarymanual
function(
    data,
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31"),
    grp = c("Model"),
    col_aggr = "total_price"
    
) {
    
    data_tbl <- get_data(startdate, enddate)
    # grp <- grp %>% strsplit(",") %>% as.list() %>% as.character()
    grp <- grp %>% strsplit("[,]") %>% as.list() 
    
    output_tbl <- data_tbl %>% 
        # group_by(!!! rlang::syms(c("biketype", "Model"))) %>%
        group_by(!!! rlang::syms(grp)) %>%
        # group_by(!!! quos(grp)) %>%
        summarise(total_prc = sum(!! rlang::sym(col_aggr), na.rm = T),
                  .groups = "drop")
    
    return(output_tbl)
}



# * /test/summaryby manual col ----
#* Get the Summary Data
#* @param startdate The start date
#* @param enddate The end date
#* @param grp groupby columns
#* @param col_aggr aggregate column

#*
#* @get /testsummarymanualnew
function(
    data,
    startdate = lubridate::as_date("2019-01-01"),
    enddate   = lubridate::as_date("2019-12-31"),
    grp = "Model",
    col_aggr = "total_price"
    
) {
    
    data_tbl <- get_data(startdate, enddate)
    # grp <- grp %>% strsplit(",") %>% as.list() %>% as.character()
    # grp <- grp %>% strsplit("[,]") %>% as.list() 
    list_elm <- unlist(strsplit(grp, "[,]")) %>% as.list()
    
    grp_lst <- c()
    for (i in seq_along(list_elm)) {
        # print(i)
        grp_lst <- c(grp_lst, list_elm[i])
        
    }
    
    # output_tbl <- data_tbl %>% 
    #     # select(biketype, Model)
    #     select(!!! rlang::syms(grp_lst))
    
    output_tbl <- data_tbl %>% 
        # group_by(!!! rlang::syms(c("biketype", "Model"))) %>%
        group_by(!!! rlang::syms(grp_lst)) %>%
        # group_by(!!! quos(grp)) %>%
        summarise(total_prc = sum(!! rlang::sym(col_aggr), na.rm = T),
                  .groups = "drop")
        
    
    return(output_tbl)
    # return(print(!!! rlang::syms(grp) %>% as.character()))
}