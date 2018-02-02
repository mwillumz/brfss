#' Download BRFSS SMART data
#'
#' @description
#' `fetch_brfss()` is a convenient wrapper to download BRFSS SMART data.
#'
#' @param year A year or vector of years between 2002 & 2016.
#' @param recode Whether to recode select variables so that variable names are consistent over time.
#' @return A tbl.
#' @export
fetch_brfss <- function(year = 2016, recode = TRUE){
  lapply(year, function(y){
    Zip <- file.path(tempdir(), paste0("brfss",y,".zip"))

    y2 <- if(y <= 2012){
      paste0(substr(y, 3, 4), "xpt.zip")
    } else{
      paste(y, "XPT.zip", sep = "_")
    }

    paste0("https://www.cdc.gov/brfss/smart/", y, "/MMSA", y2) %>%
      download.file(Zip)

    Data <- unzip(Zip, exdir = tempdir()) %>%
      read_xpt()

    #standardize select variable names
    if(recode == TRUE){
      Recode <- filter(brfss::recodes, year == y) %>%
        select(recode, variable) %>%
        deframe()
      Data <- rename(Data, !!!Recode)
    }

    mutate(Data, year = y)
  }) %>%
    bind_rows()
}


