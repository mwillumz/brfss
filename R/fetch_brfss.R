#' Download BRFSS SMART data
#'
#' @description
#' `fetch_brfss()` is a convenient wrapper to download BRFSS SMART data.
#'
#' @param year A year or vector of years between 2002 & 2016.
#' @param recode Whether to recode select variables so that variable names are consistent over time.
#' @return A tbl.
#' @export
fetch_brfss <- function(year = 2016, recode = TRUE, smart = TRUE){

  BaseUrl <- "https://www.cdc.gov/brfss"

  lapply(year, function(y){
    Zip <- file.path(tempdir(), paste0("brfss",y,".zip"))

    Recodes <- if(smart){
      file <- if(y <= 2012){
        paste0("MMSA", substr(y, 3, 4), "xpt.zip")
      } else{
        paste0("MMSA", y, "_XPT.zip")
      }

      file.path(BaseUrl, "smart", y, file) %>%
        download.file(Zip)

      bind_rows(
        tibble(year = 2002:2016,
               variable = c("A_MMSA", rep("_MMSA", 14)),
               recode = "geoid"),
        tibble(year = 2002:2016,
               variable = c("A_MMSAWT", rep("_MMSAWT", 14)),
               recode = "wt")
      )
    } else{
      file <- if(y <= 2010){
        paste0("CDBRFSS", substr(y, 3, 4), "XPT.zip")
      } else{
        paste0("LLCP", y, "XPT.zip")
      }
      file.path(BaseUrl, "annual_data", y, "files", file) %>%
        download.file(Zip)

      bind_rows(
        tibble(year = 2002:2016,
               variable = rep("_STATE", 15),
               recode = "geoid")
        ,
        tibble(year = 2002:2016,
               variable = rep("_LLCPWT", 15),
               recode = "wt")
      )
    }

    Data <- unzip(Zip, exdir = tempdir()) %>%
      read_xpt()

    #standardize select variable names
    if(recode == TRUE){
      Recode <- bind_rows(brfss::recodes, Recodes) %>%
        filter(year == y) %>%
        select(recode, variable) %>%
        deframe()
      Data <- rename(Data, !!!Recode) %>%
        mutate(geoid = as.character(geoid))
    }

    mutate(Data, year = y)
  }) %>%
    bind_rows()
}


