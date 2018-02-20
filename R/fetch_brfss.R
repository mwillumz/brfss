#' Download BRFSS SMART data
#'
#' @description
#' `fetch_brfss()` is a convenient wrapper to download BRFSS SMART data.
#'
#' @param year A year or vector of years between 2002 & 2016.
#' @param recode Whether to recode select variables so that variable names are consistent over time.
#' @return A tbl.
#' @import dplyr
#' @export
fetch_brfss <- function(year = 2016, recode = TRUE, smart = TRUE, filepath = NULL){

  BaseUrl <- "https://www.cdc.gov/brfss"

  lapply(year, function(y){
    Zip <- file.path(tempdir(), paste0("brfss",y,".zip"))

    Recodes <- if(smart){
      file <- if(y <= 2012){
        paste0("MMSA", substr(y, 3, 4), "xpt.zip")
      } else{
        paste0("MMSA", y, "_XPT.zip")
      }

      if(is.null(filepath)){
        file.path(BaseUrl, "smart", y, file) %>%
          download.file(Zip)
      }

      Filename <- if(y <= 2012){
        paste0("MMMSA", substr(y, 3, 4), ".xpt")
      } else{
        paste0("MMSA", y, ".xpt")
      }

      bind_rows(
        tibble::tibble(year = 2002:2016,
               variable = c("A_MMSA", rep("_MMSA", 14)),
               recode = "geoid"),
        tibble::tibble(year = 2002:2016,
               variable = c("A_MMSAWT", rep("_MMSAWT", 14)),
               recode = "wt")
      )
    } else{
      file <- if(y <= 2010){
        paste0("CDBRFSS", substr(y, 3, 4), "XPT.zip")
      } else{
        paste0("LLCP", y, "XPT.zip")
      }

      #add filename for brfss data

      if(is.null(filepath)){
        file.path(BaseUrl, "annual_data", y, "files", file) %>%
          download.file(Zip)
      }

      bind_rows(
        tibble::tibble(year = 2002:2016,
               variable = rep("_STATE", 15),
               recode = "geoid")
        ,
        tibble::tibble(year = 2002:2016,
               variable = rep("_LLCPWT", 15),
               recode = "wt")
      )
    }

    Data <- if(is.null(filepath)){
      unzip(Zip, exdir = tempdir()) %>%
        haven::read_xpt()
    } else{
      haven::read_xpt(file.path(tempdir(), Filename))
    }

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


