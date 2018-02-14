#' Divisions to MSAs
#'
#' @description
#' Collapse multiple divisons to one MSA
#'
#' @param data Data set
#' @export
divisions_to_msas <- function(data){
  left_join(data, brfss:::Divisions, by = c('geoid' = 'division')) %>%
    mutate(geoid = if_else(is.na(msa), geoid, msa)) %>%
    select(-msa)
}
