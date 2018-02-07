library(censusapi); library(tidyverse)

Divisions <- lapply(c("35620", "31080", "16980", "19100", "37980", "47900",
                      "33100", "14460", "41860", "19820", "42660"), function(d){
                        getCensus(name="acs5",
                                  vintage = 2015,
                                  vars=c("NAME"),
                                  region="metropolitan division:*",
                                  regionin=paste("metropolitan statistical area/micropolitan statistical area", d, sep = ":")
                                  )
                      }) %>%
  bind_rows() %>%
  setNames(c('name', 'msa', 'division')) %>%
  select(msa, division)

DivisionsOld <- tibble(
  msa = c("47900", "14460", "35620", "35620",
          "31080", "19820", "14460", "35620"),
  division = c("13644", "14484", "20764", "35644",
               "42044", "47644", "21604", "44844")
)

Divisions <- bind_rows(Divisions, DivisionsOld)

devtools::use_data(Divisions, internal = TRUE)
