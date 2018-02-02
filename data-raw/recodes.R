recodes <- bind_rows(
  tibble(year = 2002:2016,
         variable = c("A_MMSA", rep("_MMSA", 14)),
         recode = "geoid"),
  tibble(year = 2002:2016,
         variable = c("A_MMSAWT", rep("_MMSAWT", 14)),
         recode = "msawt"),
  tibble(year = 2002:2016,
         variable = c(rep('DIABETES', 2), rep('DIABETE2', 7), rep('DIABETE3', 6)),
         recode = first(variable)),
  tibble(year = 2002:2016,
         variable = c(rep('SMOKEDAY', 3), rep('SMOKDAY2', 12)),
         recode =  "SMOKEDAY")
)

devtools::use_data(recodes)
