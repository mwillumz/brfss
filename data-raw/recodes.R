library(tidyverse)

recodes <- bind_rows(
  tibble(year = 2002:2016,
         variable = c(rep("ASTHMA2", 9), rep("ASTHMA3", 6)),
         recode = "ASTHMA"),
  tibble(year = 2002:2016,
         variable = c(rep("CVDCRHD2", 3), rep("CVDCRHD3", 2), rep("CVDCRHD4", 10)),
         recode = "CVDCRHD"),
  tibble(year = c(2002:2005, 2007, 2009, 2011, 2013, 2015),
         variable = c(rep('BPHIGH3', 3), rep('BPHIGH4', 6)),
         recode = 'BPHIGH'),
  tibble(year = c(2002:2005, 2007, 2009, 2011, 2013, 2015),
         variable = c(rep('TOLDHI2', 9)),
         recode = 'TOLDHI'),
  tibble(year = 2002:2016,
         variable = c(rep('DIABETES', 2), rep('DIABETE2', 7), rep('DIABETE3', 6)),
         recode = first(variable)),
  tibble(year = 2002:2016,
         variable = c(rep('SMOKEDAY', 3), rep('SMOKDAY2', 12)),
         recode =  "SMOKEDAY"),
  tibble(year = 2002:2016,
         variable = c(rep('WEIGHT', 2), rep('WEIGHT2', 13)),
         recode = 'WEIGHT'),
  tibble(year = 2002:2016,
         variable = c(rep('HEIGHT', 2), 'HEIGHT2', rep('HEIGHT3', 12)),
         recode = 'HEIGHT'),
  tibble(year = 2002:2016,
         variable = c(rep('EXERANY2', 15)),
         recode = 'EXERANY')
)

devtools::use_data(recodes, overwrite = TRUE)
