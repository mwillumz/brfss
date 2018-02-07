library(brfss); library(stringr)
library(tidyverse)

Data <- fetch_brfss(2002:2016, smart = TRUE)

#create function for aggregating from divisions to msas
Temp <- Data %>%
  divisions_to_msas() %>%
  mutate(htm = case_when(between(HEIGHT, 200, 211) ~ (24 + HEIGHT - 200) * 0.0254,
                         between(HEIGHT, 300, 311) ~ (36 + HEIGHT - 300) * 0.0254,
                         between(HEIGHT, 400, 411) ~ (48 + HEIGHT - 400) * 0.0254,
                         between(HEIGHT, 500, 511) ~ (60 + HEIGHT - 500) * 0.0254,
                         between(HEIGHT, 600, 611) ~ (72 + HEIGHT - 600) * 0.0254,
                         between(HEIGHT, 700, 711) ~ (84 + HEIGHT - 700) * 0.0254,
                         between(HEIGHT, 9000, 9776) ~ (HEIGHT - 9000) / 100),
         weight = str_pad(WEIGHT, width = 4, side = "left", pad = "0"),
         metric = if_else(substr(weight, 1, 1) == '9', TRUE, FALSE),
         wtkg = case_when(metric == FALSE & as.numeric(substr(weight, 2, 4)) < 777 ~
                            as.numeric(substr(weight, 2, 4)) * 0.4535924,
                          metric == TRUE & as.numeric(substr(weight, 2, 4)) < 999 ~
                            as.numeric(substr(weight, 2, 4))),
         bmi = if_else(PREGNANT == 1, NA_real_, wtkg/(htm ** 2), wtkg/(htm ** 2)) %>%
           round(2)
  ) %>%
  select(-weight, -htm, -metric, -wtkg) %>%
  mutate(
    cholesterol = case_when(BLOODCHO == 1 & TOLDHI == 1 ~ 'yes',
                                 BLOODCHO == 1 & TOLDHI == 2 ~ 'no'),
         smoker = case_when(SMOKE100 == 1 & SMOKEDAY %in% 1:2 ~ 'current',
                            SMOKE100 == 1 & SMOKEDAY == 3 ~ 'former',
                            SMOKE100 == 2 ~ 'never')) %>%
  group_by(year, geoid) %>% #change to numerator denominator function
  summarise(asthma = sum(if_else(ASTHMA == 1, wt, 0), na.rm = TRUE) /
              sum(if_else(ASTHMA %in% 1:2, wt, 0), na.rm = TRUE),
            cancer = sum(if_else(CHCSCNCR == 1 | CHCOCNCR == 1, wt, 0), na.rm = TRUE) /
                sum(if_else(CHCSCNCR %in% 1:2 | CHCOCNCR %in% 1:2, wt, 0), na.rm = TRUE),
            cholesterol = sum(if_else(cholesterol == 'yes', wt, 0), na.rm = TRUE) /
              sum(if_else(cholesterol %in% c('yes', 'no'), wt, 0), na.rm = TRUE),
            diabetes = sum(if_else(DIABETES == 1, wt, 0), na.rm = TRUE) /
              sum(if_else(DIABETES %in% 1:4, wt, 0), na.rm = TRUE),
            exercise = sum(if_else(EXERANY == 1, wt, 0), na.rm = TRUE) /
              sum(if_else(EXERANY %in% 1:2, wt, 0), na.rm = TRUE),
            general_health = sum(if_else(GENHLTH %in% 1:3, wt, 0), na.rm = TRUE) /
              sum(if_else(GENHLTH %in% 1:5, wt, 0), na.rm = TRUE),
            bphigh = sum(if_else(BPHIGH == 1, wt, 0), na.rm = TRUE) /
              sum(if_else(BPHIGH %in% 1:4, wt, 0), na.rm = TRUE),
            obese = sum(if_else(bmi >= 30 & bmi < 100, wt, 0), na.rm = TRUE) /
                      sum(if_else(bmi >= 12 & bmi < 100, wt, 0), na.rm = TRUE),
            overweight = sum(if_else(bmi >= 25 & bmi < 30, wt, 0), na.rm = TRUE) /
              sum(if_else(bmi >= 12 & bmi < 100, wt, 0), na.rm = TRUE),
            smoker = sum(if_else(smoker == 'current', wt, 0), na.rm = TRUE) /
              sum(if_else(smoker %in% c('current', 'former', 'never'),
                          wt, 0), na.rm = TRUE))

Msa <- getCensus(name="acs5",
                 vintage = 2015,
                 vars=c("NAME", "B01001_001E"),
                 region="metropolitan statistical area/micropolitan statistical area:*",
                 key = ) %>%
  arrange(desc(B01001_001E))

# filter(Thing, !geoid %in% Msa$metropolitan.statistical.area.micropolitan.statistical.area) %>%
#   filter(!geoid %in% DivisionsAll$division) %>%
#   filter(!geoid %in% c("26180", "37700", "42260")) %>% # Honolulu, Pascagoula, Sarasota
#   filter(!geoid %in% c("10020", "25660", "40500")) %>% #unknown
#   filter(!geoid %in% c("30100", "42580", "37380", "37764", "48740")) #Lebanon, Seaford, Palm Coast, Peabody, Willimantic
