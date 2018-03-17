library(brfss); library(stringr)
library(tidyverse); library(ggplot2)

Data <- fetch_brfss(2002, smart = FALSE, filepath = NULL)

head(DData <- unnest(Data)

table(Data$geoid)

newbie <- function(data, year){
  Data <- if(year >= 2011){
    mutate(data , htm = case_when(between(HEIGHT, 300, 311) ~ (36 + HEIGHT - 300) * 0.0254,
                                  between(HEIGHT, 400, 411) ~ (48 + HEIGHT - 400) * 0.0254,
                                  between(HEIGHT, 500, 511) ~ (60 + HEIGHT - 500) * 0.0254,
                                  between(HEIGHT, 600, 611) ~ (72 + HEIGHT - 600) * 0.0254,
                                  between(HEIGHT, 700, 711) ~ (84 + HEIGHT - 700) * 0.0254,
                                  between(HEIGHT, 9091, 9244) ~ (HEIGHT - 9000) / 100),
           weight = str_pad(WEIGHT, width = 4, side = "left", pad = "0"),
           measurement_type = case_when(substr(weight, 1, 1) == '0' ~ "imperial",
                                        substr(weight, 1, 1) == '9' ~ "metric",
                                        TRUE ~ "unknown"),
           weight = as.numeric(substr(weight, 2, 4)),
           wtkg = case_when(measurement_type == "imperial" & between(weight, 50, 649) ~ weight * 0.4535924,
                            measurement_type == "metric" & between(weight, 23, 294) ~ weight),
           bmi = if_else(PREGNANT == 1, NA_real_, wtkg/(htm ** 2), wtkg/(htm ** 2)) %>%
             round(2)
    )
  } else{
    mutate(data, htm = case_when(between(HEIGHT, 200, 211) ~ (24 + HEIGHT - 200) * 0.0254,
                                 between(HEIGHT, 300, 311) ~ (36 + HEIGHT - 300) * 0.0254,
                                 between(HEIGHT, 400, 411) ~ (48 + HEIGHT - 400) * 0.0254,
                                 between(HEIGHT, 500, 511) ~ (60 + HEIGHT - 500) * 0.0254,
                                 between(HEIGHT, 600, 611) ~ (72 + HEIGHT - 600) * 0.0254,
                                 between(HEIGHT, 700, 711) ~ (84 + HEIGHT - 700) * 0.0254,
                                 between(HEIGHT, 9000, 9242) ~ (HEIGHT - 9000) / 100),
           weight = str_pad(WEIGHT, width = 4, side = "left", pad = "0"),
           measurement_type = case_when(substr(weight, 1, 1) == '0' ~ "imperial",
                                        substr(weight, 1, 1) == '9' ~ "metric",
                                        TRUE ~ "unknown"),
           wtkg = case_when(measurement_type == "imperial" & !as.numeric(substr(weight, 2, 4)) %in% c(777, 999) ~
                              as.numeric(substr(weight, 2, 4)) / 2.2,
                            measurement_type == "metric" & as.numeric(substr(weight, 2, 4)) != 999 ~
                              as.numeric(substr(weight, 2, 4))),
           bmi = pmin(wtkg/(htm ** 2), wtkg/(htm ** 2), 99.98) %>%
             round(2)
    )
  }

  Data <- select(Data, -weight, -htm, -measurement_type, -wtkg) %>%
    mutate(smoker = case_when(SMOKE100 == 1 & SMOKEDAY %in% 1:2 ~ 'current',
                              SMOKE100 == 1 & SMOKEDAY == 3 ~ 'former',
                              SMOKE100 == 2 ~ 'never'))

  Data <- if(!year %in% c(2006, 2008, 2010, 2012, 2014, 2016)){
    mutate(Data, cholesterol = case_when(BLOODCHO == 1 & TOLDHI == 1 ~ 'yes',
                                         BLOODCHO == 1 & TOLDHI == 2 ~ 'no'))
  } else{
    mutate(Data, cholesterol = NA, BPHIGH = NA)
  }

  if(year < 2011){
    Data <- mutate(Data, CHCSCNCR = NA, CHCOCNCR = NA)
  }

  # Data <- group_by(Data, geoid) %>% #change to numerator denominator function
  #   mutate(divisor = case_when(year >= 2011 ~ if_else(bmi >= 12 & bmi <= 100, wt, 0),
  #                              TRUE ~ if_else(bmi <= 100, wt, 0))) %>%
  #   summarise(asthma = sum(if_else(ASTHMA == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(ASTHMA %in% 1:2, wt, 0), na.rm = TRUE),
  #             cancer_skin = sum(if_else(CHCSCNCR == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(CHCSCNCR %in% 1:2, wt, 0), na.rm = TRUE),
  #             cancer_other = sum(if_else(CHCOCNCR == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(CHCOCNCR %in% 1:2, wt, 0), na.rm = TRUE),
  #             cholesterol = sum(if_else(cholesterol == 'yes', wt, 0), na.rm = TRUE) /
  #               sum(if_else(cholesterol %in% c('yes', 'no'), wt, 0), na.rm = TRUE),
  #             diabetes = sum(if_else(DIABETES == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(DIABETES %in% 1:4, wt, 0), na.rm = TRUE),
  #             exercise = sum(if_else(EXERANY == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(EXERANY %in% 1:2, wt, 0), na.rm = TRUE),
  #             general_health = sum(if_else(GENHLTH %in% 1:3, wt, 0), na.rm = TRUE) /
  #               sum(if_else(GENHLTH %in% 1:5, wt, 0), na.rm = TRUE),
  #             heart_disease = sum(if_else(CVDCRHD == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(CVDCRHD %in% 1:2, wt, 0), na.rm = TRUE),
  #             bphigh = sum(if_else(BPHIGH == 1, wt, 0), na.rm = TRUE) /
  #               sum(if_else(BPHIGH %in% 1:4, wt, 0), na.rm = TRUE),
  #             obese = sum(if_else(bmi >= 30 & bmi <= 100, wt, 0), na.rm = TRUE) /
  #               sum(divisor, na.rm = TRUE),
  #             overweight = sum(if_else(bmi >= 25 & bmi < 30, wt, 0), na.rm = TRUE) /
  #               sum(divisor, na.rm = TRUE),
  #             obese_n = sum(if_else(bmi >= 30 & bmi < 100, 1, 0), na.rm = TRUE),
  #             overweight_n = sum(if_else(bmi >= 25 & bmi < 30, 1, 0), na.rm = TRUE),
  #             normal_n = sum(if_else(bmi >= 18.5 & bmi < 25, 1, 0), na.rm = TRUE),
  #             underweight_n = sum(if_else(bmi >= 12 & bmi < 18.5, 1, 0), na.rm = TRUE),
  #             smoker = sum(if_else(smoker == 'current', wt, 0), na.rm = TRUE) /
  #               sum(if_else(smoker %in% c('current', 'former', 'never'),
  #                           wt, 0), na.rm = TRUE)) %>%
  #   gather(variable, value, asthma:smoker)

  return(Data)
}

Data <- mutate(Data, data = map2(data, year, newbie))

Dog <- unnest(Data) %>%
  data.frame()

table(Dog$from_division)

filter(Dog, from_division == TRUE) %>%
  group_by(geoid) %>%
  summarise(n = n())
