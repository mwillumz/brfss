library(brfss)

Data <- fetch_brfss(2015)




library(censusapi)
Msa <- getCensus(name="acs5",
          vintage = 2015,
          vars=c("NAME", "B01001_001E"),
          region="metropolitan statistical area/micropolitan statistical area:*",
          key = '0a05d377432e0e7c88dd138d65f79082a5c49eec') %>%
  arrange(desc(B01001_001E))

#Divisions Section
Divisions <- lapply(c("35620", "31080", "16980", "19100", "37980", "47900",
  "33100", "14460", "41860", "19820", "42660"), function(d){
    getCensus(name="acs5",
              vintage = 2015,
              vars=c("NAME"),
              region="metropolitan division:*",
              regionin=paste("metropolitan statistical area/micropolitan statistical area", d, sep = ":"),
              key = '0a05d377432e0e7c88dd138d65f79082a5c49eec')
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

DivisionsAll <- bind_rows(Divisions, DivisionsOld)

filter(Thing, !geoid %in% Msa$metropolitan.statistical.area.micropolitan.statistical.area) %>%
  filter(!geoid %in% DivisionsAll$division) %>%
  filter(!geoid %in% c("26180", "37700", "42260")) %>% # Honolulu, Pascagoula, Sarasota
  filter(!geoid %in% c("10020", "25660", "40500")) %>% #unknown
  filter(!geoid %in% c("30100", "42580", "37380", "37764", "48740")) #Lebanon, Seaford, Palm Coast, Peabody, Willimantic

Temp <- mutate(Data, smoker = case_when(SMOKE100 == 2 ~ 'never',
                                SMOKE100 == 1 & SMOKEDAY %in% 1:2 ~ 'current',
                                SMOKE100 == 1 & SMOKEDAY == 3 ~ 'former')) %>%
  group_by(year, msa) %>%
  summarise(diabetes = sum(if_else(DIABETES == 1, msawt, 0), na.rm = TRUE) /
              sum(if_else(DIABETES %in% 1:4, msawt, 0), na.rm = TRUE),
            smoker = sum(if_else(smoker == 'current', msawt, 0), na.rm = TRUE) /
              sum(if_else(smoker %in% c('current', 'former', 'never'),
                          msawt, 0), na.rm = TRUE))

  mutate(`_BMI5` = `_BMI5` / 100,
         htin4 = case_when(between(HEIGHT3, 300, 311) ~ 36 + HEIGHT3 - 300,
                           between(HEIGHT3, 400, 411) ~ 48 + HEIGHT3 - 400,
                           between(HEIGHT3, 500, 511) ~ 60 + HEIGHT3 - 500,
                           between(HEIGHT3, 600, 611) ~ 72 + HEIGHT3 - 600,
                           between(HEIGHT3, 700, 711) ~ 84 + HEIGHT3 - 700),
         htm4 = case_when(between(HEIGHT3, 300, 711) ~ htin4 * 0.0254,
                          between(HEIGHT3, 9091, 9244) ~ (HEIGHT3 - 9000) / 100),
         wtkg3 = case_when(between(WEIGHT2, 50, 650) ~ WEIGHT2 * 0.4535924,
                           between(WEIGHT2, 9023, 9295) ~ WEIGHT2 - 9000),
         bmi5 = case_when(PREGNANT == 1 ~ NA_real_,
                          TRUE ~ wtkg3/(htm4 ** 2)),
         rfchol = case_when(BLOODCHO == 1 & TOLDHI2 == 2 ~ 'no',
                            BLOODCHO == 1 & TOLDHI2 == 1 ~ 'yes'),
         rfhype5 = case_when(BPHIGH4 == 1 ~ 'yes',
                             BPHIGH4 %in% 2:4 ~ 'no')) %>%

    # rfchol = sum(if_else(rfchol == 'yes', msawt, 0), na.rm = TRUE) /
    #   sum(if_else(rfchol %in% c('yes', 'no'), msawt, 0), na.rm = TRUE),
    # rfhype5 = sum(if_else(rfhype5 == 'yes', msawt, 0), na.rm = TRUE) /
    #   sum(if_else(rfhype5 %in% c('yes', 'no'), msawt, 0), na.rm = TRUE),
    # obese = sum(if_else(bmi5 >= 30 & bmi5 < 100, msawt, 0), na.rm = TRUE) /
    #           sum(if_else(bmi5 >= 12 & bmi5 < 100, msawt, 0), na.rm = TRUE),
    # overweight = sum(if_else(bmi5 >= 25 & bmi5 < 30, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(bmi5 >= 12 & bmi5 < 100, msawt, 0), na.rm = TRUE),
    # chc0cncr = sum(if_else(CHCOCNCR == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(CHCOCNCR %in% 1:2, msawt, 0), na.rm = TRUE),
    # chcscncr = sum(if_else(CHCSCNCR == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(CHCSCNCR %in% 1:2, msawt, 0), na.rm = TRUE),
    # checkup1 = sum(if_else(CHECKUP1 == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(CHECKUP1 %in% c(1:4, 8), msawt, 0), na.rm = TRUE),
    # cvdcrhd4 = sum(if_else(CVDCRHD4 == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(CVDCRHD4 %in% 1:2, msawt, 0), na.rm = TRUE),
    # cvdstrk3 = sum(if_else(CVDSTRK3 == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(CVDSTRK3 %in% 1:2, msawt, 0), na.rm = TRUE),
    # denvst = sum(if_else(LASTDENT == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(LASTDENT %in% c(1:4, 8), msawt, 0), na.rm = TRUE),

    # flshot = sum(if_else(AGE >= 65 & FLUSHOT == 1, msawt, 0), na.rm = TRUE) /
    #   sum(if_else(AGE >= 65 & FLUSHOT %in% 1:2, msawt, 0), na.rm = TRUE)
    )


# ?tibble(year = 2002:2016,
#        variable = c(rep('FLUSHOT', 2), 'FLUSHOT2', rep('FLUSHOT3', 5),
#                     'FLUSHOT4', rep('FLUSHOT5', 2), rep('FLUSHOT6', 4)),
#        recode = first(variable))
#
# tibble(year = 2002:2016,
#        variable = c(rep('LASTDEN2', 3), rep('LASTDEN3', 12)),
#        recode = "LASTDENT")

# transmute(Data, year = year,
#           rfhlth = case_when(GENHLTH %in% 1:3 ~ 1,
#                              GENHLTH %in% 4:5 ~ 2),
#           phys14d = case_when(PHYSHLTH == 88 ~ 1,
#                               between(PHYSHLTH, 1, 13) ~ 2,
#                               between(PHYSHLTH, 14, 31) ~ 3),
#           rfchol = case_when(BLOODCHO == 1 & TOLDHI2 == 2 ~ 1,
#                              BLOODCHO == 1 & TOLDHI2 == 1 ~ 2),
#
#           bmi5cat = case_when(`_BMI5` < 18.5 ~ 1,
#                               `_BMI5` < 25 ~ 2,
#                               `_BMI5` < 30 ~ 3,
#                               `_BMI5` >= 30 ~ 4),
#           flshot6 = case_when(AGE > 64 ~ FLUSHOT6)
# )
