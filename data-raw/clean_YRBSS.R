# Cleaning the YRBSS 
# Catalina Canizares
# 05-11-2023


# The following script aims to generate a clean dataset that will focus on risk 
# behaviors related to alcohol and drug use, as well as smoking. Additionally, 
# variables related to reckless behaviors will also be included

library(tidyverse)

load("inst/extData/raw_yrbs_2019.rda")

raw_yrbss <- tidyREDCap::drop_labels(raw_yrbs_2019)
# 13677x235 
### This function is created to recode binary factors from 1 and 2 to 0 and 1.
RecodeBinary <- function(x) {
  x <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA
  )
}


riskyBehaviors <-
  raw_yrbss |>
  select(
    Q1:Q3, "raceeth", Q8:Q21, Q23, Q24, Q30:Q35, Q37,
    Q38, Q40:Q48, Q50:Q56, Q66
  ) |>
  mutate(
    Sex = case_when(
      Q2 == 2 ~ "Male",
      Q2 == 1 ~ "Female",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Race = case_when(
      raceeth == 1 ~ "Am Indian/Alaska Native",
      raceeth == 2 ~ "Asian",
      raceeth == 3 ~ "Black or African American",
      raceeth == 4 ~ "Native Hawaiian/Other PI",
      raceeth == 5 ~ "White",
      raceeth == 6 ~ "Hispanic/Latino",
      raceeth == 7 ~ "Multiple-Hispanic",
      raceeth == 8 ~ "Multiple-Non-Hispanic",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Age = case_when(
      Q1 == 1 ~ 12L,
      Q1 == 2 ~ 13L,
      Q1 == 3 ~ 14L,
      Q1 == 4 ~ 15L,
      Q1 == 5 ~ 16L,
      Q1 == 6 ~ 17L,
      Q1 == 7 ~ 18L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    Grade = case_when(
      Q3 == 1 ~ "9",
      Q3 == 2 ~ "10",
      Q3 == 3 ~ "11",
      Q3 == 4 ~ "12",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    SexOrientation = case_when(
      Q66 == 1 ~ "Heterosexual",
      Q66 == 2 ~ "Gay or Lesbian",
      Q66 == 3 ~ "Bisexual",
      Q66 == 4 ~ "Not sure",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    across(c(Q19, Q23, Q24, Q30, Q34), RecodeBinary)
  ) |>
  mutate(
    across(c(Q19, Q23, Q24, Q30, Q34), as.factor)
  ) |>
  mutate(Q8 = case_when(
    Q8 == 1 ~ "Never", 
    Q8 == 2 ~ "Rarely", 
    Q8 == 3 ~ "Sometimes", 
    Q9 == 4 ~ "Most of the Times", 
    Q10 == 5 ~ "Always", 
    TRUE ~ NA_character_
  )) |> 
  mutate(Q44 = as.character(Q44)) |> 
  mutate(MarihuanaUse = case_when(
    Q45 == 1 ~ 0, 
    Q45 %in% c(2, 3, 4, 5, 6, 7) ~ 1, 
    TRUE ~ NA
  )) |> 
  mutate(MarihuanaUse = as.factor(MarihuanaUse)) |> 
  select(-c(Q2, Q1, Q3, Q66, Q66, raceeth)) |>
  rename(
    SeatBealtUse = Q8,
    DrinkingDriver = Q9,
    DrivingDrinking = Q10,
    TextingDriving = Q11,
    WeaponCarrying = Q12,
    WeaponCarryingSchool = Q13,
    GunCarrying = Q14,
    UnsafeAtSchool = Q15,
    InjuredInSchool = Q16,
    PhysicalFight = Q17,
    SchoolPhysicalFight = Q18,
    SexualAbuse = Q19,
    TimesSexualAbuse = Q20,
    SexualAbuseByPartner = Q21,
    Bullying = Q23,
    CyberBullying = Q24,
    SmokingCigarette = Q30,
    AgeFirstCig = Q31,
    DaysSmokingCigarette = Q32,
    CigPerDay = Q33,
    Vaping = Q34,
    DaysVaping = Q35,
    DaysSmokelessTobacco = Q37,
    DaysSmokingCigar = Q38,
    AgeFirstAlcohol = Q40,
    DaysAlcohol = Q41,
    BingeDrinking = Q42,
    LargestNumberOfDrinks = Q43,
    SourceAlcohol = Q44,
    TimesMarihuanaUseLife = Q45,
    AgeFirstMarihuana = Q46,
    TimesMarihuanaUse30Days = Q47,
    TimessSyntethicMarihuanaUseLife = Q48,
    TimesCocaine = Q50,
    TimesInhalant = Q51,
    TimesHeroin = Q52,
    TimesMetha = Q53,
    TimesEcstasy = Q54,
    TimesSteroids = Q55,
    TimesNeedle = Q56
  ) |>
  select(Sex, Race, Age, Grade, SexOrientation, everything())


usethis::use_data(riskyBehaviors, overwrite = TRUE)
