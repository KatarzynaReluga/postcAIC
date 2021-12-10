library(dplyr)
library(mosaic)
library(NHANES)
library(readxl)
library(usethis)


# Source: R package NHAENS
postcAIC_nhaens0 <-
  NHANES %>%
  filter(SurveyYr == "2011_12", Age >= 20) %>%
  select(
    Poverty,
    SleepHrsNight,
    Gender,
    Age,
    Race1,
    BMI,
    BPSys2,
    DirectChol,
    Diabetes,
    PhysActive,
    SmokeNow,
    Smoke100
  ) %>%
  mutate(
    CurrentSmokingStatus = derivedFactor(
      Yes = SmokeNow == "Yes",
      No = (SmokeNow == "No" | Smoke100 == "No")
    ),
    clusterID = derivedFactor(
      Age20_f1  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Black"),
      Age20_f2  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age20_f3  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Mexican"),
      Age20_f4  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Other"),
      Age20_f5  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "White"),

      Age20_m1  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Black"),
      Age20_m2  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age20_m3  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Mexican"),
      Age20_m4  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Other"),
      Age20_m5  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "White"),

      Age30_f1  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Black"),
      Age30_f2  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age30_f3  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Mexican"),
      Age30_f4  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Other"),
      Age30_f5  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "White"),

      Age30_m1  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Black"),
      Age30_m2  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age30_m3  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Mexican"),
      Age30_m4  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Other"),
      Age30_m5  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "White"),

      Age40_f1  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Black"),
      Age40_f2  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age40_f3  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Mexican"),
      Age40_f4  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Other"),
      Age40_f5  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "White"),

      Age40_m1  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Black"),
      Age40_m2  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age40_m3  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Mexican"),
      Age40_m4  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Other"),
      Age40_m5  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "White"),

      Age50_f1  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Black"),
      Age50_f2  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age50_f3  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Mexican"),
      Age50_f4  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Other"),
      Age50_f5  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "White"),

      Age50_m1  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Black"),
      Age50_m2  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age50_m3  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Mexican"),
      Age50_m4  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Other"),
      Age50_m5  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "White"),


      Age60_f1  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Black"),
      Age60_f2  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age60_f3  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Mexican"),
      Age60_f4  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Other"),
      Age60_f5  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "White"),
      #      : Mexican, Hispanic, White, Black, or Oth
      Age60_m1  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Black"),
      Age60_m2  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age60_m3  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Mexican"),
      Age60_m4  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Other"),
      Age60_m5  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "White"),

      Age70_f1  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Black"),
      Age70_f2  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age70_f3  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Mexican"),
      Age70_f4  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Other"),
      Age70_f5  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "White"),

      Age70_m1  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Black"),
      Age70_m2  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age70_m3  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Mexican"),
      Age70_m4  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Other"),
      Age70_m5  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "White")
    ), log_BMI = log(BMI),
  ) %>%
  select(c("PhysActive", "CurrentSmokingStatus", "SleepHrsNight",
           "BPSys2", "DirectChol", "Poverty",
           "Diabetes", "clusterID", "log_BMI"))


postcAIC_nhaens <- na.omit(postcAIC_nhaens0)

write_csv(postcAIC_nhaens, "data-raw/postcAIC_nhaens.csv")
usethis::use_data(postcAIC_nhaens, overwrite = TRUE)
