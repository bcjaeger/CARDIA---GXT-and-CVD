#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

recoders_make <- function() {

 list(
  variable = c(
   gxt_duration = "At Y20, per minute",
   gxt_duration_int = "At Y0, per minute",
   gxt_duration_pch = "Retained at Y20, per 10%",
   gxt_duration_slp = "Change in GXT duration, minutes per year",
   gxt_int_cat = "Y0 Fitness category",
   gxt_pch_cat = "Y20 Fitness category"
  ),
  group = c(
   race = 'Race',
   sex = 'Sex',
   race_sex = 'Race-Sex'
  ),
  levels = c(
   high = "High",
   middle = "Medium",
   low = "Low",
   lt_25 = "< 25% decline",
   gteq_25_lt_35 = "\u2265 25% to < 35% decline",
   gteq_35 = "\u2265 35% decline",
   CENTER = "CARDIA center",
   race = "White",
   sex = "Male",
   educ = "HS/GED or less",
   exam_age = "Age, per 5 years",
   bmi = "Body mass index at Y20, per 5 units",
   pa_self = "Self reported physical activity at Y20, per 100 units",
   health_self = "Fair or poor self rated health at Y20",
   smoke = "Never smoked at Y20"
  )
 )


 #  duration_educ_hs_ged_or_less =
 #   "Duration with HS/GED or less, per 10 years",
 #  duration_paying_for_basics_somewhat_hard =
 #   "Duration with paying for basics somewhat hard, per 10 years",
 #  duration_health_self_fair_poor =
 #   "Duration with self-rated health fair or poor, per 10 years",
 #  duration_alcohol_alc_yes =
 #   "Duration with alcohol use, per 10 years",
 #  duration_smoke_current =
 #   "Duration with smoking, per 10 years",
 #  duration_smoke_former =
 #   "Duration as former smoker, per 10 years",
 #  duration_bmi_cat_over_or_obese =
 #   "Duration with overweight/obesity, per 10 years",
 #  duration_pa_self_cat_not_meeting_guidelines =
 #   "Duration not meeting physical activity guidelines, per 10 years"

}
