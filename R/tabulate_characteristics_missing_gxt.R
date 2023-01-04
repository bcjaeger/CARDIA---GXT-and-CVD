
tabulate_characteristics_missing_gxt <- function(data_included) {

 data_included$data_gxt %>%
  left_join(data_included$data_cvd) %>%
  select(
   -ID,
   -race,
   -sex,
   -meds_bp,
   -gxt_pe_max,
   -gxt_hr_max,
   -hr_max_predicted,
   -gxt_valid,
   -gxt_hr_s2,
  ) |>
  mutate(`No. of participants` = 1,
         cvd_10yrs = case_when(
          status_cvd_any == 1 & time_cvd_any_y20 < 3652.5 ~ "Yes",
          time_cvd_any_y20 >= 3652.5 ~ "No",
          status_cvd_any == 0 & time_cvd_any_y20 < 3652.5 ~ "Censored"
         ),
         death_10yrs = case_when(
          status_death == 1 & time_death_y20 < 3652.5 ~ "Yes",
          time_death_y20 >= 3652.5 ~ "No"
         ),
         .before = 1) %>%
  select(-starts_with("status"),
         -starts_with("time"),
         -starts_with("cod")) %>%
  filter(exam %in% c("A", "D", "G")) %>%
  group_by(exam) %>%
  nest() %>%
  mutate(
   tables = map(
    data,
    .f = ~ .x %>%
     mutate(
      gxt_duration_miss = factor(
       is.na(gxt_duration),
       levels = c(FALSE, TRUE),
       labels = c("GXT observed", "GXT missing")
      ),
      paying_for_basics = recode(
       paying_for_basics,
       not_very_hard = 'Not very hard',
       somewhat_hard = 'Somewhat hard'
      ),
      alcohol = recode(
       alcohol,
       alc_yes = 'Drinks alcohol',
       alc_no = 'Does not drink alcohol'
      ),
      marital = recode(
       marital,
       married_or_cohabit = 'Married or co-habitating',
       other = 'Other'
      ),
      health_self = recode(
       health_self,
       excellent_good = 'Excellent or good',
       fair_poor = 'Fair or poor'
      ),
      smoke = recode(
       smoke,
       former = 'Former',
       current = 'Current',
       never = 'Never smoked'
      ),
      pa_self = if_else(
       pa_self < 300,
       'Not meeting guidelines',
       'Meeting guidelines',
      ),
      educ = recode(
       educ,
       assoc_or_more = 'Associate\'s or more',
       hs_ged_or_less = 'Highschool/GED or less'
      )
     ) %>%
     select(-gxt_duration) %>%
     tbl_summary(
      by = gxt_duration_miss,
      label = list(
       cvd_10yrs ~ "Incident CVD (Y20 - Y30)",
       death_10yrs ~ "Incident all-cause mortality (Y20 - Y30)",
       CENTER ~ "Testing center",
       exam_age ~ "Age",
       educ ~ "Education at year 20",
       paying_for_basics ~ "Difficulty paying for basics",
       marital ~ "Marital status",
       bmi ~ "Body mass index, kg/m2",
       pa_self ~ "Self-reported physical activity",
       health_self ~ "Self-reported health",
       alcohol ~ "Alcohol use",
       smoke ~ "Smoking status"
      ),
      missing = 'no',
      type = list(`No. of participants` ~ 'continuous'),
      statistic = list(
       all_continuous() ~ "{mean} ({sd})",
       all_categorical() ~ "{p}",
       `No. of participants` ~ "{sum}"
      ),
      digits = list(`No. of participants` ~ 0)
     ) |>
     add_overall(last = FALSE, col_label = '**Overall**') |>
     modify_footnote(update = everything() ~ NA) |>
     add_p() |>
     modify_header(update = all_stat_cols(FALSE) ~ "**{level}**")
   )
  )

}
