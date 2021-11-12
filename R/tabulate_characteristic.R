#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
tabulate_characteristic <- function(data) {


 data |>
  # this will trick tbl_summary into writing
  # the no. of patients in each column of the table.
  mutate(`No. of participants` = 1, .before = 1) |>
  # these will be the column labels
  mutate(
   status = interaction(race, sex, sep = ' '),
   status = paste0(status, 's')
  ) |>
  filter(exam == 'A' | exam == 'G') |>
  group_by(exam) |>
  group_map(
   .f = ~ .x |>
    select(
     -ID,
     -race,
     -sex,
     -meds_bp,
     -gxt_pe_max,
     -gxt_hr_max,
     -hr_max_predicted,
     -gxt_valid,
     -gxt_hr_s2
    ) |>
    mutate(
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
    ) |>
    tbl_summary(
     by = status,
     label = list(
      CENTER ~ "Testing center",
      exam_age ~ "Age",
      gxt_duration ~ "GXT duration, seconds",
      # gxt_hr_s2 ~ "GXT heart rate; stage 2, beats per minute",
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
  ) |>
  set_names(nm = c("Y0", "Y20"))


}
