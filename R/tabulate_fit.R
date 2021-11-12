#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_death
tabulate_fit <- function(fit, data, exposures) {


 recode_variable <- c(
  gxt_duration = "GXT duration, Y20",
  gxt_duration_int = "GXT duration at baseline, minutes",
  gxt_duration_slp = "Change in GXT duration, minutes per year",
  gxt_int_cat = "GXT duration, Y0",
  gxt_pch_cat = "20-year % reduction in GXT duration",
  CENTER = "CARDIA center",
  race = "White",
  sex = "Male",
  educ = "HS/GED or less",
  exam_age = "Age, per 5 years",
  bmi = "Body mass index at Y20, per 5 units",
  pa_self = "Self reported physical activity at Y20, per 100 units",
  health_self = "Fair or poor self rated health at Y20",
  smoke = "Never smoked at Y20",
  duration_educ_hs_ged_or_less = "Duration with HS/GED or less, per 10 years",
  duration_paying_for_basics_somewhat_hard = "Duration with paying for basics somewhat hard, per 10 years",
  duration_health_self_fair_poor = "Duration with self-rated health fair or poor, per 10 years",
  duration_alcohol_alc_yes = "Duration with alcohol use, per 10 years",
  duration_smoke_current = "Duration with smoking, per 10 years",
  duration_smoke_former = "Duration as former smoker, per 10 years",
  duration_bmi_cat_over_or_obese = "Duration with overweight/obesity, per 10 years",
  duration_pa_self_cat_not_meeting_guidelines = "Duration not meeting physical activity guidelines, per 10 years"
 )

 recode_level <- c(
  gteq_12m = "gteq 12 minutes",
  gteq_8m_lt_12m = "8 to 12 minutes",
  lt_8m = "lt 8 minutes",
  lt_25 = "lt 25% decline",
  gteq_25_lt_35 = "25 to 35% decline",
  gteq_35 = "gteq 35% decline"
 )

 estimates <- map(fit$fits, testEstimates)

 estimate_tbl <- map_dfr(
  .x = estimates,
  .f = ~ {

   .ci <- confint(.x) |>
    as_tibble(rownames = 'variable')

   .est <- as_tibble(.x$estimates)

   ref_cats <- data |>
    select(all_of(exposures)) |>
    map_chr(~levels(.x)[1]) |>
    enframe(name = 'variable', value = 'level') |>
    mutate(level = str_remove(level, '\\.\\.'))

   level_order <- data |>
    select(all_of(exposures)) |>
    map(levels) |>
    reduce(.f = c) |>
    str_remove('\\.\\.')

   .data_tbl <- tibble(
    var = .ci$variable,
    est = .est$Estimate,
    lwr = .ci$`2.5 %`,
    upr = .ci$`97.5 %`
   ) |>
    separate(var,
             into = c("variable", "level"),
             sep = "\\.\\.",
             fill = "right") |>
    filter(variable %in% exposures) |>
    bind_rows(ref_cats) |>
    mutate(level = factor(level, levels = level_order)) |>
    arrange(level) |>
    mutate(level = as.character(level))

   .data_tbl |>
    mutate(
     across(.cols = c(est, lwr, upr), exp),
     variable = recode(variable, !!!recode_variable),
     level = recode(level, !!!recode_level)
    )
  },
  .id = 'model'
 )

 pval_tbl <- fit$pvals |>
  pivot_longer(cols = c(m1,m2,m3),
               names_to = 'model',
               values_to = 'pval') |>
  mutate(variable = recode(variable, !!!recode_variable)) |>
  pivot_wider(names_from = interaction,
              names_prefix = 'pval_',
              values_from = pval)

 left_join(estimate_tbl, pval_tbl)

}
