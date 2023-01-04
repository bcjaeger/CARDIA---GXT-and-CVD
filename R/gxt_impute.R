#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_gxt_all
gxt_impute <- function(data_gxt_all,
                       data_cvd_all,
                       n_impute = 2,
                       n_iter = 2) {

 str_dots <- function(x){

  x_obs <- !is.na(x)
  x[x_obs] <- paste('..', x[x_obs], sep = '')
  x

 }

 data_to_impute <- data_gxt_all |>
  select(-starts_with("gxt_valid")) |>
  left_join(data_cvd_all) |>
  select(-cod_noncvd, -cod_cvd,
         -contains("_chd_"),
         -contains("_stroke_")) |>
  mutate(cod = replace(cod, is.na(cod), 'alive'))

 # data_to_impute$haz_death_y20 <- data_to_impute %>%
 #  mice::nelsonaalen(timevar = 'time_death_y20',
 #                    statusvar = 'status_death')
 #
 # data_to_impute$haz_cvd_any_y20 <- data_to_impute %>%
 #  mice::nelsonaalen(timevar = 'time_cvd_any_y20',
 #                    statusvar = 'status_cvd_any')

 vars <- vector(mode = 'list', length = ncol(data_to_impute))
 names(vars) <- names(data_to_impute)

 vars$ID <- NULL
 vars$exam_age..B <- NULL
 vars$exam_age..C <- NULL
 vars$exam_age..D <- NULL
 vars$exam_age..E <- NULL
 vars$exam_age..F <- NULL
 vars$exam_age..G <- NULL

 preds <- setdiff(
  x = names(data_to_impute),
  # don't use these as predictors
  y = c("ID",
        "exam_age..B",
        "exam_age..C",
        "exam_age..D",
        "exam_age..E",
        "exam_age..F",
        "exam_age..G")
 )

 for(v in seq_along(vars)) vars[[v]] <- setdiff(preds, names(vars)[v])

 set.seed(329)

 data_imputed <- data_to_impute |>
  miceRanger(m = n_impute,
             maxiter = n_iter,
             meanMatchCandidates = 10,
             vars = vars,
             num.trees = 500)

 # the only difference between post-processing for original data versus
 # imputed data is that na.locf is applied to the imputed data.

 data_to_impute_output <- as_tibble(data_to_impute) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = -c(ID, CENTER, race, sex, cod,
                         starts_with('time'),
                         starts_with('status'))) |>
  separate(name, into = c('variable', 'exam'), sep = '\\.\\.') |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
   across(
    .cols = c(exam_age,
              gxt_duration,
              gxt_hr_s2,
              gxt_pe_max,
              gxt_hr_max,
              hr_max_predicted,
              bmi,
              pa_self,
              starts_with('time'),
              starts_with('status')),
    .fns = as.numeric
   )
  ) |>
  group_by(ID) |>
  mutate(
   bmi_cat = if_else(
    bmi < 25,
    "under_or_normal",
    "over_or_obese"
   ),
   # physical activity
   pa_self_cat = if_else(
    pa_self < 300,
    'not_meeting_guidelines',
    'meeting_guidelines'
   ),
   smoke = as.character(smoke),
   smoke = correct_smoke(x=smoke)
  ) |>
  mutate(
   exam_year = make_numeric_exam(exam),
   exam_period = recode(
    exam,
    "A" = "A",
    "B" = "A",
    "C" = "A",
    "D" = "D",
    "E" = "D",
    "F" = "D",
    "G" = "G"
   ),
   age_centered = exam_age - 20,
   .after = exam
  ) |>
  group_by(ID) |>
  mutate(
   bmi_cat = if_else(
    bmi < 25,
    "under_or_normal",
    "over_or_obese"
   ),
   # physical activity
   pa_self_cat = if_else(
    pa_self < 300,
    'not_meeting_guidelines',
    'meeting_guidelines'
   ),
   smoke = as.character(smoke),
   smoke = correct_smoke(x=smoke),
   exam_age = if_else(is.na(exam_age),
                      exam_age[1] + exam_year,
                      exam_age),
   time_diff = lead(exam_year) - exam_year,
   time_diff = replace(time_diff, exam_year == 20, 0),
  ) |>
  initialize_durations('paying_for_basics') |>
  initialize_durations('bmi_cat') |>
  initialize_durations('pa_self_cat') |>
  initialize_durations('alcohol') |>
  initialize_durations('smoke') |>
  initialize_durations('health_self') |>
  initialize_durations('educ') |>
  ungroup() |>
  mutate(
   smoke = factor(
    smoke,
    levels = c('never', 'former','current'),
    labels = c('never', 'former_current', 'former_current')
   )
  ) |>
  mutate(across(where(is.factor), as.character),
         across(where(is.character), str_dots),
         across(where(is.character), as.factor)) |>
  group_by(ID) |>
  mutate(
   across(
    matches("^duration_"),
    ~ c(0, cumsum(.x[-length(.x)])
    )
   )
  )

 data_imputed_output <-
  completeData(data_imputed) |>
  map(
   ~as_tibble(.x) |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(cols = -c(ID, CENTER, race, sex, cod,
                           starts_with('time'),
                           starts_with('status'))) |>
    separate(name, into = c('variable', 'exam'), sep = '\\.\\.') |>
    pivot_wider(names_from = variable, values_from = value) |>
    mutate(
     across(
      .cols = c(exam_age,
                gxt_duration,
                gxt_hr_s2,
                gxt_pe_max,
                gxt_hr_max,
                hr_max_predicted,
                bmi,
                pa_self,
                starts_with('time'),
                starts_with('status')),
      .fns = as.numeric
     )
    ) |>
    group_by(ID) |>
    mutate(
     across(everything(), zoo::na.locf),
     bmi_cat = if_else(
      bmi < 25,
      "under_or_normal",
      "over_or_obese"
     ),
     # physical activity
     pa_self_cat = if_else(
      pa_self < 300,
      'not_meeting_guidelines',
      'meeting_guidelines'
     ),
     smoke = as.character(smoke),
     smoke = correct_smoke(x=smoke)
    ) |>
    mutate(
     exam_year = make_numeric_exam(exam),
     exam_period = recode(
      exam,
      "A" = "A",
      "B" = "A",
      "C" = "A",
      "D" = "D",
      "E" = "D",
      "F" = "D",
      "G" = "G"
     ),
     age_centered = exam_age - 20,
     .after = exam
    ) |>
    group_by(ID) |>
    mutate(
     bmi_cat = if_else(
      bmi < 25,
      "under_or_normal",
      "over_or_obese"
     ),
     # physical activity
     pa_self_cat = if_else(
      pa_self < 300,
      'not_meeting_guidelines',
      'meeting_guidelines'
     ),
     smoke = as.character(smoke),
     smoke = correct_smoke(x=smoke),
     exam_age = if_else(is.na(exam_age),
                        exam_age[1] + exam_year,
                        exam_age),
     time_diff = lead(exam_year) - exam_year,
     time_diff = replace(time_diff, exam_year == 20, 0),
    ) |>
    initialize_durations('paying_for_basics') |>
    initialize_durations('bmi_cat') |>
    initialize_durations('pa_self_cat') |>
    initialize_durations('alcohol') |>
    initialize_durations('smoke') |>
    initialize_durations('health_self') |>
    initialize_durations('educ') |>
    ungroup() |>
    mutate(
     smoke = factor(
      smoke,
      levels = c('never', 'former','current'),
      labels = c('never', 'former_current', 'former_current')
     )
    ) |>
    mutate(across(where(is.factor), as.character),
           across(where(is.character), str_dots),
           across(where(is.character), as.factor)) |>
    group_by(ID) |>
    mutate(
     across(
      matches("^duration_"),
      ~ c(0, cumsum(.x[-length(.x)])
      )
     )
    )
  )


 # some MI functions need the original data as well as imputed data.
 c(list(Dataset_0 = data_to_impute_output), data_imputed_output)

}
