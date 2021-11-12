#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_gxt_all
gxt_impute <- function(data_gxt_all, IDs_included, n_impute = 2, n_iter = 2) {

  str_dots <- function(x){

    x_obs <- !is.na(x)
    x[x_obs] <- paste('..', x[x_obs], sep = '')
    x

  }

  data_to_impute <- data_gxt_all |>
    select(-starts_with("gxt_valid"))

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
    y = c("ID",
          "exam_age..B",
          "exam_age..C",
          "exam_age..D",
          "exam_age..E",
          "exam_age..F",
          "exam_age..G")
  )

  for(v in seq_along(vars)) vars[[v]] <- setdiff(preds, names(vars)[v])

  data_imputed <- data_to_impute |>
    miceRanger(m = n_impute,
               maxiter = n_iter,
               meanMatchCandidates = 10,
               vars = vars,
               num.trees = 50)

  completeData(data_imputed) %>%
    map(
      ~as_tibble(.x) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = -c(ID, CENTER, race, sex)) %>%
        separate(name, into = c('variable', 'exam'), sep = '\\.\\.') %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(
          across(
            .cols = c(exam_age,
                      gxt_duration,
                      gxt_hr_s2,
                      gxt_pe_max,
                      gxt_hr_max,
                      hr_max_predicted,
                      bmi,
                      pa_self),
            .fns = as.numeric
          )
        ) %>%
        group_by(ID) %>%
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
        ) %>%
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
        ) %>%
        group_by(ID) %>%
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
        ) %>%
        initialize_durations('paying_for_basics') %>%
        initialize_durations('bmi_cat') %>%
        initialize_durations('pa_self_cat') %>%
        initialize_durations('alcohol') %>%
        initialize_durations('smoke') %>%
        initialize_durations('health_self') %>%
        initialize_durations('educ') %>%
        ungroup() %>%
        mutate(
          smoke = factor(
            smoke,
            levels = c('never', 'former','current'),
            labels = c('never', 'former_current', 'former_current')
          )
        ) %>%
        filter(ID %in% IDs_included) %>%
        mutate(across(where(is.factor), as.character),
               across(where(is.character), str_dots),
               across(where(is.character), as.factor)) %>%
        group_by(ID) %>%
        mutate(
          across(
            matches("^duration_"),
            ~ c(0, cumsum(.x[-length(.x)])
            )
          )
        )
    )


}
