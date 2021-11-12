#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
#' @param status
#' @param time
cph_fit <- function(data_analysis,
                    exposures,
                    status,
                    time) {

  data_fit <- map(
   .x = data_analysis,
   ~ .x |>
     rename(status = !!status,  time = !!time) |>
     ungroup() |>
     mutate(gxt_duration = gxt_duration / 60,
            gxt_duration_int = gxt_duration_int / 60,
            exam_age = exam_age / 5,
            bmi = bmi / 5,
            pa_self = pa_self / 100,
            across(starts_with("duration"), ~.x / 10))
  )

  time_scale <- 1 / 365

  map(
   .x = data_fit,
   .f = ~ {
    total_events <- sum(getElement(.x, 'status'))
    total_time <- sum(getElement(.x, 'time')) * time_scale

    estimate = total_events / total_time

    fit <- glm(total_events ~ offset(log(total_time)), family="poisson")
   }
  )

  c1 <- c('CENTER', 'race', 'sex', 'educ', 'exam_age')

  c2 <- c(c1, 'bmi', 'pa_self', 'health_self', 'smoke')

  c3 <- c(c1,
          "duration_educ_hs_ged_or_less",
          "duration_paying_for_basics_somewhat_hard",
          "duration_health_self_fair_poor",
          "duration_alcohol_alc_yes",
          "duration_smoke_current",
          "duration_smoke_former",
          "duration_bmi_cat_over_or_obese",
          "duration_pa_self_cat_not_meeting_guidelines")

  formulas <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposures, .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int1_race <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposures, paste0(exposures[1], ":race"), .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int2_race <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposures, paste0(exposures[2], ":race"), .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int1_sex <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposures, paste0(exposures[1], ":sex"), .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int2_sex <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposures, paste0(exposures[2], ":sex"), .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  as_mitml_result <- function(x){
   class(x) <- c("mitml.result", class(x))
   x
  }

  fits <- map(
   .x = formulas,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  fits_int1_race <- map(
   .x = formulas_int1_race,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  fits_int2_race <- map(
   .x = formulas_int2_race,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  fits_int1_sex <- map(
   .x = formulas_int1_sex,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  fits_int2_sex <- map(
   .x = formulas_int2_sex,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  anova_pval <- function(.x, .y, method = 'D2'){
    anova(.x, .y, method=method)$test[[1]]$test[, 'P(>F)']
  }

  pvals <- bind_rows(
   race = map2_dfc(fits, fits_int1_race, anova_pval),
   race = map2_dfc(fits, fits_int2_race, anova_pval),
   sex  = map2_dfc(fits, fits_int1_sex, anova_pval),
   sex  = map2_dfc(fits, fits_int2_sex, anova_pval),
   .id = 'interaction'
  ) |>
   mutate(variable = rep(exposures, 2),
          .before = 1)

  list(
   fits = fits,
   pvals = pvals
  )


}
