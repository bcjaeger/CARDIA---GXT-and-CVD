#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
#' @param status
#' @param time
#'

as_mitml_result <- function(x){
 class(x) <- c("mitml.result", class(x))
 x
}

cph_fit <- function(data_analysis,
                    subset = 'overall',
                    exposure,
                    status,
                    time,
                    pass = 1) {


 if(subset != 'overall' && pass == 1){

  lvls <- levels(data_analysis[[1]][[subset]])
  output <- vector(mode = 'list', length = length(lvls))
  names(output) <- lvls

  for(lvl in lvls){

   print(lvl)

   data_analysis_sub <- data_analysis

   for(i in seq_along(data_analysis_sub)){

    rows_keep <- data_analysis_sub[[i]][[subset]] == lvl
    data_analysis_sub[[i]] <- data_analysis_sub[[i]][rows_keep, ]

   }

   output[[lvl]] <- cph_fit(data_analysis = data_analysis_sub,
                            subset = subset,
                            exposure = exposure,
                            status = status,
                            time = time,
                            pass = 2)

  }

  return(output)

 }

  data_fit <- map(
   .x = data_analysis[-1],
   ~ .x |>
     rename(status = !!status,  time = !!time) |>
     ungroup() |>
     mutate(gxt_duration = gxt_duration / 60, # per minute
            gxt_duration_int = gxt_duration_int / 60,
            gxt_duration_pch = (gxt_duration_pch * 100) / 5, # per 5%
            exam_age = exam_age / 5,
            bmi = bmi / 5,
            pa_self = pa_self / 100,
            across(starts_with("duration"), ~.x / 10),
            time = time / 365.25)
  )

  take_out <- subset

  if(subset == 'race_sex') take_out <- c('race', 'sex')

  c1 <- c('CENTER', 'race', 'sex', 'educ', 'exam_age')

  if(exposure == 'gxt_duration_pch') c1 <- c(c1, 'gxt_duration_int')

  c1 <- setdiff(c1, take_out)

  c2 <- c(c1,
          'bmi',
          'pa_self',
          'health_self',
          'smoke')

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
   .f = ~ paste(c(exposure, .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int1_race <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(paste(c(exposure, .x), "* race"), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_int1_sex <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(paste(c(exposure, .x), "* sex"), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  fits <- map(
   .x = formulas,
   .f = function(formula){
    as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
   }
  )

  pvals <- list()

  if(subset != 'race' && subset != 'race_sex'){


   fits_int1_race <- map(
    .x = formulas_int1_race,
    .f = function(formula){
     as_mitml_result(map(.x = data_fit, .f = ~ coxph(formula, data = .x)))
    }
   )

   formulas_null <- map(
    formulas_int1_race,
    ~update(.x, as.formula(paste0(". ~ . -", exposure, ":", "race")))
   )

   fits_null <- map(
    .x = formulas_null,
    .f = function(formula){
     as_mitml_result(map(.x = data_fit,
                         .f = ~ coxph(formula, data = .x)))
    }
   )

   pvals$race <- map2_dfc(
    .x = fits_int1_race,
    .y = fits_null,
    .f = ~ testModels(model = .x,
                      null.model = .y,
                      method = 'D2',
                      use = 'likelihood') |>
     getElement('test') |>
     as.data.frame() |>
     pull(`P(>F)`)
   )

  }

  if(subset != 'sex' && subset != 'race_sex'){

   # if(exposure == 'gxt_duration_int' && status == 'status_death' &&
   #    data_analysis$Dataset_1$race[1] == '..Black') browser()

   fits_int1_sex <- map(
    .x = formulas_int1_sex,
    .f = function(formula){
     as_mitml_result(map(.x = data_fit,
                         .f = ~ coxph(formula, data = .x)))
    }
   )

   formulas_null <- map(
    formulas_int1_sex,
    ~update(.x, as.formula(paste0(". ~ . -", exposure, ":", "sex")))
   )

   fits_null <- map(
    .x = formulas_null,
    .f = function(formula){
     as_mitml_result(map(.x = data_fit,
                         .f = ~ coxph(formula, data = .x)))
    }
   )

   pvals$sex <- map2_dfc(
    .x = fits_int1_sex,
    .y = fits_null,
    .f = ~ testModels(model = .x,
                      null.model = .y,
                      method = 'D2',
                      use = 'wald') |>
     getElement('test') |>
     as.data.frame() |>
     pull(`P(>F)`)
   )

  }

  pvals <- bind_rows(pvals, .id = 'interaction') |>
   mutate(variable = exposure, .before = 1)

  # if(any(pvals$m3 > 0.98)) browser()

  list(
   fits = fits,
   pvals = pvals
  )


}
