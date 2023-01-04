#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
#' @param subset
#' @param exposure
#' @param time
#' @param status
cph_fit_ns <- function(data_analysis,
                       subset,
                       exposure,
                       time,
                       status,
                       x_ref,
                       pass = 1) {


 if(subset != 'overall' && pass == 1){

   lvls <- levels(data_analysis[[1]][[subset]])
   output <- vector(mode = 'list', length = length(lvls))
   names(output) <- lvls

   for(lvl in lvls){

    print(lvl)

    data_analysis_sub <- data_analysis

    for(i in seq_along(data_analysis_sub)){

     # reminder: dataset_0 can have missing values
     # drop them here if needed.
     rows_keep <-
      data_analysis_sub[[i]][[subset]] == lvl &
      !is.na(data_analysis_sub[[i]][[subset]])

     data_analysis_sub[[i]] <- data_analysis_sub[[i]][rows_keep, ]

    }

    output[[lvl]] <- cph_fit_ns(data_analysis = data_analysis_sub,
                                subset = lvl,
                                exposure = exposure,
                                status = status,
                                time = time,
                                pass = 2)

   }

   return(bind_rows(output))

  }


  data_fit <- map(
   .x = data_analysis,
   ~ .x |>
    rename(status = !!status,  time = !!time) |>
    ungroup() |>
    mutate(gxt_duration = gxt_duration / 60, # per minute
           gxt_duration_int = gxt_duration_int / 60,
           gxt_duration_pch = (gxt_duration_pch * 100),
           exam_age = exam_age / 5,
           bmi = bmi / 5,
           pa_self = pa_self / 100,
           across(starts_with("duration"), ~.x / 10),
           time = time / 365.25)
  )

  x_min <- quantile(data_fit$Dataset_0[[exposure]], probs = 0.2)
  x_max <- quantile(data_fit$Dataset_0[[exposure]], probs = 0.8)
  x_ref <- quantile(data_fit$Dataset_0[[exposure]], probs = 0.5)

  # drop the data with missing values
  data_fit <- data_fit[-1]

  take_out <- 'overall'

  if(subset %in% c("..Black", "..White")) take_out <- 'race'

  if(subset %in% c("..Male", "..Female")) take_out <- 'sex'

  if(subset %in% c("..Black_Male",
                   "..Black_Female",
                   "..White_Male",
                   "..White_Female")) take_out <- c('race', 'sex')

  if(subset == 'race_sex') take_out <- c('race', 'sex')

  c1 <- c('CENTER', 'race', 'sex', 'educ', 'exam_age')

  c1 <- setdiff(c1, take_out)

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

  exposure_ns <- paste0("ns(", exposure, ", df = 4)")


  formulas <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposure_ns, .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_linear <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(c(exposure, .x), collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  formulas_null <- map(
   .x = list(m1 = c1, m2 = c2, m3 = c3),
   .f = ~ paste(.x, collapse = ' + ')
  ) |>
   map(~ paste("Surv(time, status) ~", .x)) |>
   map(as.formula)

  n_impute <- length(data_fit)

  bases <- map(
   .x = data_fit,
   .f = ~ ns(.x[[exposure]], df = 4)
  )

  pmap_dfr(
   .l = list(formulas, formulas_linear, formulas_null),
   .id = 'model',
   .f = function(f, f_linear, f_null){

    fits_nonlinear <- map(
     .x = data_fit,
     .f = ~ coxph(formula = f, data = .x)
    )

    fits_linear <- map(
     .x = data_fit,
     .f = ~ coxph(data = .x, formula = f_linear)
    )

    fits_null <- map(
     .x = data_fit,
     .f = ~ coxph(data = .x, formula = f_null)
    )

    pval_nonlinear <- testModels(fits_nonlinear,
                                 null.model = fits_linear,
                                 method = 'D2',
                                 use = 'likelihood') |>
     getElement('test') |>
     as.data.frame() |>
     pull('P(>F)')

    pval_effect <- testModels(fits_nonlinear,
                              null.model = fits_null,
                              method = 'D2',
                              use = 'likelihood') |>
     getElement('test') |>
     as.data.frame() |>
     pull('P(>F)')

    pval <- list(nonlinear = pval_nonlinear,
                 effect = pval_effect)

    # using the fit and basis function,
    # get a predicted spline estimate + SE
    spline_preds <- map2(
     .x = fits_nonlinear,
     .y = bases,
     .f = get_spline_preds,
     pattern = '^ns\\(',
     x_min = x_min,
     x_max = x_max,
     x_ref = x_ref
    )

    # pool results using Rubin's rules

    # V_w = mean of the variance estimates
    variance_within <- map_dfc(spline_preds, "se") %>%
     apply(MARGIN = 1, function(x) mean(x^2))

    # V_b = variance of the predictions
    variance_between <- map_dfc(spline_preds, "pred") %>%
     apply(MARGIN = 1, var)

    # V_total = V_w + V_b * (n_imputes+1) / n_imputes
    variance_total <-
     variance_within + variance_between + variance_between/n_impute

    se_pooled <- sqrt(variance_total)

    data_pval <- as_tibble(pval)

    data_spline <- tibble(
     group = subset,
     x = spline_preds[[1]]$x,
     pred = apply(map_dfc(spline_preds, "pred"), 1, mean),
     se = se_pooled,
     ci_lwr = pred + qnorm(0.025) * se,
     ci_upr = pred + qnorm(0.975) * se
    )

    data_out <- nest(data_spline, data=c(x, pred, se, ci_lwr, ci_upr)) |>
     mutate(exposure = exposure,
            outcome = str_remove(status, '^status_'),
            pval_nonlinear = data_pval$nonlinear,
            pval_effect = data_pval$effect,
            .before = data)

    data_out

   }

  )

 }
