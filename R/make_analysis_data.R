#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
make_analysis_data <- function(data_gxt_imputed,
                               data_excluded) {

 recipes <- map(
  data_gxt_imputed,
  ~ .x |>
   recipe(gxt_duration ~ exam_age + exam + ID) |>
   step_filter(exam %in% c("..A", "..D", "..G")) |>
   step_normalize(exam_age) |>
   prep()
 )

 mdl_data <- map(recipes, juice)

 mdl_fits <- map(
  mdl_data,
  ~ lmer(
   formula = gxt_duration ~ exam_age + (1 + exam_age | ID),
   data = .x
  )
 )

 for(i in names(mdl_data)){
  mdl_data[[i]]$preds <- predict(mdl_fits[[i]])
 }

 cmp_slope <- function(x, y, x_sd, x_mean){

  if(length(x) == 1) return(NA_real_)

  x[c(1, length(x))] <- x[c(1, length(x))] * x_sd + x_mean

  diff_x <- x[length(x)] - x[1]

  diff_y <- y[length(y)] - y[1]

  diff_y / diff_x

 }

 perc_change <- function(x_old, x_new)  (x_new-x_old) / x_old

 gxt_int_slp <- map2(
  .x = mdl_data,
  .y = recipes,
  .f = function(.mdl_data, .recipe){
   .mdl_data |>
    group_by(ID) |>
    summarize(
     gxt_duration_int = preds[1],
     gxt_duration_pch = perc_change(preds[1], preds[n()]),
     gxt_duration_slp = cmp_slope(
      exam_age,
      preds,
      x_sd = .recipe$steps[[2]]$sds['exam_age'],
      x_mean = .recipe$steps[[2]]$means['exam_age']
     )
    )
  }
 )

 data_excluded$data_cvd$ID <- paste0('..', data_excluded$data_cvd$ID)
 data_excluded$data_cvd$ID <- factor(data_excluded$data_cvd$ID)

 map2(data_gxt_imputed, gxt_int_slp, left_join) |>
  map(filter, exam == '..G') |>
  map(left_join, data_excluded$data_cvd) |>
  map(ungroup) |>
  map(mutate,
      gxt_int_cat = cut(gxt_duration_int,
                        breaks = c(-Inf, 480, 720, Inf),
                        include.lowest = TRUE,
                        labels = c("..lt_8m", "..gteq_8m_lt_12m", "..gteq_12m")),
      gxt_reduce_cat = cut(gxt_duration_slp * (-1),
                           breaks = c(-Inf, 5, 10, Inf),
                           include.lowest = TRUE,
                           labels = c("..lt_5s", "..gteq_5s_lt_10s", "..gteq_10s")),
      gxt_pch_cat = cut(gxt_duration_pch * (-1),
                        breaks = c(-Inf, 0.25, 0.35, Inf),
                        labels = c("..lt_25", "..gteq_25_lt_35", "..gteq_35")),
      gxt_int_cat = fct_rev(gxt_int_cat)
  )

}
