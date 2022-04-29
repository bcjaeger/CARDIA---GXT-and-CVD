#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_death
tabulate_fit <- function(fit, data, exposure) {

 estimates <- map(fit$fits, testEstimates)

 estimate_tbl <- map_dfr(
  .x = estimates,
  .f = ~ {

   .ci <- confint(.x) |>
    as_tibble(rownames = 'variable')

   .est <- as_tibble(.x$estimates)

   ref_cats <- level_order <- NULL

   if(is.factor(data[[exposure]])){

    ref_cats <- data |>
     select(all_of(exposure)) |>
     map_chr(~levels(.x)[1]) |>
     enframe(name = 'variable', value = 'level') |>
     mutate(level = str_remove(level, '\\.\\.'))

    level_order <- data |>
     select(all_of(exposure)) |>
     map(levels) |>
     reduce(.f = c) |>
     str_remove('\\.\\.')

   }

   .data_tbl <- tibble(
    var = .ci$variable,
    est = .est$Estimate,
    lwr = .ci$`2.5 %`,
    upr = .ci$`97.5 %`
   ) |>
    separate(var,
             into = c("variable", "level"),
             sep = "\\.\\.", # this is why I use '..' in factor levels
             fill = "right") |>
    filter(variable == exposure) |>
    bind_rows(ref_cats) |>
    mutate(level = factor(level, levels = level_order)) |>
    arrange(level) |>
    mutate(level = as.character(level))

   .data_tbl |>
    mutate(across(.cols = c(est, lwr, upr), exp))
  },

  .id = 'model'

 )

 if(nrow(fit$pvals) > 0){

  pval_tbl <- fit$pvals |>
   pivot_longer(cols = c(m1,m2,m3),
                names_to = 'model',
                values_to = 'pval') |>
   pivot_wider(names_from = interaction,
               names_prefix = 'pval_',
               values_from = pval)

  return(left_join(estimate_tbl, pval_tbl))

 }

 estimate_tbl

}

tabulate_fit_list <- function(fit, subset, data, exposure){

 if(subset == 'overall'){
  return(tabulate_fit(fit, data, exposure) |>
          mutate(group = 'overall'))
 }

 map_dfr(fit,
         tabulate_fit,
         data = data,
         exposure = exposure,
         .id = 'group')


}
