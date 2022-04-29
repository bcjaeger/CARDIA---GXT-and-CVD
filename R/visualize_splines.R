#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param spline_data
visualize_splines <- function(spline_data, recoders) {


 recoders$variable['gxt_duration_int'] <-
  "GXT duration at Y0, minutes"

 recoders$variable['gxt_duration_pch'] <-
  "Percent of GXT duration retained at Y20 versus Y0"

 outcome_recoders <- c("cvd_any" = 'Cardiovascular events',
                       "death" = 'All-cause mortality')

 gg_data <- spline_data |>
  filter(model == 'm3') |>
  mutate(exposure = recode(exposure, !!!recoders$variable),
         outcome = recode(outcome, !!!outcome_recoders)) |>
  unnest(cols = 'data') |>
  group_by(group) |>
  group_split()

 text_data <-
  map(gg_data, mutate, pred = pmin(max(ci_upr * 1.10),log(8))) |>
  map(group_by, exposure, outcome) |>
  map(slice, 450) |>
  map(select, model:pred) |>
  map(mutate,
      across(starts_with('pval'), table_pvalue),
      pval = paste("Linear effect; p =", pval_effect,
                   "\nNon-linear effect; p =", pval_nonlinear))

 out_data <- map_chr(text_data, ~.x$group[1]) |>
  enframe(name = NULL, value = 'group') |>
  mutate(
   plot = map2(
    .x = gg_data,
    .y = text_data,
    .f = ~
     ggplot(.x) +
     aes(
      x = x,
      y = exp(pred),
      ymin = exp(ci_lwr),
      ymax = exp(ci_upr)
     ) +
     labs(x = '',
          y = 'Hazard ratio') +
     facet_grid(outcome~exposure, scales = 'free_x', switch = 'both') +
     geom_text(data = .y,
               inherit.aes = FALSE,
               hjust = 1/2,
               mapping = aes(x = x, y = exp(pred), label = pval)) +
     geom_line() +
     geom_ribbon(alpha = 0.2) +
     scale_y_log10(limits = c(min(exp(.x$ci_lwr)),
                              max(exp(.y$pred)*1.15))) +
     theme_bw() +
     theme(panel.grid = element_blank()) +
     geom_hline(yintercept = 1, color = 'red', linetype = 2)
   )
  )

 out_data


 }

