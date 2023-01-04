#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param recoders
#' @param cph_inc
#' @param cph_tbl
visualize_hr_forest <- function(recoders, cph_inc, cph_tbl) {

 recoders$variable['gxt_duration_int'] <-
  "GXT duration at Y0"

 recoders$variable['gxt_duration_pch'] <-
  "percent of GXT duration retained at Y20 versus Y0"

 rspec <- round_spec() |>
  round_using_magnitude(digits = c(2, 2, 1),
                        breaks = c(1, 10, Inf))

 cph_to_merge <- cph_inc |>
  select(-exposure, -level) |>
  distinct()

 data_merged <- cph_tbl |>
  left_join(cph_to_merge) |>
  select(group,
         outcome,
         n_events,
         n_person,
         i_est,
         i_lwr,
         i_upr,
         exposure,
         level,
         model,
         hr_est = est,
         hr_lwr = lwr,
         hr_upr = upr,
         pval_race,
         pval_sex) |>
  mutate(exposure = fct_relevel(exposure,
                                'gxt_duration_int',
                                'gxt_duration_pch')) |>
  arrange(group, outcome, exposure) |>
  mutate(across(.cols = c(hr_est,
                          hr_lwr,
                          hr_upr),
                .f = fill_miss,
                value = 1),
         exposure = recode(exposure, !!!recoders$variable),
         level = if_else(is.na(level),
                         as.character(exposure),
                         level),
         level = recode(level, !!!recoders$levels),
         # level = paste('    ', level),
         hr = table_glue('{hr_est} ({hr_lwr}, {hr_upr})', rspec = rspec),
         hr = str_replace(hr, fixed("1.0 (1.0, 1.0)"), '1 (Reference)'),
         inc = table_glue('{100*i_est} ({100*i_lwr}, {100*i_upr})',
                          rspec = rspec),
         inc = recode(inc, '-- (--, --)' = '--'))

 outcome_recoders <- c("cvd" = 'Cardiovascular events',
                       "death" = 'All-cause mortality')

 hr_lowest_val <- 0.2

 gg_splits <- data_merged |>
  filter(exposure %in% c(recoders$variable[c('gxt_duration_int',
                                             'gxt_duration_pch')])) |>
  mutate(outcome = recode(outcome, !!!outcome_recoders),
         group = fct_relevel(group,
                             'overall',
                             'meeting_guidelines',
                             'not_meeting_guidelines',
                             'Black',
                             'White',
                             'Male',
                             'Female',
                             'Black_Male',
                             'Black_Female',
                             'White_Male',
                             'White_Female'),
         group = fct_recode(group,
                            'Overall' = 'overall',
                            'Yes' = 'meeting_guidelines',
                            'No' = 'not_meeting_guidelines',
                            'Men' = 'Male',
                            'Women' = 'Female',
                            'Black Men' = 'Black_Male',
                            'Black Women' = 'Black_Female',
                            'White Men' = 'White_Male',
                            'White Women' = 'White_Female'),
         across(starts_with('pval'), table_pvalue)) |>
  group_by(exposure, outcome, model) |>
  arrange(group) |>
  mutate(
   line_arrow_lwr_begin = if_else(hr_lwr < hr_lowest_val,
                                  hr_upr,
                                  NA_real_),
   line_arrow_lwr_end = if_else(hr_lwr < hr_lowest_val,
                                hr_lowest_val,
                                NA_real_),
   hr_lwr = pmax(hr_lwr, hr_lowest_val),
   hr_est = if_else(hr_est < hr_lowest_val, NA_real_, hr_est),
   n_label = table_glue("{n_events} / {n_person}")
  )

 out <- gg_splits |>
  nest() |>
  mutate(
   unit = case_when(
    exposure == recoders$variable['gxt_duration_int'] ~ "Per 1 minute increase",
    exposure == recoders$variable['gxt_duration_pch'] ~ "Per 5% retained",
   ),
   plot = pmap(
    .l = list(outcome, exposure, unit, data),
    .f = function(.outcome, .exposure, .unit, ..data){
     forest_worker(data = ..data,
                   header_top = paste(.outcome, 'and', .exposure),
                   header_unit = .unit)
    }
   )
  )

 out

}


forest_worker <- function(data,
                          header_top = 'OK',
                          header_pval = 'P-value for interaction',
                          header_unit = 'OK',
                          y_col_0 = exp(-5),
                          y_col_a = exp(-3.75),
                          y_col_1 = exp(-2.5),
                          y_col_2 = exp(-1.3),
                          y_col_3 = exp(1.25),
                          y_col_4 = exp(2),
                          size_text = 3,
                          size_arrow = 0.15,
                          include_PA = TRUE){

 if(include_PA){

  gg_data <- data |>
   mutate(group = as.character(group),
          group = if_else(group == 'Overall',
                          group,
                          paste("  ", group))) |>
   add_row(group = "Meeting PA Guidelines", .before = 2) %>%
   add_row(group = 'Race', .before = 5) |>
   add_row(group = 'Sex', .before = 8) |>
   add_row(group = 'Race by Sex', .before = 11) |>
   mutate(x = rev(seq(n())))

 } else {

  gg_data <- data |>
   filter(group != 'Meeting PA Guideline',
          group != "No",
          group != "Yes") %>%
   mutate(group = as.character(group),
          group = if_else(group == 'Overall',
                          group,
                          paste("  ", group))) |>
   add_row(group = 'Race', .before = 2) |>
   add_row(group = 'Sex', .before = 5) |>
   add_row(group = 'Race by Sex', .before = 8) |>
   mutate(x = rev(seq(n())))

 }

 xmax <- max(gg_data$x)
 ymax <- y_col_4 + 4

 y_pval <- exp((log(y_col_3) + log(y_col_4)) / 2)
 y_hr <- exp(log(y_col_2) + log(y_col_3)*0.5)
 y_mid <- exp((log(y_col_0) + log(y_col_4)) / 2)

 header_inc <- 'At 10 years post Y20 exam'

 gg_header <- tribble(
  ~label                           , ~x    , ~y       , ~hjust, ~fontface,
  "Group"                          , xmax+2,  y_col_0 ,  0    , "bold",
  "N events / participants"        , xmax+2,  y_col_a ,  0.5  , "bold",
  "Cumulative\nIncidence (95% CI)" , xmax+2,  y_col_1 ,  0.5  , "bold",
  "Hazard ratio (95% CI)"          , xmax+2,  y_hr    ,  0.5  , "bold",
  "Race"                           , xmax+1,  y_col_3 ,  0.5  , "italic",
  "Sex"                            , xmax+1,  y_col_4 ,  1    , "italic",
  header_inc                       , xmax+1,  y_col_1 ,  0.5  , "italic",
  header_unit                      , xmax+1,  y_hr    ,  0.5  , "italic",
  header_pval                      , xmax+2,  y_pval  ,  0.5  , "bold",
  header_top                       , xmax+3.5,  y_mid ,  0.5  , "bold"
 )


 gg_rect <- tibble(
  xmin = seq(xmax+1) - 1/2,
  xmax = seq(xmax+1) + 1/2,
  ymin = y_col_0,
  ymax = ymax
 ) |>
  filter(seq(n()) %% 2 == 0)

 gg_arrows_bottom <- tibble(
  x = c(0, 0),
  y = c(exp(-1/4), exp(1/4)),
  yend = c(exp(-1), exp(1)),
  label = c("Favors\nHigher value", "Favors\nLower value")
 )

 fig_bottom <- ggplot(gg_arrows_bottom) +
  aes(x=x, xend=x, y=y, yend=yend, label=label) +
  geom_segment(arrow=arrow(length = unit(size_arrow, 'cm'))) +
  geom_text(size = size_text,
            hjust = c(1,0),
            vjust = 1/2) +
  scale_y_log10(limits = c(y_col_0, ymax),
                breaks = c(0.5, 1, 2),
                expand = c(0,0)) +
  coord_flip() +
  theme_void()

 fig_main <- ggplot(gg_data) +
  aes(x = x) +
  geom_rect(data = gg_rect,
            inherit.aes = FALSE,
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = 'grey',
            alpha = 1/5) +
  coord_flip() +
  theme_bw() +
  scale_y_log10(limits = c(y_col_0, ymax), breaks = c(0.5, 1, 2),
                expand = c(0,0)) +
  scale_x_continuous(limits = c(1, max(gg_header$x))) +
  labs(y = 'Hazard ratio') +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank()) +
  geom_text(aes(label = group, y = y_col_0), hjust = 0, size = size_text) +
  geom_text(aes(label = n_label, y = y_col_a), hjust = 0.5, size = size_text) +
  geom_text(aes(label = inc, y = y_col_1), hjust = 0.5, size = size_text) +
  geom_text(aes(label = hr, y = y_col_2), hjust = 0.5, size = size_text) +
  geom_text(aes(label = pval_race, y = y_col_3), hjust = 1, size = size_text) +
  geom_text(aes(label = pval_sex, y = y_col_4), hjust = 1, size = size_text) +
  geom_text(data = gg_header,
            aes(x = x,
                y = y,
                label = label,
                hjust = hjust,
                fontface = fontface),
            size = size_text) +
  geom_vline(xintercept = max(gg_header$x)-3/4) +
  geom_vline(xintercept = xmax + 1/2) +
  geom_segment(y = 0, yend = 0,
               x = 0, xend = xmax + 1/3,
               color = 'grey',
               alpha = 0.5,
               linetype = 2) +
  geom_segment(y = log(0.5)/2, yend = log(2)/2,
               x = .4, xend = .4) +
  geom_segment(aes(xend = x,
                   y = line_arrow_lwr_begin,
                   yend = line_arrow_lwr_end),
               arrow = arrow(length = unit(size_arrow, 'cm'))) +
  geom_point(aes(y = hr_est), shape = 15, size = 3) +
  geom_linerange(aes(ymin = hr_lwr, ymax = hr_upr))

 fig <- cowplot::plot_grid(fig_main,
                           fig_bottom,
                           align = 'v',
                           nrow=2, rel_heights = c(14, 1))

 fig

}

