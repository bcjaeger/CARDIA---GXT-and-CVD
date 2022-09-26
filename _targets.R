## Load your packages, e.g. library(targets).
source("./packages.R")

# TODO:
# re-run analysis excluding participants who died within 2 yrs of Y20
# verify the initialize_duration and fill in Kelley's example
# mixed model method for exposure


## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()

# set up sens analysis with >= 1 and >= 2 gxt events

analyses <-
 expand_grid(
  group = c("overall",
            "sex",
            "race",
            "race_sex"),
  outcomes = c('time_cvd_any_y20..status_cvd_any',
               'time_death_y20..status_death'),
  exposure = c("gxt_int_cat",
               "gxt_pch_cat",
               "gxt_duration_int",
               "gxt_duration_pch")
 ) |>
 separate(outcomes, into = c('time', 'status'), sep = '\\.\\.') |>
 mutate(outcome_label = if_else(str_detect(status, 'cvd'), 'cvd', 'death'))

list(

 tar_target(recoders, recoders_make()),

 tar_target(data_gxt_all, load_gxt_all()),

 tar_target(data_cvd_all, load_cvd_all()),

 tar_target(data_included, exclude(data_gxt_all, data_cvd_all,
                                   sensitivity_analysis = T)),

 tar_target(IDs_included, unique(data_included$data_gxt$ID)),

 tar_target(tbl_chrs, tabulate_characteristic(data_included$data_gxt)),

 tar_target(tbl_cod, tabulate_cod(data_included)),

 tar_target(fig_kaps, visualize_events(data_gxt_all,
                                       data_cvd_all,
                                       IDs_included,
                                       grp_vars = c("CENTER","race","sex"))),

 tar_target(data_gxt_imputed, gxt_impute(data_gxt_all,
                                         data_cvd_all,
                                         IDs_included,
                                         n_impute = 10,
                                         n_iter = 10)),

 tar_target(data_analysis, gxt_prep(data_gxt_imputed, data_included)),

 cph <- tar_map(
  values = analyses,
  names = c('group', 'outcome_label', 'exposure'),

  tar_target(incidence, cph_incidence(data_analysis,
                                      subset = group,
                                      exposure = exposure,
                                      time = time, status = status)),

  tar_target(fit, cph_fit(data_analysis,
                          subset = group,
                          exposure = exposure,
                          time = time,
                          status = status)),

  tar_target(tbl, tabulate_fit_list(fit,
                                    subset = group,
                                    data = data_analysis[[1]],
                                    exposure = exposure))
 ),

 tar_combine(
  cph_tbl,
  cph[[3]],
  command = bind_rows(!!!.x, .id = "variable") |>
   mutate(group = str_remove_all(group, '\\.\\.'),
          outcome = if_else(str_detect(variable, 'cvd'),
                            true = 'cvd',
                            false = 'death'),
          exposure = str_remove(variable, '.*cvd_|.*death_')) |>
   select(-variable)
 ),

 tar_combine(
  cph_inc,
  cph[[1]],
  command = bind_rows(!!!.x, .id = "variable") |>
   mutate(group = str_remove_all(group, '\\.\\.'),
          outcome = if_else(str_detect(variable, 'cvd'),
                            true = 'cvd',
                            false = 'death'),
          level = exposure,
          exposure = str_remove(variable, '.*cvd_|.*death_'),
          level = str_remove(level, '\\.\\.')) |>
   select(-variable)
 ),


 spline <- tar_map(

  values = filter(analyses, exposure %in% c("gxt_duration_int",
                                            "gxt_duration_pch")),

  names = c('group', 'outcome_label', 'exposure'),

  tar_target(ns_data, cph_fit_ns(data_analysis,
                                 subset = group,
                                 exposure = exposure,
                                 time = time,
                                 status = status))
 ),

 tar_combine(spline_data, spline[[1]],
             command = bind_rows(!!!.x)),

 tar_target(fig_splines,
            visualize_splines(spline_data, recoders)),

 tar_target(fig_forest,
            visualize_hr_forest(recoders, cph_inc, cph_tbl)),


 tar_render(manuscript, "doc/manuscript.Rmd")


)
