## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()

# set up sens analysis with >= 1 and >= 2 gxt events

tar_plan(

 data_gxt_all = load_gxt_all(),

 data_cvd_all = load_cvd_all(),

 data_excluded = exclude(data_gxt_all,
                         data_cvd_all),

 tbl_chrs = tabulate_characteristic(data = data_excluded$data_gxt),

 IDs_included = unique(data_excluded$data_gxt$ID),

 fig_kaps = visualize_events(data_gxt_all,
                             data_cvd_all,
                             IDs_included,
                             grp_vars = c("CENTER","race","sex")),

 data_gxt_imputed = gxt_impute(data_gxt_all,
                               IDs_included,
                               n_impute = 5,
                               n_iter = 5),

 data_analysis = make_analysis_data(data_gxt_imputed,
                                    data_excluded),

 fit_cvd_any = cph_fit(data_analysis,
                       exposures = c("gxt_int_cat", "gxt_pch_cat"),
                       status = 'status_cvd_any',
                       time = 'time_cvd_any_y20'),

 fit_death = cph_fit(data_analysis,
                     exposures = c("gxt_int_cat", "gxt_pch_cat"),
                     status = 'status_death',
                     time = 'time_death_y20'),


 tbl_fit_cvd = tabulate_fit(fit_cvd_any,
                            data = data_analysis[[1]],
                            exposures = c("gxt_int_cat", "gxt_pch_cat")),

 tbl_fit_death = tabulate_fit(fit_death,
                              data = data_analysis[[1]],
                              exposures = c("gxt_int_cat", "gxt_pch_cat")),



 tar_render(report, "doc/report.Rmd")

)
