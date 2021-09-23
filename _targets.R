## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

 data_gxt_all = load_gxt_all(),

 data_cvd_all = load_cvd_all(),

 data_analysis = exclude(data_gxt_all,
                         data_cvd_all)



)
