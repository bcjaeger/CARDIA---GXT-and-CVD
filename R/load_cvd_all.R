#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_cvd_all <- function() {

 # import mortality data ----------------------------------------------------

 mortality_q <- read_sas('data/z33e21.sas7bdat')

 data_mortality <- mortality_q |>
  filter(Z33ECLASS != 1,
         Z33ECLASS != 2) |>
  transmute(
   ID = as.character(ID),
   cod_noncvd = recode(
    Z33ENCD,
    "0" = NA_character_,
    "1" = "AIDS",
    "2" = "cancer",
    "3" = "diabetes",
    "4" = "homicide",
    "5" = "kidney",
    "6" = "liver",
    "7" = "asthma",
    "8" = "other_lung_disease",
    "9" = "suicide",
    "10" = "unintentional_injury",
    "11" = "other",
    "12" = "sepsis"
   ),
   cod_cvd = recode(
    Z33ECLAS,
    "1" = "cvd_athero_CHD",
    "2" = "cvd_athero_stroke",
    "3" = "cvd_athero_other",
    "4" = "cvd_non_athero",
    "5" = "cvd_pulm_embo",
    "6" = "non_cvd",
    "7" = "unknown"
   ),
   cod = if_else(condition = is.na(cod_noncvd),
                 true = cod_cvd,
                 false = cod_noncvd)
  ) |>
  distinct()

 outcomes <- read_sas('data/outcomes21_v3.sas7bdat') |>
  mutate(ID = as.character(ID)) |>
  select(ID,

         status_chd_any     = CHDafnf,
         status_chd_hard    = CHDhfnf,
         status_stroke_any  = strokeafnf,
         status_stroke_hard = strokehfnf,
         status_cvd_any     = CVDafnf,
         status_death       = DEAD21,

         time_chd_any_y20     = CHDafnfgtt,
         time_chd_hard_y20    = CHDhfnfgtt,
         time_stroke_any_y20  = strokeafnfgtt,
         time_stroke_hard_y20 = strokehfnfgtt,
         time_cvd_any_y20     = cvdafnfgtt,
         time_death_y20       = DEAD21gtt)

 left_join(outcomes, data_mortality) |>
 # there is one participant with unknown COD and status_death = 0
 # we are choosing to 'fix' the status_death value by assigning it to 1
 mutate(
  status_death = if_else(status_death == 0 & !is.na(cod), 1, status_death)
 )

}
