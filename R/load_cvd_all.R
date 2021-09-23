#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_cvd_all <- function() {

  outcomes <- read_sas('data/outcomes21.sas7bdat') |>
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

}
