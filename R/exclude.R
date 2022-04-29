#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_gxt_all
#' @param data_cvd
exclude <- function(data_gxt_all, data_cvd) {

 # convert to long data before any exclusions
 e0 <- data_gxt_all |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = -c(ID, CENTER, race, sex)) |>
  separate(name, into = c('variable', 'exam'), sep = '\\.\\.') |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
   across(
    .cols = c(exam_age,
              gxt_duration,
              gxt_hr_s2,
              gxt_pe_max,
              gxt_hr_max,
              hr_max_predicted,
              bmi,
              pa_self),
    .fns = as.numeric
   ),
   gxt_valid = as.logical(gxt_valid),
   gxt_valid = replace(gxt_valid, is.na(gxt_duration), FALSE)
  )

 n_participants <- c(length(unique(e0$ID)))

 # exclusion 1: participants who completed gxt duration in >= 1 visit
 e1 <- e0 |>
  group_by(ID) |>
  mutate(n_complete_gxt = sum(!is.na(gxt_duration)),
         .after = ID) |>
  ungroup() |>
  filter(n_complete_gxt >= 1) |>
  select(-n_complete_gxt)

 n_participants <- c(n_participants, length(unique(e1$ID)))

 # drop missings to get the actual count of GXT tests
 e1 |>
  drop_na(gxt_duration) |>
  count(exam)

 e1 |>
  drop_na(gxt_duration) |>
  pull(ID) |>
  unique() |>
  length()

 # exclusion 2: no beta blockers
 e2 <- e1 |>
  mutate(
   # assume no beta blockers if they weren't recorded in the meds data
   blocker_beta = replace(blocker_beta, is.na(blocker_beta), 'no'),
   blocker_beta_during_gxt = if_else(
    condition = exam %in% c("A","D","G") & blocker_beta == 'yes',
    true = 'yes',
    false = 'no'
   )
  ) |>
  filter(blocker_beta_during_gxt == 'no') |>
  select(-starts_with('blocker_'))

 e2_ids <- e2 |>
   drop_na(gxt_duration) |>
   pull(ID) |>
   unique()

 n_participants <- c(n_participants, length(e2_ids))


 # drop missings to get the actual count of GXT tests
 e2 |>
  drop_na(gxt_duration) |>
  count(exam)


 # Switching to exclusions based on CVD

 e3 <- data_cvd |>
   filter(ID %in% e2_ids) |>
   filter(time_death_y20 > 0) |>
   filter(time_cvd_any_y20 > 0) |>
   mutate(ID = as.character(ID))

 n_participants <- c(n_participants, length(e3$ID))


 names(n_participants) <- c(
   "CARDIA participants enrolled at Year 0 exam",
   "Completed at least one graded exercise test",
   "Not using beta blockers during graded exercise test",
   "Alive and CVD free at Year 20 exam"
 )

 list(table = enframe(n_participants),
      data_cvd = e3,
      data_gxt = filter(e2, ID %in% e3$ID))

}
