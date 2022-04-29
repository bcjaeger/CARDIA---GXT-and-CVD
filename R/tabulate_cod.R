#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param IDs_included
tabulate_cod <- function(data_included) {

 merge_in <- select(data_included$data_gxt, ID, race, sex) |>
  group_by(ID) |>
  slice(1)

 data_to_count <- data_included$data_cvd |>
  # there isn't perfect overlap between status_death and cod
  # so present cod among the participants who the current analysis
  # considers to have a death event.
  filter(status_death == 1) |>
  left_join(merge_in) |>
  mutate(cod = replace(cod, is.na(cod), 'unknown'),
         cod = factor(
          cod,
          levels = c(
           "AIDS",
           "asthma",
           "cancer",
           "cvd_athero_CHD",
           "cvd_athero_other",
           "cvd_athero_stroke",
           "cvd_non_athero",
           "cvd_pulm_embo",
           "diabetes",
           "kidney",
           "liver",
           "other",
           "other_lung_disease",
           "sepsis",
           "homicide",
           "suicide",
           "unintentional_injury",
           "unknown"
          )
         ),
         cod = recode(cod,
                      "AIDS" = "AIDS",
                      "asthma" = "Asthma",
                      "cancer" = "Cancer",
                      "cvd_athero_CHD" = "ASCVD, CHD",
                      "cvd_athero_other" = "ASCVD, other",
                      "cvd_athero_stroke" = "ASCVD, stroke",
                      "cvd_non_athero" = 'CVD, non-atherosclerotic',
                      "cvd_pulm_embo" = "Pulmonary embolism",
                      "diabetes" = "Diabetes",
                      "homicide" = "Homicide",
                      "kidney" = 'Kidney disease',
                      "liver" = "Liver disease",
                      "other" = "Other",
                      "other_lung_disease" = "Other",
                      "sepsis" = "Sepsis",
                      "suicide" = "Suicide",
                      "unintentional_injury" = "Unintentional injury",
                      "unknown" = 'Unknown'))

 by_sex <- data_to_count |>
  group_by(sex) |>
  count(cod) |>
  mutate(p = 100 * n / sum(n)) |>
  rename(group = sex) |>
  mutate(group = table_glue("{group} (N = {sum(n)})"))

 by_race <- data_to_count |>
  group_by(race) |>
  count(cod) |>
  mutate(p = 100 * n / sum(n)) |>
  rename(group = race) |>
  mutate(group = table_glue("{group} (N = {sum(n)})"))

 by_race_sex <- data_to_count |>
  group_by(race, sex) |>
  count(cod) |>
  mutate(p = 100 * n / sum(n),
         group = paste(race, sex, sep = '_')) |>
  mutate(group = table_glue("{group} (N = {sum(n)})")) |>
  ungroup() |>
  select(-race, -sex)

 overall <- data_to_count |>
  count(cod) |>
   mutate(p = 100 * n / sum(n)) |>
   mutate(group = "Overall") |>
   mutate(group = table_glue("{group} (N = {sum(n)})"))

 data_flex <- list(overall, by_race, by_sex, by_race_sex) |>
  bind_rows() |>
  mutate(label = table_glue("{n} ({p}%)")) |>
  select(-n, -p) |>
  pivot_wider(names_from = group, values_from = label) |>
  mutate(across(-cod, ~ replace(.x, is.na(.x), '0 (0%)')))

 names(data_flex) <- names(data_flex) |>
  str_replace('_', " ") |>
  str_replace(fixed(" ("), "\n(")

 data_flex |>
  flextable() |>
  add_header_row(
   values = c("Cause of death", "Overall", "Race", "Sex", "Race by Sex"),
   colwidths = c(1, 1, 2, 2, 4)
  ) |>
  set_header_labels(cod = 'Cause of death') |>
  theme_box() |>
  merge_v(part = 'header') |>
  width(width = 0.95) |>
  width(j=1, width = 1.5) |>
  align(align = 'center', part = 'all') |>
  align(j = 1, align = 'left', part = 'all') |>
  fontsize(size = 11, part = 'all')


}

