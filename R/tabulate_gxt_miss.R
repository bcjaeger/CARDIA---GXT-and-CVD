#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_included
tabulate_gxt_miss <- function(data_included) {

 data_to_tabulate <- data_included$data_gxt %>%
  filter(exam %in% c("A", "D", "G"))

 overall <- data_to_tabulate %>%
  nest(data = everything()) %>%
  mutate(group = 'Overall', .before = 1)

 by_sex <- data_to_tabulate %>%
  group_by(sex) %>%
  nest() %>%
  rename(group = sex)

 by_race <- data_to_tabulate %>%
  group_by(race) %>%
  nest() %>%
  rename(group = race)

 by_sex_race <- data_to_tabulate %>%
  group_by(race, sex) %>%
  nest() %>%
  unite(col = 'group', race, sex, sep = ' ') %>%
  mutate(group = paste(group, 's', sep = ''))

 bind_rows(Overall = overall,
           Sex = by_sex,
           Race = by_race,
           `Race-by-sex` = by_sex_race,
           .id = 'variable') %>%
  mutate(
   tbl = map(
    .x = data,
    .f = ~  .x %>%
     group_by(exam) %>%
     summarize(
      n_gxt = sum(!is.na(gxt_duration)),
      p_gxt = mean(!is.na(gxt_duration))
     ) %>%
     mutate(tbl_value = table_glue("{n_gxt} ({100 * p_gxt}%)"))
   )
  ) %>%
  unnest(tbl) %>%
  select(-data, -ends_with("gxt")) %>%
  pivot_wider(names_from = exam, values_from = tbl_value)

}
