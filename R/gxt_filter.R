#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param IDS_included
gxt_filter <- function(data_gxt_imputed_all,
                       IDs_included) {

  map(data_gxt_imputed_all,
      filter,
      ID %in% paste("..", IDs_included, sep = ''))

}
