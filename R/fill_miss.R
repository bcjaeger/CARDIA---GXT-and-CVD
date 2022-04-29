#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param value
fill_miss <- function(x, value){

 na_indx <- which(is.na(x))
 x[na_indx] <- value
 x

}
