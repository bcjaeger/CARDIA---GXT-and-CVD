#' Get index of pattern in coefficient names
#'
#' @param fit
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
get_index <- function(fit, pattern){
 which(stringr::str_detect(names(coef(fit)), pattern = pattern))
}

#' get spline predictions
#'
#' @param fit
#' @param basis
#' @param pattern
#' @param x_min
#' @param x_max
#' @param x_ref
#'
#' @return
#' @export
#'
#' @examples
get_spline_preds <- function(fit,
                             basis,
                             pattern,
                             x_min,
                             x_max,
                             x_ref = NULL){

 spline_index <- get_index(fit, pattern = pattern)
 spline_coef <- coef(fit)[spline_index]
 spline_covariance <- vcov(fit)[spline_index, spline_index, drop = FALSE]

 x_grid <- seq(x_min, x_max, length.out = 1000)
 x_mat <- predict(basis, newx = x_grid)

 if(!is.null(x_ref)){
  ref_index <- which.min(abs(x_grid - x_ref))
  x_mat <- sweep(x_mat, MARGIN = 2, STATS = x_mat[ref_index, ])
 }

 xb <- x_mat %*% spline_coef
 xb_stderr <- sqrt(diag(x_mat %*% tcrossprod(spline_covariance, x_mat)))
 lwr_ci <- xb - qt(0.975, length(fit$residual)+1) * xb_stderr
 upr_ci <- xb + qt(0.975, length(fit$residual)+1) * xb_stderr

 if(requireNamespace('tibble', quietly = TRUE)){
  return(
   tibble::tibble(
    x      = x_grid,
    pred   = as.numeric(xb),
    se     = as.numeric(xb_stderr),
    ci_lwr = as.numeric(lwr_ci),
    ci_upr = as.numeric(upr_ci)
   )
  )
 }

 data.frame(
  x      = x_grid,
  pred   = as.numeric(xb),
  se     = as.numeric(xb_stderr),
  ci_lwr = as.numeric(lwr_ci),
  ci_upr = as.numeric(upr_ci)
 )

}
