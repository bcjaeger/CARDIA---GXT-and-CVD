#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @param data
#' @param time
#' @param status
#'
#' @title

cmp_incidence <- function(data,
                          time,
                          status,
                          time_predict = NULL,
                          time_scale = 1) {


 if(!is.null(time_predict)){
  index_curtail <- data[[time]] > time_predict
  data[[time]][index_curtail] <- time_predict
  data[[status]][index_curtail] <- 0
 }

 total_events <- sum(getElement(data, status))
 total_time <- sum(getElement(data, time)) * time_scale

 estimate = total_events / total_time

 fit <- glm(total_events ~ offset(log(total_time)), family="poisson")

 fit_ci <- exp(confint(fit))

 list(total_events = total_events,
      total_censored = sum(data[[status]] == 0),
      total_n = nrow(data),
      total_time = total_time,
      incidence_est = estimate,
      incidence_lwr = fit_ci['2.5 %'],
      incidence_upr = fit_ci['97.5 %'])

}
