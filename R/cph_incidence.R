##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param time
##' @param data
##' @param status
cph_incidence <- function(data,
                          subset,
                          exposure,
                          time,
                          status) {

 n_imputes <- length(data)

 if(exposure %in% c('gxt_duration_int', 'gxt_duration_pch')){
  return(tibble())
 }

 # browser()

 if(subset == 'overall'){

  incidence <- map_dfr(
   # the first dataset isn't imputed
   data[-1],
   .f = function(d){
    # split(d,
    #       f = list(d[[exposure]]),
    #       sep = '') |>
    #  map_dfr(.f = cph_incidence_worker,
    #          time = time,
    #          status = status,
    #          div_time_by = div_time_by,
    #          .id = 'exposure')

    cph_incidence_worker(data = d,
                         time = time,
                         status = status)

   },
   .id = 'impute'
  ) |>
   mutate(subset = "overall")

 } else {

  incidence <- map_dfr(
   # the first dataset isn't imputed
   data[-1],
   .f = function(d){
    split(d,
          f = list(d[[subset]]), # d[[exposure]],
          sep = '') |>
     map_dfr(.f = cph_incidence_worker,
             time = time,
             status = status,
             .id = 'subset')
   },
   .id = 'impute'
  )
  # |>
  #  mutate(exposure_subset = str_remove(exposure_subset, fixed('..'))) |>
  #  separate(exposure_subset,
  #           into = c("exposure", "subset"),
  #           sep = '\\.\\.')

 }


  incidence |>
  group_by(subset) |>
  summarize(
   i_est = mean(incidence_est),
   var_within = mean(incidence_se^2),
   var_between = var(incidence_est),
   n_events = n_events[1],
   n_person = n_person[1]
  ) |>
  mutate(se_pooled = sqrt(var_within + var_between * (1 + 1/n_imputes))) |>
  transmute(exposure,
            group = subset,
            n_events,
            n_person,
            i_est,
            i_lwr = i_est - 1.96 * se_pooled,
            i_upr = i_est + 1.96 * se_pooled)


}


cph_incidence_worker <- function(data,
                                 time,
                                 status,
                                 horizon = 365.25 * 10){

 total_events <- sum(getElement(data, status))

 stats <- cuminc(ftime = data[[time]],
                 fstatus = data[[status]])

 index <- max(which(stats[[1]]$time < horizon))

 tibble(n_events = total_events,
        n_person = nrow(data),
        incidence_est = stats[[1]]$est[index],
        incidence_se = sqrt(stats[[1]]$var[index]))

}



