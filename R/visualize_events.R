#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_excluded
visualize_events <- function(data_gxt_all,
                             data_cvd_all,
                             IDs_included,
                             grp_vars) {

  data_descriptive <- data_gxt_all |>
   mutate(ID = as.numeric(ID)) |>
   left_join(data_cvd_all) |>
   filter(ID %in% IDs_included)

  plots <- list()

  for(g in grp_vars){

   cml_inc <- cuminc(ftime   = data_descriptive$time_cvd_any_y20,
                     fstatus = data_descriptive$status_cvd_any,
                     group   = data_descriptive[[g]])

   ggdat <- cml_inc %>%
    ggcompetingrisks(conf.int = T) %>%
    getElement('data') %>%
    as_tibble() %>%
    select(
     time,
     group, # same group as in cuminc(),
     est, # est is cumulative incidence,
     std # std is the standard deviation of est
    )

   plots[[g]] <- ggplot(ggdat, aes(x = time, y = est, col = group))+
    geom_line(size=0.9)+
    # This is a theme that is consistent with many journals expectations
    # for figures
    theme_bw()+
    theme(
     panel.grid = element_blank(),
     text = element_text(size = 13, color = 'black', face = 'bold')
    ) +
    scale_y_continuous(label=scales::percent) +
    labs(x='Time, years', y = 'Cumulative Incidence, any CVD')

  }

  plots

}



