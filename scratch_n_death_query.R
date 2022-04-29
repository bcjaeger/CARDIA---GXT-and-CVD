

library(tidyverse)
library(haven)

# total number of adjudicated deaths according to z33e21
orig <- read_sas("data/outcomes21_v2.sas7bdat") |>
 drop_na(DEAD21) |>
 filter(cvdafnfgtt > 0, DEAD21gtt > 0)

# total number of adjudicated deaths according to
alex <- read_sas('data/outcomes21_v3.sas7bdat') |>
 drop_na(DEAD21) |>
 filter(cvdafnfgtt > 0, DEAD21gtt > 0)

dd <- bind_rows(orig = orig, alex = alex, .id = 'data')

fit_surv <- survfit(Surv(cvdafnfgtt, CVDafnf) ~ data,
                    data = dd)

fit_surv <- survfit(Surv(DEAD21gtt, DEAD21) ~ data,
                    data = dd)

library(survminer)

fig_kap <- ggsurvplot(
 fit_surv,
 data = orig,
 fun = 'event',
 palette = "lancet",
 ggtheme = theme_bw(),
 fontsize = 12,
 conf.int = TRUE,
 risk.table.fontsize = 3.5,
 legend.title = "Treatment",
 risk.table = 'nrisk_cumevents',
 ylab = 'Cumulative incidence',
 xlab = 'Years since randomization',
 ylim = c(0, 0.08),
 break.x.by = 500,
 break.y.by = 0.05,
 censor.size = 0,
 surv.scale = 'percent',
 tables.theme = theme_void(),
 tables.height = 1/8
)

fig_kap

cmli_orig <- cuminc(ftime   = orig$cvdafnfgtt,
                    fstatus = orig$CVDafnf)

cmli_alex <- cuminc(ftime   = alex$cvdafnfgtt,
                    fstatus = alex$CVDafnf)

ggdat <- list(`Original data` = cmli_orig,
              `Newer data` = cmli_alex) %>%
 map_dfr(
  ~ .x |>
   ggcompetingrisks(conf.int = T) %>%
   getElement('data') %>%
   as_tibble() %>%
   select(
    time,
    group, # same group as in cuminc(),
    est, # est is cumulative incidence,
    std # std is the standard deviation of est
   ),
  .id = 'data'
 )

sfit <-
 survfit(formula = Surv(DEAD21gtt,DEAD21)~1, data = orig)

tibble(time = sfit$time, surv = sfit$surv) |>
 ggplot(aes(x = time, y = 1-surv)) +
 geom_line()

ggplot(ggdat, aes(x = time, y = est, col = data)) +
 geom_line(size=0.9) +
 # This is a theme that is consistent with many journals expectations
 # for figures
 theme_bw()+
 theme(
  panel.grid = element_blank(),
  text = element_text(size = 13, color = 'black', face = 'bold')
 ) +
 scale_y_continuous(label=scales::percent) +
 labs(x='Time, years', y = 'Cumulative Incidence, any CVD')

