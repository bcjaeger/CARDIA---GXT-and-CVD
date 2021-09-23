##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
load_gxt_all <- function() {

  # import files -------------------------------------------------------------
  
  data_raw <- 
    read_sas("data/a1188req01_13_2021.sas7bdat") %>% 
    mutate(ID = as.character(ID))
  
  data_meds <- 
    read_sas("data/UCSFMedFeb2013.sas7bdat") 
  
  data_meds_blockers <- data_meds %>% 
    mutate(
      ID = as.character(ID),
      exam = recode(ExamYear,
                    '0'  = 'A',
                    '2'  = 'B',
                    '5'  = 'C',
                    '7'  = 'D',
                    '10' = 'E',
                    '15' = 'F',
                    '20' = 'G',
                    '25' = 'H')
    ) %>% 
    group_by(ID, exam) %>% 
    summarize(
      blocker_beta = if_else(
        condition = any(UCSFMedType %in% c('Beta Blockers')),
        true = 'yes',
        false = 'no'
      ),
      blocker_calcium = if_else(
        condition = any(UCSFMedType %in% c('Calcium Channel Blockers')),
        true = 'yes',
        false = 'no'
      )
    ) %>% 
    pivot_wider(names_from = exam, 
                values_from = starts_with('blocker'),
                names_sep = '..') %>% 
    # assume 'no' if blocker status is missing
    mutate(
      across(
        starts_with('blocker'), 
        ~replace(.x, is.na(.x), 'no')
      )
    ) %>% 
    select(-ends_with("H"))
  
  data_meds_highbp <- data_raw %>% 
    mutate(ID = as.character(ID)) %>% 
    select(ID, A08BPMED, D08HBNOW, G08HBNOW) %>% 
    mutate(
      across(
        .cols = c(A08BPMED, D08HBNOW, G08HBNOW),
        .fns = ~ recode(.x, 
                        '1' = "no",
                        '2' = "yes",
                        '8' = NA_character_)
      )
    ) %>% 
    rename(
      meds_bp..A = A08BPMED,
      meds_bp..D = D08HBNOW,
      meds_bp..G = G08HBNOW
    )
  
  data_y7_minn_rpe <- read_sas('data/d1f22m.sas7bdat') %>% 
    transmute(
      ID = as.character(ID),
      D22RPEMAX_corrected = pmax(D22RPES1,
                                 D22RPES2,
                                 D22RPES3,
                                 D22RPES4,
                                 D22RPES5,
                                 D22RPES6,
                                 D22RPES7,
                                 D22RPES8,
                                 D22RPES9,
                                 na.rm = TRUE)
    )
  
  # covariates: 
  #  sex, race, participating center, [done]
  #  marital status (FORM 3), difficulty paying for basics, 
  #  tobacco use, alcohol use, physical activity, obesity, 
  #  type and/or # of co-morbid conditions 
  #  (cvd [FORM 8], type 2 diab [FORM 8; self report], hypertension [FORM 8?])
  
    # Form 08
  # -Reported HTN: A-G08HBP (yes; no)
  # -Reported High Cholesterol: A-G08HCHOL (yes; no)
  # -Reported Heart Problem (CVD): A-G08HEART (yes; no)
  # -Reported Diabetes: A-GDIAB (yes; no)
  # 
  # Form 10
  # -Smoking Status: A-G10SMOKE (Never; past; current)
  # -Alcohol Use: A-G07DRINK (yes; no)
  # 
  # Form 18
  # -Reported MVPA: A-G18TOTAL (cutoff: <300 versus >=300 [meeting PA guideline])
  # 
  # Form 20
  # -BMI: A-G20BMI 
  # 
  
  data_y7_minn_corrected <- 
    read_sas('data/mnduration_corrected_210117.sas7bdat') %>% 
    mutate(ID = as.character(ID)) %>% 
    select(-short_id) %>% 
    rename(D22DURTN_corrected = D22DURTN) %>% 
    left_join(data_y7_minn_rpe)
  
  # demographic data ---------------------------------------------------------
  
  data_demo <- data_raw %>% 
    rename(EX1_AGE = EXAMAGE,
           race = RACE,
           sex = SEX) %>% 
    select(ID, race, sex, matches("^EX\\d_AGE$")) %>%
    rename(
      exam_age..A = EX1_AGE,
      exam_age..B = EX2_AGE,
      exam_age..C = EX3_AGE,
      exam_age..D = EX4_AGE,
      exam_age..E = EX5_AGE,
      exam_age..F = EX6_AGE,
      exam_age..G = EX7_AGE
    ) %>% 
    mutate(race = recode(race,
                         "4" = 'Black',
                         "5" = 'White'),
           sex = recode(sex,
                        "1" = "Male",
                        "2" = "Female"))
  
  # education varies over time and most CARDIA participants had HS or less 
  # at baseline. Should we use the mode of their education throughout
  # follow-up to be more precise?
  
  # Form 3 -------------------------------------------------------------------
  # browser()
  # -Education: A-G03DEGRE (<=HS/GED; Associate Degree; >=Bachelor Degree)
  # -use the most recent education value 
  
  # update 2021-03-12: HS/GED or less versus associates and up, 
  # group the 'other' category with...associates and up
  
  data_educ <- data_raw %>% 
    transmute(
      ID, 
      across(
        contains("DEGRE"),
        .fns = ~ recode(.x, 
                        "1" = "hs_ged_or_less",
                        "2" = "assoc_or_more",
                        "3" = "assoc_or_more",
                        "4" = "assoc_or_more",
                        "5" = "assoc_or_more",
                        "6" = "assoc_or_more",
                        "7" = "assoc_or_more",
                        "8" = "hs_ged_or_less",
                        "9" = NA_character_)
      )
    ) %>% 
    rename(
      educ..A = A03DEGRE,
      educ..B = B03DEGRE,
      educ..C = C03DEGRE,
      educ..D = D03DEGRE,
      educ..E = E03DEGRE,
      educ..F = F03DEGRE,
      educ..G = G03DEGRE
    )
  
  # -Marital Status: A-G03MRAGE (Married/Living w SO; Other)
  # -use baseline or first recorded marital status
  
  data_marital <- data_raw %>% 
    transmute(
      ID, 
      across(
        contains("MRAGE"),
        .fns = ~ recode(.x, 
                        "1" = "married_or_cohabit",
                        "2" = "other", #"Widowed",
                        "3" = "other", #"Divorced",
                        "4" = "other", #"Separated",
                        "5" = "other", #"Never married",
                        "6" = "other",
                        "7" = "married_or_cohabit",
                        "9" = NA_character_)
      )
    ) %>% 
    rename(
      marital..A = A03MRAGE,
      marital..B = B03MRAGE,
      marital..C = C03MRAGE,
      marital..D = D03MRAGE,
      marital..E = E03MRAGE,
      marital..F = F03MRAGE,
      marital..G = G03MRAGE
    )
    
  # -Paying for Basics: A-G03DFPAY (Not very hard; other)
  data_basics <- data_raw %>% 
    transmute(
      ID, 
      across(
        contains("DFPAY"),
        .fns = ~ recode(.x, 
                        "1" = "somewhat_hard",
                        "2" = "somewhat_hard", 
                        "3" = "somewhat_hard", 
                        "4" = "not_very_hard", 
                        "5" = NA_character_,
                        "8" = NA_character_,
                        "9" = NA_character_)
      )
    ) %>% 
    rename(
      paying_for_basics..A = A03DFPAY,
      paying_for_basics..B = B03DFPAY,
      paying_for_basics..D = D03DFPAY,
      paying_for_basics..E = E03DFPAY,
      paying_for_basics..F = F03DFPAY,
      paying_for_basics..G = G03DFPAY
    )

  
  # BMI ----
  
  data_bmi <- data_raw %>% 
    select(ID, ends_with("BMI")) %>% 
    rename(
      bmi..A = A20BMI,
      bmi..B = B20BMI,
      bmi..C = C20BMI,
      bmi..D = D20BMI,
      bmi..E = E20BMI,
      bmi..F = F20BMI,
      bmi..G = G20BMI
    )

  # alcohol use; form 7 ----
  # drinks alcohol in the past year - yes or no A07DRINK
  
  data_drinks <- data_raw %>% 
    transmute(
      ID, 
      across(
        ends_with("DRINK"),
        ~recode(.x,
               '1' = "alc_no",
               '2' = "alc_yes",
               '8' = NA_character_,
               '9' = NA_character_)
      )
    ) %>% 
    rename(
      alcohol..A = A07DRINK,
      alcohol..B = B07DRINK,
      alcohol..C = C07DRINK,
      alcohol..D = D07DRINK,
      alcohol..E = E07DRINK,
      alcohol..F = F07DRINK,
      alcohol..G = G07DRINK
    )
  
  # smoking; form 10 ----
  # currently smoking - yes or no A
  
  data_smoke <- data_raw %>% 
    transmute(
      ID, 
      across(
        ends_with("10SMOKE"),
        ~ recode(.x,
                 '0' = "never",
                 '1' = "former",
                 '2' = "current")
      )
    ) %>% 
    rename(
      smoke..A = A10SMOKE,
      smoke..B = B10SMOKE,
      smoke..C = C10SMOKE,
      smoke..D = D10SMOKE,
      smoke..E = E10SMOKE,
      smoke..F = F10SMOKE,
      smoke..G = G10SMOKE
    )
  
  # Self-reported health
  # number of co-morbid conditions; self rated health (A01HELTH)
  # turn into excellent/good versus fair/poor
  
  data_health_self_y15plus <- 
    read_sas('data/a1188additionalreq07_26_2021.sas7bdat') %>% 
    transmute(
      ID = as.character(ID),
      across(
        .cols = c(F65HELTH, G65HELTH),
        .fns = ~recode(
          .x,
          "1" = "excellent_good",
          "2" = "excellent_good",
          "3" = "excellent_good",
          "4" = "fair_poor",
          "5" = "fair_poor"
        )
      )
    ) %>% 
    rename(
      health_self..F = F65HELTH,
      health_self..G = G65HELTH
    )
  
  
  data_health_self_y0 <- read_sas('data/a4f01.sas7bdat') %>% 
    transmute(
      ID = as.character(ID), 
      health_self..A = recode(
        A01HELTH,
        "0" = NA_character_, 
        "3" = "excellent_good",
        "4" = "excellent_good",
        "5" = "fair_poor",
        "6" = "fair_poor",
        "8" = NA_character_, 
        "9" = NA_character_  
      )
    )
  
  data_health_self <- left_join(data_health_self_y0,
                                data_health_self_y15plus, 
                                by = 'ID')
  
  # Form 18
  # -Reported MVPA: A-G18TOTAL (cutoff: <300 versus >=300 [meeting PA guideline])
  
  data_pa <- data_raw %>% 
    select(ID, ends_with("18TOTAL")) %>% 
    rename(
      pa_self..A = A18TOTAL,
      pa_self..B = B18TOTAL,
      pa_self..C = C18TOTAL,
      pa_self..D = D18TOTAL,
      pa_self..E = E18TOTAL,
      pa_self..F = F18TOTAL,
      pa_self..G = G18TOTAL
    )
  
  data_gxt <- data_raw %>% 
    mutate(
      # maximum perceived exertion, 
      # used to validate Minnesota data
      A22RPEMAX = pmax(A22RPES2,
                       A22RPES3,
                       A22RPES4,
                       A22RPES5,
                       A22RPES6,
                       A22RPES7,
                       A22RPES8,
                       A22RPES9,
                       na.rm = TRUE),
      D22RPEMAX = pmax(D22RPES1,
                       D22RPES2,
                       D22RPES3,
                       D22RPES4,
                       D22RPES5,
                       D22RPES6,
                       D22RPES7,
                       D22RPES8,
                       D22RPES9,
                       na.rm = TRUE),
      G22RPEMAX = pmax(G22RPES1,
                       G22RPES2,
                       G22RPES3,
                       G22RPES4,
                       G22RPES5,
                       G22RPES6,
                       G22RPES7,
                       G22RPES8,
                       G22RPES9,
                       na.rm = TRUE)
    ) %>% 
    select(
      ID,
      CENTER,
      matches("^(A|D|G)22DURTN$"), # maximal fitness
      matches("^(A|D|G)22HRS2$"), # sub-maximal fitness
      matches("^(A|D|G)22RPEMAX$"), # max perceived exertion
      matches("^(A|D|G)22HRMAX$") # max heart rate
    ) %>%
    left_join(data_y7_minn_corrected) %>% 
    mutate(D22RPEMAX = coalesce(D22RPEMAX, D22RPEMAX_corrected))
  
  # note: 
  # 40 people have D22DURTN in Minnesota and no corrected value
  # what should we do with these data?
  # (current strategy is to keep their data)
  # data_gxt %>%
  #   select(ID, CENTER, A22DURTN, starts_with("D22DURT"), G22DURTN) %>%
  #   filter(CENTER == 3) %>%
  #   drop_na(D22DURTN)
  
  data_gxt <- data_gxt %>% 
    mutate(
      D22DURTN = if_else(
        condition = CENTER == 3 & is.na(D22DURTN),
        true = D22DURTN_corrected,
        false = D22DURTN
      )
    ) %>% 
    select(-ends_with("corrected")) %>% 
    pivot_longer(cols = -c(ID, CENTER), 
                 names_to = 'temp', 
                 values_to = c('value')) %>% 
    mutate(exam = str_sub(temp, 1, 1),
           temp = str_remove(temp, '^(A|D|G)22'),
           temp = recode(temp, 
                         'DURTN' = 'gxt_duration',
                         'HRS2' = 'gxt_hr_s2',
                         'HRMAX' = 'gxt_hr_max',
                         'RPEMAX' = 'gxt_pe_max')) %>% 
    pivot_wider(names_from = temp, values_from = value) %>% 
    pivot_wider(names_from = exam, 
                values_from = starts_with('gxt'),
                names_sep = '..') %>% 
    mutate(CENTER = recode(CENTER,
                           "1" = "Birmingham", 
                           "2" = "Chicago", 
                           "3" = "Minnesota", 
                           "4" = "Oakland"))
    
  data_out <- data_demo %>% 
    left_join(data_gxt, by = 'ID') %>%
    left_join(data_meds_blockers, by = 'ID') %>% 
    left_join(data_meds_highbp, by = 'ID') %>% 
    left_join(data_educ, by = 'ID') %>% 
    left_join(data_basics, by = 'ID') %>% 
    left_join(data_marital, by = 'ID') %>% 
    left_join(data_bmi, by = 'ID') %>% 
    left_join(data_pa, by = 'ID') %>% 
    left_join(data_health_self, by = 'ID') %>% 
    left_join(data_drinks, by = 'ID') %>% 
    left_join(data_smoke, by = 'ID') %>% 
    relocate(CENTER, .after = ID) %>% 
    mutate(
      exam_age..B = if_else(is.na(exam_age..B), exam_age..A + 2, exam_age..B),
      exam_age..C = if_else(is.na(exam_age..C), exam_age..A + 5, exam_age..C),
      exam_age..D = if_else(is.na(exam_age..D), exam_age..A + 7, exam_age..D),
      exam_age..E = if_else(is.na(exam_age..E), exam_age..A + 10, exam_age..E),
      exam_age..F = if_else(is.na(exam_age..F), exam_age..A + 15, exam_age..F),
      exam_age..G = if_else(is.na(exam_age..G), exam_age..A + 20, exam_age..G),
      hr_max_predicted..A = 179 + 0.29 * exam_age..A - 0.11 * exam_age..A^2,
      hr_max_predicted..D = 179 + 0.29 * exam_age..D - 0.11 * exam_age..D^2,
      hr_max_predicted..G = 179 + 0.29 * exam_age..G - 0.11 * exam_age..G^2,
      gxt_valid..A = gxt_hr_max..A >= 0.85 * hr_max_predicted..A,
      gxt_valid..D = if_else(
        CENTER == 'Minnesota',
        true = gxt_pe_max..D >= 15,
        false = gxt_hr_max..D >= 0.85 * hr_max_predicted..D
      ),
      gxt_valid..G = gxt_hr_max..G >= 0.85 * hr_max_predicted..G,
      .after = gxt_hr_max..G
    )
  
  data_out
  
}

