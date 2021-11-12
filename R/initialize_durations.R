
# mutate(exam = str_sub(exam, start = 1, end = 1),
#        exam_numeric = make_numeric_exam(exam_letters = exam)) %>%
#   group_by(ID) %>%
#   mutate(time_diff = lead(exam_numeric) - exam_numeric) %>%
#   ungroup() %>%
#   initialize_durations(col = 'paying_for_basics')

initialize_durations <- function(data, col){


  col_distinct_vals <- pull(drop_na(distinct(data[,col])), 1)

  new_cols <- paste('duration', col, col_distinct_vals, sep = '_')

  input_list <- vector(mode = 'list', length = length(new_cols))
  names(input_list) <- new_cols

  for(i in seq_along(new_cols)){

    input_list[[i]] <- if_else(data[[col]] == col_distinct_vals[i],
                               true = data$time_diff,
                               false = 0)



  }

  tibble::add_column(data, !!!input_list, .after = col)

}
