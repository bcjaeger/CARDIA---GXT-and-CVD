#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param exam_letters
make_numeric_exam <- function(exam_letters){
  
  recode(exam_letters,
         "A" = "0",
         "B" = "2",
         "C" = "5",
         "D" = "7",
         "E" = "10",
         "F" = "15",
         "G" = "20",
         "H" = "25",
         "I" = "30") %>% 
    as.numeric()
  
}
