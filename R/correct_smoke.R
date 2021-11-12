#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
correct_smoke <- function(x){
  
  if('never' %in% x & 'current' %in% x){
    index_first_current <- min(which(x=='current'))
    index_nevers <- which(x=='never')
    index_nevers <- index_nevers[index_nevers > index_first_current]
    if(length(index_nevers) > 0) x[index_nevers] <- 'former'
  }
  
  if('former' %in% x & 'never' %in% x){
    index_first_former <- min(which(x=='former'))
    index_nevers <- which(x=='never')
    index_nevers <- index_nevers[index_nevers > index_first_former]
    if(length(index_nevers) > 0) x[index_nevers] <- 'former'
  }
  
  x
  
}
