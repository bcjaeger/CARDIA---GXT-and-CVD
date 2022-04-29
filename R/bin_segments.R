
#' Create bins for histogram
#'
#' @param x
#' @param y
#' @param x_min
#' @param x_max
#' @param bin_count
#' @param bin_yintercept
#' @param bin_length
#'
#' @return
#' @export
#'
#' @examples
#'

bin_segments <- function(x,
                         x_min = NULL,
                         x_max = NULL,
                         y = NULL,
                         by_y = FALSE,
                         bin_count,
                         bin_yintercept,
                         bin_length){

 bins <- seq(from = if(is.null(x_min)) min(x) else x_min,
             to   = if(is.null(x_max)) max(x) else x_max,
             length.out = bin_count + 1)

 if(!by_y){

  freqs_orig <- table(cut(x, bins))

  freqs <- 0.1 * freqs_orig / max(freqs_orig)

  return(
   data.frame(
    x = bins[-(bin_count + 1)],
    y = bin_yintercept,
    xend = bins[-(bin_count + 1)],
    yend = as.numeric(bin_length * freqs + bin_yintercept),
    count = as.numeric(freqs)
   )
  )

 }

 if(is.null(y)) stop("y must be specified if by_y = TRUE")

 # separate frequency counts by event status
 f0	<- table(cut(x[y == 0], bins))
 f1	<- table(cut(x[y == 1], bins))

 j0	<- f0 > 0
 j1	<- f1 > 0

 bins0 <- (bins[-(bin_count + 1)])[j0]
 bins1 <- (bins[-(bin_count + 1)])[j1]

 f0	<- f0[j0]
 f1	<- f1[j1]

 maxf <- max(f0, f1)

 freqs_below <- f0
 freqs_above <- f1

 f0	<- (0.1 * f0) / maxf
 f1	<- (0.1 * f1) / maxf

 data_segments_below <- data.frame(
  x = bins0,
  y = bin_yintercept,
  xend = bins0,
  yend = as.numeric(-1 * bin_length * f0 + bin_yintercept),
  event_status = 0,
  count = as.numeric(freqs_below)
 )

 data_segments_above <- data.frame(
  x = bins1,
  y = bin_yintercept,
  xend = bins1,
  yend = as.numeric(bin_length * f1 + bin_yintercept),
  event_status = 1,
  count = as.numeric(freqs_above)
 )

 rbind(data_segments_above, data_segments_below)

}
