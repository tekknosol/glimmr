#' Functions to modify interval between start and end of chamber deployment
#'
#' These functions are used to modify the measurement interval.
#'
#' `trim_time()` increases start of interval to the nearest minute and
#'  decreases end of interval to the nearest minute.
#'
#'
#' @param interval Intervall of chamber deployment. Created with
#'   \code{\link[lubridate]{interval}}.
#'
#' @return Modified interval
#'
#' @examples
#' library(lubridate)
#' start <- ymd_hm("2018-06-25 12:13")
#' end <- ymd_hm("2018-06-25 12:18")
#' int <- interval(start, end)
#'
#' int
#' trim_time(int)
#' @name trimmer
NULL

#' @rdname trimmer
#' @export
trim_time <- function(interval){
  if(missing(interval)){return(NA)}
  lubridate::int_start(interval) <- lubridate::ceiling_date(lubridate::int_start(interval), "minute",
                                                            change_on_boundary = TRUE
  )
  lubridate::int_end(interval) <- lubridate::floor_date(lubridate::int_end(interval), "minute")
  return(interval)
}
