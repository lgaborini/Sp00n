#' Strip date from POSIXct/POSIXlt, optionally replacing with another one
#'
#' @param x the POSIXt object
#' @param origin NULL, a date-time object, or something which can be coerced by as.POSIXct(tz = "GMT") to such an object.
#' @return if origin is NULL, a difftime object, else a POSIXt object
POSIX_strip <- function(x, origin = NULL) {
   stopifnot(is.POSIXt(x))
   
   diff.start.day <- x - floor_date(x, 'day')
   if (is.null(origin)) {
      diff.start.day
   } else {
      diff.start.day + as.POSIXct(origin)
   }
}