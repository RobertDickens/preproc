#' Convert temporal variable into dummied indicator variables
#'
#' @param data A dataframe.
#' @param time.column str. The name of the column containing the time variable.
#' @param data.format str. The format of the date.
#' @param dummy.for vec. Vector of strings "m"/"d"/"h" corresponding to "month"/
#'   "day"/"year". What resolution to make the dummy indicators.
#' @param drop bool. If TRUE drops the original column from the dataframe.
#' @return The original dataframe with the dummified time column.

library(lubridate)
library(dummies)

MakeTemporalDummies <- function(data, time.column, date.format,
                                dummy.for = c("m", "d"), drop=TRUE) {

  temporal.col = data[, time.column]

  if ("h" %in% dummy.for) {
    temporal.col <- as_datetime(temporal.col)
  } else {
    temporal.col <- as.Date(temporal.col, format = date.format)
  }

  if (anyNA(temporal.col)) {
    stop("Date not in correct format, NAs produced")
  }

  if ("m" %in% dummy.for) {
    month_dummies <- dummy(month(temporal.col), sep="_month_")
    data <- cbind(data, month_dummies)
  }

  if ("d" %in% dummy.for) {
    day_dummies <- dummy(wday(temporal.col), sep="_day_")
    data <- cbind(data, day_dummies)
  }

  if ("h" %in% dummy.for) {
    hour_dummies <- dummy(hour(temporal.col), sep="_hour_")
    data <- cbind(data, hour_dummies)
  }

  if (drop) {
    data <- data[, !(names(data) %in% time.column)]
  }

  return(data)
}
