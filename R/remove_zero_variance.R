#' Remove zero variance columns from a dataframe.
#'
#' @param data A dataframe or matrix
#' @param verbose If TRUE, prints names of removed variables
#' @return A dataframe with the zero variance columns removed


RemoveZeroVariance <- function(data, verbose = TRUE) {

  data.initial <- as.data.frame(data)
  variances <- unlist(lapply(df, function(x) length(unique(x))))
  removed.variables <- (names(which(variances == 1)))
  data.new <- data.initial[, !(names(data.initial) %in% removed.variables)]

  if (verbose)
    cat("Removed columns with zero variance:", removed.variables, sep="\n")

  return(data.new)
}
