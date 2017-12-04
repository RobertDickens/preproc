#' Drop columns from a dataframe
#'
#' @param data A dataframe.
#' @param columns vec. The names of the columns to drop
#' @return The original dataframe the specified columns removed.

DropColumns <- function(data, columns) {
  # Drops columns of a dataframe specified by name.

  # Args:
  #   data: a dataframe
  #   columns: vec, vector of column names to drop

  # Returns:
  #   Dataframe with specified columns removed

  for (column in columns) {
    if (!column %in% names(data)) {
      stop(sprintf('"%s" not found in dataframe', column))
    }
  }

  return(data[, !(names(data) %in% columns)])
}
