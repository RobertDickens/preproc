#' Generate one-vs-the-rest correlation metrics for a target variable
#'
#' @param data A dataframe.
#' @param target str. The name of the column to correlate against.
#' @param method str. a character string indicating which correlation coefficient
#'   (or covariance) is to be computed. One of "pearson" (default),
#'   "kendall", or "spearman".
#' @param remove.na.cols if TRUE, remove columns containing NAs.
#' @param verbose if TRUE print names of removed columns.
#' @return The original dataframe the specified columns removed.

CorrelationsTable <- function(data, target, method="pearson",
                              remove.na.cols = FALSE, verbose = TRUE) {

  data <- RemoveZeroVariance(data, verbose = verbose)
  numeric.columns <- sapply(data, is.numeric)
  non.numeric.columns <- names(data)[!numeric.columns]
  na.columns <- names(data)[unlist(lapply(data, function(x) any(anyNA(x))))]

  if (verbose) {
    cat("\nRemoved non numeric columns:", non.numeric.columns, sep="\n")

    if (remove.na.cols) {
      cat("\nRemoved columns containing NAs:", na.columns, sep="\n")
    }
  }

  data <- data[, numeric.columns]

  if (remove.na.cols) {
    data <- DropColumns(data, na.columns)
  }

  correlations <- c()

  for (i in 1:ncol(data)) {
    correlations <- append(correlations, cor(data[, target], data[, i],
                                             method = method))
  }

  correlations <- data.frame("Variable" = names(data),
                             "Correlation" = correlations)

  return(correlations)
}
