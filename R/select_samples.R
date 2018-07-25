#' Select Subset of Rows
#'
#' This function selects subset of rows from data set.
#' This is usefull if data is large and we need just a sample to calculate profiles.
#'
#' Note that \code{select_subsample} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data set of observations. Profile will be calculated for every observation (every row)
#' @param n named list of vectors. Elements of the list are vectors with points in which profiles should be calculated. See an example for more details.
#' @param seed seed for random number generator.
#'
#' @return a data frame with selected rows
#' @examples
#' library("DALEX")
#' small_apartments <- select_sample(apartmentsTest)
#' head(small_apartments)
#' @export
select_sample <- function(data, n = 100, seed = 1313) {
  UseMethod("select_sample")
}

#' @export
select_sample.default <- function(data, n = 100, seed = 1313) {
  set.seed(seed)
  ids <- sample.int(nrow(data), n, replace = TRUE)
  data[ids,]
}
