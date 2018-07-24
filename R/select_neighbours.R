#' Select Subset of Rows Closest to a Specified Observation
#'
#' This function selects subset of rows from data set.
#' This is usefull if data is large and we need just a sample to calculate profiles.
#'
#' Note that \code{select_neighbours} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data set of observations
#' @param observation single observation
#' @param variables variables that shall be used for calculation of distance. By default these are all variables present in `data` and `observation`
#' @param distance distance function, by default the `gower_dist` function.
#' @param n number of neighbours to select
#' @param frac if `n` is not specified (NULL), then will be calculated as `frac` * number of rows in `data`. Either `n` or `frac` need to be specified.
#'
#' @return a data frame with selected rows
#' @examples
#' library("DALEX")
#'
#' new_apartment <- apartments[1, 2:6]
#' small_apartments <- select_neighbours(apartmentsTest, new_apartment, n = 10)
#' new_apartment
#' small_apartments
#' @export
select_neighbours <- function(data, observation, variables = NULL, distance = gower::gower_dist, n = 20, frac = NULL) {
  UseMethod("select_neighbours")
}

#' @export
select_neighbours.default <- function(data, observation, variables = NULL, distance = gower::gower_dist, n = 20, frac = NULL) {
 if (is.null(variables)) {
   variables <- intersect(colnames(observation),
                          colnames(data))
 }
 if (is.null(n)) {
   n <- ceiling(nrow(data)*frac)
 }

  distances <- distance(observation[,variables],
                          data[,variables])
  selected_points <- head(order(distances), n)
  data[selected_points, ]
}

