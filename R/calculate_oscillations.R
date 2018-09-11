#' Calculate Oscillations for Ceteris Paribus Explainer
#'
#' @param x a ceteris_paribus explainer produced with the `ceteris_paribus()` function
#' @param sort a logical value. If TRUE then rows are sorted along the oscillations
#' @param ... other arguments
#'
#' @export
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'       no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'       data = apartmentsTest, y = apartmentsTest$m2.price)
#'
#' apartment <- apartmentsTest[1,]
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartment)
#' calculate_oscillations(cp_rf)
#' }
calculate_oscillations <- function(x, sort = TRUE, ...) {
  stopifnot("ceteris_paribus_explainer" %in% class(x))

  observations <- attr(x, "observations")
  variables <- unique(as.character(x$`_vname_`))
  ids <- unique(as.character(x$`_ids_`))
  lapply(variables, function(variable) {
    lapply(ids, function(id) {
      diffs <- x[x$`_vname_` == variable & x$`_ids_` == id,"_yhat_"] - observations[id,"_yhat_"]
      data.frame(`_vname_` = variable, `_ids_` = id, oscillations = mean(abs(diffs)))
    }) -> tmp
    do.call(rbind, tmp)
  }) -> tmp
  res <- do.call(rbind, tmp)
  colnames(res) <- c("_vname_", "_ids_", "oscillations")
  if (sort) {
    res <- res[order(res$oscillations, decreasing = TRUE),]
  }
  class(res) = c("ceteris_paribus_oscillations", "data.frame")
  res
}

