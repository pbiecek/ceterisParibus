#' Prints Local Fit / Wangkardu Summary
#'
#' @param x a local fit explainer produced with the 'local_fit' function
#' @param ... other arguments that will be passed to 'head' function
#'
#' @export
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("randomForest")
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'                      no.rooms + district, data = apartments)
#' explainer_rf <- explain(apartments_rf_model,
#'                      data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#' new_apartment <- apartmentsTest[1, ]
#' new_apartment
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'     select_points = 0.002, selected_variable = "surface")
#' cr_rf
#' }
print.local_fit_explainer <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x, ...))
}
