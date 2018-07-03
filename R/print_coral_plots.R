#' Prints Coral Plots Explainer Summary
#'
#' @param x a coral_plot explainer produced with the 'coral_plot' function
#' @param ... other arguments that will be passed to head()
#'
#' @export
#'
#' @examples
#' library("DALEX")
#' library("randomForest")
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'                      no.rooms + district, data = apartments)
#' explainer_rf <- explain(apartments_rf_model,
#'                      data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#' new_apartment <- apartmentsTest[1, ]
#' new_apartment

print.coral_plot_explainer <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x, ...))
}
