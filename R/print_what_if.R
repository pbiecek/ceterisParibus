#' Print What If Explainer Summary
#'
#' @param x a what_if explainer produced with the 'what_if' function
#' @param ... other arguments that will be passed to head()
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
#' }
print.what_if_explainer <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x, ...))
}
