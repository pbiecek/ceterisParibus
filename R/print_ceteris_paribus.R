#' Print Ceteris Paribus Explainer Summary
#'
#' @param x a ceteris_paribus explainer produced with the `ceteris_paribus()` function
#' @param ... other arguments that will be passed to `head()`
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
#'       data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' apartments_small <- select_sample(apartmentsTest, 10)
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartments_small)
#' cp_rf
#' }
print.ceteris_paribus_explainer <- function(x, ...) {
  cat("Top profiles    : \n")
  class(x) <- "data.frame"
  print(head(x, ...))

  cat("\n\nTop observations:\n")
  print(head(as.data.frame(attr(x, "observations")), ...))
  return(invisible(NULL))
}
