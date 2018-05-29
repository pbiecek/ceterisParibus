#' Prints What-If Explainer Summary
#'
#' @param x a what-if explainer produced with the 'what_if' function
#'
#' @export
#'
#' @examples
print.what_if_explainer <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x, ...))
}
