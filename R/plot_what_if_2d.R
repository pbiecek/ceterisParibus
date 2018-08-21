#' Plot What If 2D Explanations
#'
#' Function 'plot.what_if_2d_explainer' plots What-If Plots for a single prediction / observation.
#'
#' @param x a ceteris paribus explainer produced with the 'what_if_2d' function
#' @param ... currently will be ignored
#' @param split_ncol number of columns for the 'facet_wrap'
#' @param add_raster if TRUE then `geom_raster` will be added to present levels with diverging colors
#' @param add_contour if TRUE then `geom_contour` will be added to present contours
#' @param add_observation if TRUE then `geom_point` will be added to present observation that is explained
#' @param bins number of contours to be added
#'
#' @return a ggplot2 object
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
#' new_apartment <- apartmentsTest[1, ]
#' new_apartment
#'
#' wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment)
#' wi_rf_2d
#'
#' plot(wi_rf_2d)
#' plot(wi_rf_2d, add_contour = FALSE)
#' plot(wi_rf_2d, add_observation = FALSE)
#' plot(wi_rf_2d, add_raster = FALSE)
#'
#' # HR data
#' model <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR)
#' pred1 <- function(m, x)   predict(m, x, type = "prob")[,1]
#' explainer_rf_fired <- explain(model, data = HR[,1:5],
#'    y = HR$status == "fired",
#'    predict_function = pred1, label = "fired")
#'
#' new_emp <- HR[1, ]
#' new_emp
#'
#' wi_rf_2d <- what_if_2d(explainer_rf_fired, observation = new_emp)
#' wi_rf_2d
#'
#' plot(wi_rf_2d)
#' }
plot.what_if_2d_explainer <- function(x, ..., split_ncol = NULL, add_raster = TRUE, add_contour = TRUE, add_observation = TRUE, bins = 3) {
  all_responses <- x
  class(all_responses) <- "data.frame"

  midpoint <- mean(all_responses$y_hat)
  new_x1 <- y_hat <- new_x2 <- NULL

  pred <- attr(x, "prediction")$observation
  tmp <- unique(all_responses[,c("vname1","vname2")])
  observation <- data.frame(tmp,
    new_x1 = unlist(pred[as.character(tmp$vname1)]),
    new_x2 = unlist(pred[as.character(tmp$vname2)]),
    y_hat = midpoint)

  pl <- ggplot(all_responses, aes(new_x1, new_x2, fill = y_hat, z = y_hat)) +
    facet_wrap(vname1 ~ vname2, scales = "free", ncol = split_ncol) +
    xlab("") + ylab("")

  if (add_raster) {
    pl <- pl + geom_raster() +
      scale_fill_gradient2(midpoint = midpoint)
  }

  if (add_contour) {
    pl <- pl + geom_contour(color = "white", alpha = 0.5, bins = bins)
  }

  if (add_observation) {
    pl <- pl + geom_point(data = observation, fill = "black", pch = "+", size = 6)
  }
  pl
}
