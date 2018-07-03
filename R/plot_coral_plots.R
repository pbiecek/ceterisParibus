#' Plots Coral Explanations
#'
#' Function 'plot.coral_explainer_explainer' plots Coral Plots for a single prediction / observation.
#'
#' @param x a ceteris paribus explainer produced with the 'ceteris_paribus' function
#' @param ... other explainers that shall be plotted together
#' @param plot_residuals if TRUE (default) then residuals are plotted as red/blue bars
#'
#' @return a ggplot2 object
#' @export
#' @importFrom stats na.omit
#'
#' @examples
#' library("DALEX")
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
#' cr_rf <- coral_plots(explainer_rf, observation = new_apartment, select_points = 0.002)
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' new_apartment <- apartmentsTest[10, ]
#' cr_rf <- coral_plots(explainer_rf, observation = new_apartment, select_points = 0.002)
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' new_apartment <- apartmentsTest[302, ]
#' cr_rf <- coral_plots(explainer_rf, observation = new_apartment, select_points = 0.002)
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' new_apartment <- apartmentsTest[720, ]
#' cr_rf <- coral_plots(explainer_rf, observation = new_apartment, select_points = 0.002)
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
plot.coral_explainer <- function(x, ..., plot_residuals = TRUE) {
  all_responses <- x
  class(all_responses) <- "data.frame"
  all_predictions <- attr(x, "prediction")

  predicted_y <- all_predictions[1,"predictions"]
  predicted_x <- all_predictions[1,"x"]
  vname <- all_responses[1,"vname"]

  # because of checks
  new_x <- obs_id <- predictions <- y <- y_hat <- NULL

  pl <- ggplot(na.omit(all_responses), aes(new_x, group = obs_id)) +
      geom_line(aes(y = y_hat), alpha = 0.1) +
      geom_line(aes(y = y_hat), data = all_responses[all_responses$obs_id == 0, ], lwd = 1)

  if (plot_residuals) {
    pl <- pl + geom_linerange(data = na.omit(all_predictions), aes(x,  ymin = predictions, ymax = y, color = predictions > y), alpha = 0.5) +
      geom_point(data = na.omit(all_predictions), aes(x, y, color = predictions > y), alpha = 0.5)
  } else {
    pl <- pl +
      geom_point(data = all_predictions, aes(x, predictions), alpha = 0.5)
  }
  pl <- pl +
    geom_point(data = all_predictions[1,], aes(x, predictions), size = 5) +
    theme_mi2() + ylab("Predicted y") + xlab(vname) + theme(legend.position = "none") +
    scale_color_manual(values = c("TRUE" = "blue3", "FALSE" = "red3"))
  pl
}
