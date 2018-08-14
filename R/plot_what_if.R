#' Plot What If Explanations
#'
#' Function 'plot.what_if_explainer' plots What-If Plots for a single prediction / observation.
#'
#' @param x a ceteris paribus explainer produced with the 'what_if' function
#' @param ... other explainers that shall be plotted together
#' @param quantiles if TRUE (default) then quantiles will be presented on OX axis. If FALSE then original values will be presented on OX axis
#' @param split a character, either 'models' or 'variables'. Sets the variable for faceting
#' @param color a character, either 'models' or 'variables'. Sets the variable for coloring
#' @param split_ncol number of columns for the 'facet_wrap'
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @importFrom DALEX theme_mi2
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
#' wi_rf <- what_if(explainer_rf, observation = new_apartment)
#' wi_rf
#'
#' plot(wi_rf, split = "variables", color = "variables")
#' plot(wi_rf)
#' }
plot.what_if_explainer <- function(x, ..., quantiles = TRUE, split = "models", split_ncol = NULL, color = "variables") {
  dfl <- c(list(x), list(...))
  all_responses <- do.call(rbind, dfl)
  class(all_responses) <- "data.frame"

  all_predictions <- lapply(dfl, function(tmp) {
    pred <- attr(tmp, "prediction")
    data.frame(prediction = pred$new_y_hat,
               label = tmp$label[1])
  })
  all_predictions <- do.call(rbind, all_predictions)

  on_x <- y_hat <- label <- vname <- values <- prediction <- NULL
  # what on OX scale
  if (quantiles) {
    all_responses$on_x <- all_responses$relative_quant
    scales_x <- "fixed"
  } else {
    all_responses$on_x <- all_responses$new_x
    scales_x <- "free_x"
  }
  # colors
  if (color == "models") {
    pl <- ggplot(all_responses, aes(on_x, y_hat, color = label))
  } else {
    pl <- ggplot(all_responses, aes(on_x, y_hat, color = vname))
  }
  # do we need faceting?
  if (split == "models") {
    pl <- pl + facet_wrap(~label, scales = scales_x, ncol = split_ncol)
  } else {
    pl <- pl + facet_wrap(~vname, scales = scales_x, ncol = split_ncol)
  }

  pl <- pl +
    geom_hline(data = all_predictions, aes(yintercept = prediction), lty = 2) +
    geom_point() +
    geom_line() +
    theme_mi2() + ylab("Predicted y") + ggtitle("Ceteris Paribus Plot") +
    theme(legend.position = "bottom")

  if (quantiles) {
    pl <- pl +
      geom_vline(xintercept = 0, lty = 2) +
      xlab("Relative percentile") +
      scale_x_continuous(breaks = seq(-1,1,0.2), labels = paste0(seq(-100,100,20),"%"))
  } else {
    numericals <- attr(x, "prediction")$observation[,levels(all_responses$vname), drop = FALSE]
    true_x <- data.frame(vname = colnames(numericals), values = unlist(numericals))
    pl <- pl +
      xlab("") +
      geom_vline(data=true_x, aes(xintercept = values), lty = 2)
  }

  pl
}
