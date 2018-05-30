#' Plots What-If Explanations
#'
#' Function 'plot.what_if_explainer' plots What-If plots for a single prediction.
#'
#' @param x a what-if explainer produced with the 'what_if' function
#' @param ... other explainers that shall be plotted together
#' @param split a character, either 'models' or 'variables'. Sets the variable for faceting
#' @param color a character, either 'models' or 'variables'. Sets the variable for coloring
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @importFrom DALEX theme_mi2
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
#' wi_rf <- what_if(explainer_rf, observation = new_apartment)
#' wi_rf
#'
#' plot(wi_rf, split = "variables", color = "variables")
#' plot(wi_rf)
plot.what_if_explainer <- function(x, ..., split = "models", color = "variables") {
  dfl <- c(list(x), list(...))
  all_responses <- do.call(rbind, dfl)
  class(all_responses) <- "data.frame"

  all_predictions <- lapply(dfl, function(tmp) {
    pred <- attr(tmp, "prediction")
    data.frame(prediction = pred$new_y_hat,
               label = tmp$label[1])
  })
  all_predictions <- do.call(rbind, all_predictions)

  # do we need faceting?
  relative_quant <- y_hat <- label <- vname <- prediction <- NULL
  if (color == "models") {
    pl <- ggplot(all_responses, aes(relative_quant, y_hat, color = label))
  } else {
    pl <- ggplot(all_responses, aes(relative_quant, y_hat, color = vname))
  }
  if (split == "models") {
    pl <- pl + facet_wrap(~label)
  } else {
    pl <- pl + facet_wrap(~vname)
  }

  pl <- pl +
    geom_vline(xintercept = 0, lty = 2) +
    geom_hline(data = all_predictions, aes(yintercept = prediction), lty = 2) +
    geom_point() +
    geom_line() +
    theme_mi2() + ylab("Predicted y") + xlab("Relative percentile of X_i") + ggtitle("What-If Plot") +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(-1,1,0.2), labels = paste0(seq(-100,100,20),"%"))

  pl
}
