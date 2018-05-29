#' Plots Interactive What-If Explanations
#'
#' Function 'plot.what_if_explainer' plots What-If plots for a single prediction.
#'
#' @param x a what-if explainer produced with the 'what_if' function
#' @param ... other explainers that shall be plotted together
#' @param split a character, either 'models' or 'variables'. Sets the vaiable for faceting
#'
#' @return a ggiraph object
#' @name plot_interactive
#' @export
#' @import ggiraph
#'
#' @examples
plot_interactive.what_if_explainer <- function(x, ..., split = "models") {
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
  if (length(dfl) > 1) {
    if (split == "models") {
      pl <- ggplot(all_responses, aes(relative_quant, y_hat, color = vname,
                          tooltip = paste(vname, "=", new_x, " -> ", round(y_hat)))) +
        facet_wrap(~label, ncol = 1)
    } else {
      pl <- ggplot(all_responses, aes(relative_quant, y_hat, color = label,
                          tooltip = paste(vname, "=", new_x, " -> ", round(y_hat)))) +
        facet_wrap(~vname)
    }
  } else {
    pl <- ggplot(all_responses, aes(relative_quant, y_hat, color = vname,
                          tooltip = paste(vname, "=", new_x, " -> ", round(y_hat))))
  }

  pl <- pl +
    geom_vline(xintercept = 0, lty = 2) +
    geom_hline(data = all_predictions, aes(yintercept = prediction), lty = 2) +
    geom_point_interactive() +
    geom_line_interactive() +
    theme_mi2() + ylab("Predicted y") + xlab("X percentile") + ggtitle("1D 'What If' Plot") +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(-1,1,0.1), labels=scales::percent)

  ggiraph(code = print(pl), hover_css = "fill-opacity:.3;cursor:pointer;")
}

#' @name plot_interactive
#' @export
plot_interactive <- function (x, ...) {
  UseMethod("plot_interactive", x)
}

#' @name plot_interactive
#' @export
plot_interactive.default <- plot_interactive.what_if_explainer
