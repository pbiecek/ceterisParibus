#' Local Fit Plots / Wangkardu Explanations
#'
#' Function 'plot.local_fit_explainer' plots Local Fit Plots for a single prediction / observation.
#'
#' @param x a local fir explainer produced with the 'local_fit' function
#' @param ... other explainers that shall be plotted together
#' @param plot_residuals if TRUE (default) then residuals are plotted as red/blue bars
#' @param palette color palette. Currently the choice is limited to 'wangkardu' and 'default'
#'
#' @return a ggplot2 object
#' @export
#' @importFrom stats na.omit
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
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'     select_points = 0.002, selected_variable = "surface")
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'           select_points = 0.002, selected_variable = "surface")
#' plot(cr_rf, plot_residuals = FALSE, palette = "wangkardu")
#' plot(cr_rf, palette = "wangkardu")
#'
#' new_apartment <- apartmentsTest[10, ]
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'           select_points = 0.002, selected_variable = "surface")
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' new_apartment <- apartmentsTest[302, ]
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'           select_points = 0.002, selected_variable = "surface")
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#'
#' new_apartment <- apartmentsTest[720, ]
#' cr_rf <- local_fit(explainer_rf, observation = new_apartment,
#'          select_points = 0.002, selected_variable = "surface")
#' plot(cr_rf, plot_residuals = FALSE)
#' plot(cr_rf)
#' }
plot.local_fit_explainer <- function(x, ..., plot_residuals = TRUE, palette = "default") {
  all_responses <- x
  class(all_responses) <- "data.frame"
  all_predictions <- attr(x, "prediction")

  # Wangkardu palette
  selected_palette <- switch(palette,
                              wangkardu = list(light = "#f5ffea", dark = "#e43d19", medium = "#de8131", up = "#de8131", down = "#de8131", background = "#f6d288", alpha = 0.6),
                              list(light = "black", dark = "black", medium = "#de8131", up = "red3", down = "blue3", background = "white", alpha = 0.1)
  )

  predicted_y <- all_predictions[1, "predictions"]
  predicted_x <- all_predictions[1, "x"]
  vname <- all_responses[1, "vname"]

  # fake variables added because of the CHECK
  new_x <- obs_id <- predictions <- y <- y_hat <- NULL

  pl <- ggplot(na.omit(all_responses), aes(new_x, group = obs_id)) +
      geom_line(aes(y = y_hat), alpha = selected_palette$alpha, color = selected_palette$light) +
      geom_line(aes(y = y_hat), data = all_responses[all_responses$obs_id == 0, ], lwd = 1, color = selected_palette$dark)

  if (plot_residuals) {
    pl <- pl + geom_linerange(data = na.omit(all_predictions), aes(x,  ymin = predictions, ymax = y, color = predictions > y), alpha = 0.5) +
      geom_point(data = na.omit(all_predictions), aes(x, y, color = predictions > y), alpha = 0.5)
  } else {
    pl <- pl +
      geom_point(data = all_predictions, aes(x, predictions), alpha = 0.5, color = selected_palette$dark)
  }
  pl <- pl +
    geom_point(data = all_predictions[1,], aes(x, predictions), size = 6, color = selected_palette$dark) +
    geom_point(data = all_predictions[1,], aes(x, predictions), size = 4, color = selected_palette$background) +
    ylab("Predicted y") + xlab(vname) + theme(legend.position = "none") +
    scale_color_manual(values = c("TRUE" = selected_palette$down, "FALSE" = selected_palette$up))
  pl + if(palette == "wangkardu") theme_wangkardu(selected_palette) else theme_mi2()
}

theme_wangkardu <- function(selected_palette) {
  theme(axis.ticks = element_line(linetype = "blank"),
        axis.text = element_text(family = "sans", color = selected_palette$medium),
        axis.title = element_text(family = "sans", color = selected_palette$medium),
        plot.title = element_text(family = "sans", color = selected_palette$medium),
        legend.text = element_text(family = "sans", color = selected_palette$medium),
        legend.title = element_text(family = "sans", color = selected_palette$medium),
        panel.background = element_rect(fill = selected_palette$background),
        panel.grid.minor.x = element_line(linetype = "dotted", colour = selected_palette$background),
        panel.grid.minor.y = element_line(linetype = "dotted", colour = selected_palette$medium),
        panel.grid.major.x = element_line(linetype = "dotted", colour = selected_palette$background),
        panel.grid.major.y = element_line(linetype = "dotted", colour = selected_palette$medium),
        legend.position = "none",
        plot.background = element_rect(fill = selected_palette$background, colour = selected_palette$background,
                                       size = 0.8, linetype = "dotted"),
        strip.background = element_rect(fill = selected_palette$background),
        strip.text = element_text(family = "sans", color = selected_palette$medium))
}
