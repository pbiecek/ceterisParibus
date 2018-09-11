#' Plot Ceteris Paribus Explanations
#'
#' Function 'plot.ceteris_paribus_explainer' plots Ceteris Paribus Plots for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param color_points a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_points a numeric. Size of points to be plotted
#' @param alpha_points a numeric between 0 and 1. Opacity of points
#' @param color_rugs a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_rugs a numeric. Size of rugs to be plotted
#' @param alpha_rugs a numeric between 0 and 1. Opacity of rugs
#' @param color_residuals a character. Either name of a color or name of a variable that should be used for coloring for residuals
#' @param size_residuals a numeric. Size of line and points to be plotted for residuals
#' @param alpha_residuals a numeric between 0 and 1. Opacity of points and lines for residuals
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#' @param show_profiles a logical. If TRUE then profiles will be plotted. Either individual or aggregate (see `aggregate_profiles`)
#' @param aggregate_profiles function. If NULL (default) then individual profiles will be plotted. If a function (e.g. mean or median) then profiles will be aggregated and only the aggregate profile will be plotted
#' @param show_observations a logical. If TRUE then individual observations will be marked as points
#' @param show_rugs a logical. If TRUE then individual observations will be marked as rugs
#' @param show_residuals a logical. If TRUE then residuals will be plotted as a line ended with a point
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#' @param as.gg if TRUE then returning plot will have gg class
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @importFrom DALEX theme_mi2
#' @importFrom stats aggregate
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
#' apartments_small <- apartmentsTest[1:20,]
#' apartments_small_1 <- apartmentsTest[1,]
#' apartments_small_2 <- select_sample(apartmentsTest, n = 20)
#' apartments_small_3 <- select_neighbours(apartmentsTest, apartments_small_1, n = 20)
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartments_small)
#' cp_rf_1 <- ceteris_paribus(explainer_rf, apartments_small_1)
#' cp_rf_2 <- ceteris_paribus(explainer_rf, apartments_small_2)
#' cp_rf_3 <- ceteris_paribus(explainer_rf, apartments_small_3)
#' cp_rf
#'
#' cp_rf_y <- ceteris_paribus(explainer_rf, apartments_small, y = apartments_small$m2.price)
#' cp_rf_y1 <- ceteris_paribus(explainer_rf, apartments_small_1, y = apartments_small_1$m2.price)
#' cp_rf_y2 <- ceteris_paribus(explainer_rf, apartments_small_2, y = apartments_small_2$m2.price)
#' cp_rf_y3 <- ceteris_paribus(explainer_rf, apartments_small_3, y = apartments_small_3$m2.price)
#'
#' plot(cp_rf_y, show_profiles = TRUE, show_observations = TRUE,
#'                show_residuals = TRUE, color = "black",
#'                alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
#'                size_points = 2, size_rugs = 0.5)
#'
#' plot(cp_rf_y, show_profiles = TRUE, show_observations = TRUE,
#'                show_residuals = TRUE, color = "black",
#'                selected_variables = c("construction.year", "surface"),
#'                alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
#'                size_points = 2, size_rugs = 0.5)
#'
#' plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#'                show_residuals = TRUE, alpha = 0.5, size_points = 3,
#'                alpha_points = 1, size_rugs = 0.5)
#'
#' plot(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#'                alpha = 0.2, alpha_points = 1, size_rugs = 0.5)
#'
#' plot(cp_rf_y3, show_profiles = TRUE, show_rugs = TRUE,
#'                show_residuals = TRUE, alpha = 0.2, color_residuals = "orange", size_rugs = 0.5)
#'
#' plot(cp_rf_y, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE, size_rugs = 0.5,
#'                show_residuals = TRUE, alpha = 0.5, color = "surface", as.gg = TRUE) +
#'                scale_color_gradient(low = "darkblue", high = "darkred")
#'
#' plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#'                show_residuals = TRUE, alpha = 0.5, color = "surface", size_points = 3)
#'
#' plot(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE, show_rugs = TRUE,
#'                size = 0.5, alpha = 0.5, color = "surface")
#'
#' plot(cp_rf_y, show_profiles = TRUE, show_rugs = TRUE, size_rugs = 0.5,
#'                show_residuals = FALSE, aggregate_profiles = mean, color = "darkblue")
#' }
plot.ceteris_paribus_explainer <- function(x, ...,
   size = 1,
   alpha = 0.3,
   color = "black",
   size_points = 2,
   alpha_points = 1,
   color_points = color,
   size_rugs = 0.5,
   alpha_rugs = 1,
   color_rugs = color,
   size_residuals = 1,
   alpha_residuals = 1,
   color_residuals = color,

   only_numerical = TRUE,

   show_profiles = TRUE,
   show_observations = TRUE,
   show_rugs = FALSE,
   show_residuals = FALSE,
   aggregate_profiles = NULL,
   as.gg = FALSE,
   facet_ncol = NULL, selected_variables = NULL) {
  ceteris_paribus_layer(x = x, ...,
      size = size, alpha = alpha, color = color,
      size_points = size_points, alpha_points = alpha_points, color_points = color_points,
      size_rugs = size_rugs, alpha_rugs = alpha_rugs, color_rugs = color_rugs,
      size_residuals = size_residuals, alpha_residuals = alpha_residuals, color_residuals = color_residuals,
      only_numerical = only_numerical,
      show_profiles = show_profiles, show_observations = show_observations, show_rugs = show_rugs, show_residuals = show_residuals,
      aggregate_profiles = aggregate_profiles,
      facet_ncol = facet_ncol, selected_variables = selected_variables,
      init_plot = TRUE, as.gg = as.gg)(NULL)
}

#' Add Layer to the Ceteris Paribus Plot
#'
#' Function `ceteris_paribus_layer()` adds a layer to a plot created with `plot.ceteris_paribus_explainer()` plots.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param color_points a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_points a numeric. Size of points to be plotted
#' @param alpha_points a numeric between 0 and 1. Opacity of points
#' @param color_rugs a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_rugs a numeric. Size of rugs to be plotted
#' @param alpha_rugs a numeric between 0 and 1. Opacity of rugs
#' @param color_residuals a character. Either name of a color or name of a variable that should be used for coloring for residuals
#' @param size_residuals a numeric. Size of line and points to be plotted for residuals
#' @param alpha_residuals a numeric between 0 and 1. Opacity of points and lines for residuals
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#' @param show_profiles a logical. If TRUE then profiles will be plotted. Either individual or aggregate (see `aggregate_profiles`)
#' @param aggregate_profiles function. If NULL (default) then individual profiles will be plotted. If a function (e.g. mean or median) then profiles will be aggregated and only the aggregate profile will be plotted
#' @param show_observations a logical. If TRUE then individual observations will be marked as points
#' @param show_rugs a logical. If TRUE then individual observations will be marked as rugs
#' @param show_residuals a logical. If TRUE then residuals will be plotted as a line ended with a point
#' @param facet_ncol number of columns for the `facet_wrap()`.
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#' @param init_plot technical parameter, do not use.
#' @param as.gg if TRUE then returning plot will have gg class
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
#' apartments_small_1 <- apartmentsTest[1,]
#' apartments_small_2 <- select_sample(apartmentsTest, n = 20)
#' apartments_small_3 <- select_neighbours(apartmentsTest, apartments_small_1, n = 20)
#'
#' cp_rf_y1 <- ceteris_paribus(explainer_rf, apartments_small_1, y = apartments_small_1$m2.price)
#' cp_rf_y2 <- ceteris_paribus(explainer_rf, apartments_small_2, y = apartments_small_2$m2.price)
#' cp_rf_y3 <- ceteris_paribus(explainer_rf, apartments_small_3, y = apartments_small_3$m2.price)
#'
#' tmp <- plot(cp_rf_y3, show_profiles = TRUE, show_observations = TRUE,
#'                show_residuals = TRUE, color = "black",
#'                alpha = 0.2, color_residuals = "darkred",
#'                selected_variables = c("construction.year", "surface"))
#'
#' tmp <- plot(cp_rf_y3, show_profiles = TRUE, show_observations = TRUE,
#'                show_residuals = TRUE, color = "black",
#'                alpha = 0.2, color_residuals = "darkred")
#'
#' tmp
#'
#' tmp +
#'  ceteris_paribus_layer(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE,
#'                alpha = 0.2, color = "darkblue")
#'
#' tmp +
#'   ceteris_paribus_layer(cp_rf_y2, show_profiles = TRUE, show_observations = TRUE,
#'                alpha = 0.2, color = "darkblue") +
#'   ceteris_paribus_layer(cp_rf_y2, show_profiles = TRUE, show_observations = FALSE,
#'                alpha = 1, size = 2, color = "blue", aggregate_profiles = mean) +
#'   ceteris_paribus_layer(cp_rf_y1, show_profiles = TRUE, show_observations = FALSE,
#'                alpha = 1, size = 2, color = "red", aggregate_profiles = mean)
#' }
ceteris_paribus_layer <- function(x, ...,
      size = 1,
      alpha = 0.3,
      color = "black",
      size_points = 2,
      alpha_points = 1,
      color_points = color,
      size_rugs = 0.5,
      alpha_rugs = 1,
      color_rugs = color,
      size_residuals = 1,
      alpha_residuals = 1,
      color_residuals = color,

       only_numerical = TRUE,

       show_profiles = TRUE,
       show_observations = TRUE,
       show_rugs = FALSE,
       show_residuals = FALSE,
       aggregate_profiles = NULL,
       as.gg = FALSE,
      facet_ncol = NULL, selected_variables = NULL, init_plot = FALSE) {

  function(pl) {
    # if there is more explainers, they should be merged into a single data frame
    dfl <- c(list(x), list(...))
    all_profiles <- do.call(rbind, dfl)
    class(all_profiles) <- "data.frame"

    all_observations <- lapply(dfl, function(tmp) {
      attr(tmp, "observations")
    })
    all_observations <- do.call(rbind, all_observations)
    all_observations$`_ids_` <- factor(rownames(all_observations))
    all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

    # variables to use
    all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
    if (!is.null(selected_variables)) {
      all_variables <- intersect(all_variables, selected_variables)
      if (length(all_variables) == 0) stop(paste0("selected_variables do not overlap with ", paste(all_variables, collapse = ", ")))
    }
    # is color a variable or literal?
    is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
    # only numerical or only factors?
    is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
    if (only_numerical) {
      vnames <- names(which(is_numeric))
      if (length(vnames) == 0) stop("There are no numerical variables")
      all_profiles$`_x_` <- 0
    } else {
      vnames <- names(which(!is_numeric))
      if (length(vnames) == 0) stop("There are no non-numerical variables")
      all_profiles$`_x_` <- ""
    }
    # select only suitable variables
    all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
    # create _x_
    tmp <- as.character(all_profiles$`_vname_`)
    for (i in seq_along(tmp)) {
      all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
    }

    # prepare plot
    `_x_` <- `_y_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
    if (init_plot) {
      pl <- ggplot(all_profiles, aes(`_x_`, `_yhat_`, group = paste(`_ids_`, `_label_`))) +
        facet_wrap(~ `_vname_`, scales = "free_x", ncol = facet_ncol)
    }

    # show profiles without aggregation
    if (show_profiles & is.null(aggregate_profiles)) {
      if (is_color_a_variable) {
        pl <- pl + geom_line(data = all_profiles, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha)
      } else {
        pl <- pl + geom_line(data = all_profiles, size = size, alpha = alpha, color = color)
      }
    }

    # prepare data for plotting points
    if (show_observations | show_rugs | show_residuals) {
      is_color_points_a_variable    <- color_points %in% c(all_variables, "_label_", "_vname_", "_ids_")
      is_color_rugs_a_variable      <- color_rugs %in% c(all_variables, "_label_", "_vname_", "_ids_")
      is_color_residuals_a_variable <- color_residuals %in% c(all_variables, "_label_", "_vname_", "_ids_")

      tmp <- lapply(vnames, function(var) {
        data.frame(`_x_` = all_observations[,var],
                   `_vname_` = var,
                   `_yhat_`  = all_observations$`_yhat_`,
                   `_y_`     = if (is.null(all_observations$`_y_`)) NA else all_observations$`_y_`,
                   `_color_` = if (!is_color_points_a_variable) NA else {
                     if (color_points == "_vname_") var else all_observations[,color_points]
                   },
                   `_ids_`   = all_observations$`_ids_`,
                   `_label_`  = all_observations$`_label_`)
      })
      all_observations_long <- do.call(rbind, tmp)
      colnames(all_observations_long) <- c("_x_", "_vname_", "_yhat_", "_y_", "_color_", "_ids_", "_label_")
      if ((is_color_points_a_variable | is_color_rugs_a_variable) & !(color_points %in% colnames(all_observations_long))) colnames(all_observations_long)[5] = color_points

      # show observations
      if (show_observations) {
        if (is_color_points_a_variable) {
          pl <- pl + geom_point(data = all_observations_long, aes_string(color = paste0("`",color_points,"`")), size = size_points, alpha = alpha_points)
        } else {
          pl <- pl + geom_point(data = all_observations_long, size = size_points, alpha = alpha_points, color = color_points)
        }
      }

      # show rugs
      if (show_rugs) {
        if (is_color_rugs_a_variable) {
          pl <- pl + geom_rug(data = all_observations_long, aes_string(color = paste0("`",color_rugs,"`")), size = size_rugs, alpha = alpha_rugs)
        } else {
          pl <- pl + geom_rug(data = all_observations_long, size = size_rugs, alpha = alpha_rugs, color = color_rugs)
        }
      }

      if (show_residuals) {
        if (is_color_residuals_a_variable) {
          pl <- pl + geom_linerange(data = all_observations_long, aes_string(ymin = "`_y_`", ymax = "`_yhat_`", color = paste0("`",color_residuals,"`")), size = size_residuals, alpha = alpha_residuals) +
            geom_point(data = all_observations_long, aes_string(y = "`_y_`", color = paste0("`",color_residuals,"`")), size = size_residuals, alpha = alpha_residuals)
        } else {
          pl <- pl + geom_linerange(data = all_observations_long, aes_string(ymin = "`_y_`", ymax = "`_yhat_`"), size = size, alpha = alpha_residuals, color = color_residuals) +
            geom_point(data = all_observations_long, aes(y = `_y_`), size = size_residuals, alpha = alpha_residuals, color = color_residuals)
        }
      }
    }

    # show profiles with aggregation
    if (show_profiles & !is.null(aggregate_profiles)) {
      tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_")]
      aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`), FUN = aggregate_profiles)
      colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_yhat_")
      aggregated_profiles$`_ids_` <- 0

      if (is_color_a_variable) {
        pl <- pl + geom_line(data = aggregated_profiles, aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
      } else {
        pl <- pl + geom_line(data = aggregated_profiles, aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
      }
    }

    if (init_plot) {
      pl <- pl + theme_mi2()
    }

    if (!as.gg) {
      class(pl) <- c("plot_ceteris_paribus_explainer", class(pl))
    }
    pl
  }
}

#' Add More Layers to a Ceteris Paribus Plot
#'
#' @param e1 An object of class `plot_ceteris_paribus_explainer`.
#' @param e2 A plot component
#' @export
#' @method + plot_ceteris_paribus_explainer
"+.plot_ceteris_paribus_explainer" <- function(e1, e2) {
  if (missing(e2)) {
    stop("Cannot use `+.plot_ceteris_paribus_explainer()` with a single argument. ")
  }

  class(e1) <- class(e1)[-1]
  if (class(e2) == "function") { # working as plot_ceteris_paribus_explainer
    tmp <- e2(e1)
  } else { # should act as a +.gg
    tmp <- e1 + e2
  }
  tmp
}

#' Print Ceteris Paribus Explainer Summary
#'
#' See more examples in the \code{ceteris_paribus_layer} function
#'
#' @param x a plot_ceteris_paribus_explainer object to plot
#' @param ... other arguments that will be passed to `print.ggplot()`
#' @export
"print.plot_ceteris_paribus_explainer" <- function(x, ...) {
  class(x) <- class(x)[-1]
  print(x, ...)
}
