#' Local Fit / Wangkardu Explanations
#'
#' @param explainer a model to be explained, preprocessed by the 'DALEX::explain' function
#' @param selected_variable variable to be presented in the local fit plot
#' @param observation a new observarvation for which predictions need to be explained
#' @param grid_points number of points used for response path
#' @param select_points fraction of points fromvalidation data to be presented in local fit plots
#'
#' @return An object of the class 'local_fit_explainer'.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @import gower
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
#' cr_rf
#' }
local_fit <- function(explainer, observation, selected_variable, grid_points = 101, select_points = 0.1) {
  if (!("explainer" %in% class(explainer)))
      stop("The what_if() function requires an object created with explain() function.")
  if (is.null(explainer$data))
    stop("The what_if() function requires explainers created with specified 'data' parameter.")

  data <- explainer$data
  y <- explainer$y
  model <- explainer$model
  predict_function <- explainer$predict_function

  #
  # select common features in observation and data
  vars_to_use <- setdiff(
                    intersect(colnames(observation),
                              colnames(data)),
                    selected_variable
                  )

  distances <- gower_dist(observation[,vars_to_use],
             data[,vars_to_use])
  selected_points <- head(order(distances), ceiling(length(distances) * select_points))
  points_to_plot <- rbind(observation[, c(selected_variable, vars_to_use)],
                         data[selected_points, c(selected_variable, vars_to_use)])

  # calculate predictions
  probs <- seq(0, 1, length.out = grid_points)
  new_x <- quantile(data[,selected_variable], probs = probs)
  new_data <- points_to_plot[rep(1:nrow(points_to_plot), each = grid_points),]
  new_data[, selected_variable] <- new_x
  responses <- data.frame(y_hat = predict_function(model, newdata = new_data),
             new_x = rep(new_x, nrow(points_to_plot)),
             vname = selected_variable,
             quant = rep(probs, nrow(points_to_plot)),
             obs_id = rep(c(0, selected_points), each = grid_points),
             label = explainer$label)

  attr(responses, "prediction") <-
    data.frame(x = points_to_plot[,selected_variable],
         predictions = predict_function(model, newdata = points_to_plot),
         y = c(NA, y[selected_points]),
         obs_id = c(0, selected_points))
  class(responses) = c("local_fit_explainer", "data.frame")
  responses
}
