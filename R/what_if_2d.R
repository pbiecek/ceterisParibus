#' What-If 2D Plot
#'
#' This function calculates what if scores for grid of values spanned by two variables.
#'
#' @param explainer a model to be explained, preprocessed by the 'DALEX::explain' function
#' @param observation a new observarvation for which predictions need to be explained
#' @param grid_points number of points used for response path. Will be used for both variables
#' @param selected_variables if specified, then only these variables will be explained
#'
#' @return An object of the class 'what_if_2d_explainer'.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom utils head
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
#' wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment,
#'          selected_variables = c("surface", "floor", "no.rooms"))
#' wi_rf_2d
#' plot(wi_rf_2d)
#' }
what_if_2d <- function(explainer, observation, grid_points = 101, selected_variables = NULL) {
  if (!("explainer" %in% class(explainer)))
      stop("The what_if() function requires an object created with explain() function.")
  if (is.null(explainer$data))
    stop("The what_if() function requires explainers created with specified 'data' parameter.")

  data <- base::as.data.frame(explainer$data)
  model <- explainer$model
  predict_function <- explainer$predict_function
  var_to_present <- which(sapply(data, is.numeric))
  names_to_present <- colnames(data)[var_to_present]

  if (!is.null(selected_variables)) {
    names_to_present <- intersect(names_to_present, selected_variables)
  }

  # generate all pairs of names_to_present
  v1 <- rep(names_to_present, times = rev(seq_along(names_to_present)) - 1)
  v2 <- unlist(sapply(seq_along(names_to_present), function(x) names_to_present[-(1:x)]))

  responses <- lapply(seq_along(v1), function(i) {
    vname1 <- v1[i]
    vname2 <- v2[i]
    probs <- seq(0, 1, length.out = grid_points)
    new_x1 <- min(data[,vname1]) + probs*diff(range(data[,vname1]))
    new_x2 <- min(data[,vname2]) + probs*diff(range(data[,vname2]))
    new_data <- observation[rep(1, length(new_x1) * length(new_x2)),]
    new_data[,vname1] <- rep(new_x1, each = length(new_x2))
    new_data[,vname2] <- rep(new_x2, times = length(new_x1))
    data.frame(y_hat = predict_function(model, new_data), new_x1 = new_data[,vname1], new_x2 = new_data[,vname2],
               vname1 = vname1, vname2 = vname2, label = explainer$label)
  })

  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation)

  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat)
  class(all_responses) = c("what_if_2d_explainer", "data.frame")
  all_responses
}

