#' What-If Plot
#'
#' @param explainer a model to be explained, preprocessed by the 'DALEX::explain' function
#' @param observation a new observarvation for which predictions need to be explained
#' @param grid_points number of points used for response path
#' @param selected_variables if specified, then only these variables will be explained
#'
#' @return An object of the class 'what_if_explainer'.
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
#' wi_rf <- what_if(explainer_rf, observation = new_apartment)
#' wi_rf
#' wi_rf <- what_if(explainer_rf, observation = new_apartment,
#'          selected_variables = c("surface", "floor", "no.rooms"))
#' wi_rf
#' }
what_if <- function(explainer, observation, grid_points = 101, selected_variables = NULL) {
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

  responses <- lapply(names_to_present, function(vname) {
    probs <- seq(0, 1, length.out = grid_points)
    new_x <- quantile(data[,vname], probs = probs)
    quant_x <- mean(observation[1,vname] >= data[,vname], na.rm = TRUE)
    new_data <- observation[rep(1, grid_points),]
    new_data[,vname] <- new_x
    data.frame(y_hat = predict_function(model, new_data), new_x = new_x,
               vname = vname, x_quant = quant_x, quant = probs,
               relative_quant = probs - quant_x, label = explainer$label)
  })
  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation)

  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat)
  class(all_responses) = c("what_if_explainer", "data.frame")
  all_responses
}
