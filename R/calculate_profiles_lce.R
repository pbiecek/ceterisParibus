#' Calculate Local Conditional Expectation profiles
#'
#' This function Local Conditional Expectation profiles
#'
#' Note that \code{calculate_profiles_lce} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data set of observations. Profile will be calculated for every observation (every row)
#' @param variable_splits named list of vectors. Elements of the list are vectors with points in which profiles should be calculated. See an example for more details.
#' @param predict_function function that takes data and model and returns numeric predictions. Note that the ... arguments will be passed to this function.
#' @param model a model that will be passed to the \code{predict_function}
#' @param dataset a data.frame, usually training data of a model, used for calculation of LCE profiles
#' @param ... other parameters that will be passed to the \code{predict_function}
#'
#' @importFrom stats model.matrix
#' @return a data frame with profiles for selected variables and selected observations
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("randomForest")
#' set.seed(59)
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'                                       no.rooms + district, data = apartments)
#' explainer_rf <- explain(apartments_rf_model,
#'       data = apartments[,2:6], y = apartments$m2.price)
#' vars <- c("construction.year", "surface", "floor", "no.rooms", "district")
#' variable_splits <- calculate_variable_splits(apartments, vars)
#' new_apartment <- apartments[1, ]
#'
#' profiles <- calculate_profiles_lce(new_apartment, variable_splits,
#'                                apartments_rf_model, explainer_rf$data)
#' profiles
#' }
#' @export
calculate_profiles_lce <- function(data, variable_splits, model, dataset, predict_function = predict, ...) {
  UseMethod("calculate_profiles_lce")
}
calculate_profiles_lce.default <- function(data, variable_splits, model, dataset, predict_function = predict, ...) {
  variables <- names(variable_splits)
  profiles <- lapply(variables, function(variable) {
    split_points <- variable_splits[[variable]]

    # remember ids of selected points
    if (is.null(rownames(data))) {
      ids <- rep(1:nrow(data), each = length(split_points))
    } else {
      ids <- rep(rownames(data), each = length(split_points))
    }

    new_data <- data[rep(1:nrow(data), each = length(split_points)),]
    new_data[, variable] <- rep(split_points, nrow(data))

    # fitting linear model pairwise between 'variable' and every other feature variable
    for (feature_variable in setdiff(variables, variable)) {
      formula <- formula(paste(feature_variable, " ~ ", variable))
      if(is.numeric(dataset[, feature_variable])) {
        lm <- lm(formula, data = dataset)
        alpha <- lm$coefficients[-1]
        if(is.numeric(data[, variable])) {
          shifted_intercepts_tmp <-
            data[, feature_variable] - model.matrix( ~ data[, variable])[,-1] * lm$coefficients[-1]
        } else {
          shifted_intercepts_tmp <-
            data[, feature_variable] - t( model.matrix( ~ data[, variable])[,-1] %*% lm$coefficients[-1] )
        }
        shifted_intercepts <- shifted_intercepts_tmp
        names(shifted_intercepts) <- unique(data[, feature_variable])
        #new_data[, feature_variable] <- alpha*new_data[, variable] + shifted_intercepts
        X <- as.matrix( model.matrix(~new_data[, variable])[, -1] )
        for (i in 1:nrow(new_data)) {
          x <- X[i, ]
          new_data[i, feature_variable] <- alpha %*% x + shifted_intercepts[paste(new_data[i, feature_variable])]
        }
      }
    }


    yhat <- predict_function(model, new_data, ...)
    new_data <- cbind(new_data,
                      `_yhat_` = yhat,
                      `_vname_` = variable,
                      `_ids_` = ids)
    new_data
  })

  profile <- do.call(rbind, profiles)
  class(profile) <- c("ceteris_paribus_profile", class(profile))
  profile
}
