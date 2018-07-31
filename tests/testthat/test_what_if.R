context("Check what_if() function")

wi_rf <- what_if(explainer_rf, observation = new_apartment, selected_variables = "construction.year")
wi_rf_all <- what_if(explainer_rf, observation = new_apartment)
wi_lm <- what_if(explainer_lm, observation = new_apartment, selected_variables = "construction.year")

test_that("Wrong input",{
  expect_error(what_if(apartments_rf_model))
})


test_that("Output format - what_if_explainer",{
  expect_is(wi_rf, "what_if_explainer")
})


test_that("Plotting what_if_explainer",{
  expect_is(plot(wi_rf_all, quantiles = FALSE), "gg")
  expect_is(plot(wi_rf, wi_lm, color = "models", split = "variable"), "gg")
})
