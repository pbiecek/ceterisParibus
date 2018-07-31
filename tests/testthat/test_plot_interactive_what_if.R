context("Check what_if() function")

library("ggiraph")

wi_rf_all <- what_if(explainer_rf, observation = new_apartment)

test_that("output",{
  expect_is(plot_interactive(wi_rf_all), "ggiraph")
  expect_is(plot_interactive(wi_rf_all, split = "variables", color = "variable"), "ggiraph")
})
