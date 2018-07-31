context("Check local_fit() function")

cr_rf <- local_fit(explainer_rf, observation = new_apartment,
                   select_points = 0.002, selected_variable = "surface")

test_that("Output format - ceteris_paribus_explainer",{
  expect_is(cr_rf, "local_fit_explainer")
})


test_that("Plotting local_fit_explainer",{
  expect_is(plot(cr_rf), "gg")
  expect_is(plot(cr_rf, plot_residuals = FALSE), "gg")
  expect_is(plot(cr_rf, palette = "wangkardu"), "gg")
})
