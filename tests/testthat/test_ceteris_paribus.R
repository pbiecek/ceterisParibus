context("Check ceteris_paribus() function")

cp_rf <- ceteris_paribus(explainer_rf, apartments_A, y = apartments_A$m2.price)
cp_rf_cy <- ceteris_paribus(explainer_rf, apartments_A, y = apartments_A$m2.price, variables = "construction.year")


test_that("Wrong input",{
  expect_error(ceteris_paribus(apartments_rf_model))
})


test_that("Output format - ceteris_paribus_explainer",{
  expect_is(cp_rf, "ceteris_paribus_explainer")
})


test_that("Plotting ceteris_paribus_explainer",{
  expect_is(plot(cp_rf), "gg")
  expect_is(plot(cp_rf, only_numerical = FALSE, int_plot = TRUE, show_rugs = TRUE, show_observations = TRUE,
                 show_profiles = FALSE, show_residuals = TRUE, aggregate_profiles = mean), "gg")
  expect_is(plot(cp_rf, show_profiles = TRUE, aggregate_profiles = mean), "gg")
})


test_that("Add layers to Ceteris Paribus Plot",{
  expect_is(plot(cp_rf) +
              ceteris_paribus_layer(cp_rf, selected_variables = c("surface","construction.year")) ,
            "gg")
})

