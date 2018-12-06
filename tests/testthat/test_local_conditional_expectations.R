context("Check local_conditional_expectations() function")

lce_rf <- local_conditional_expectations(explainer_rf, apartments_A, y = apartments_A$m2.price)
lce_rf_cy <- local_conditional_expectations(explainer_rf, apartments_A, y = apartments_A$m2.price, variables = "construction.year")


test_that("Wrong input",{
  expect_error(local_conditional_expectations(apartments_rf_model))
})


test_that("Output format - ceteris_paribus_explainer",{
  expect_is(lce_rf, "ceteris_paribus_explainer")
})


test_that("Plotting ceteris_paribus_explainer",{
  expect_is(plot(lce_rf), "gg")
  expect_is(plot(lce_rf, show_profiles = TRUE), "gg")
})


test_that("Add layers to Ceteris Paribus Plot",{
  expect_is(plot(lce_rf) +
              ceteris_paribus_layer(lce_rf, selected_variables = c("surface","construction.year")) ,
            "gg")
})

