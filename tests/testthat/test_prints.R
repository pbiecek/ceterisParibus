context("Check print() functions")


test_that("Output format - print.what_if_explainer",{
  wi_rf <- what_if(explainer_rf, observation = new_apartment, selected_variables = "construction.year")

  expect_is(print(wi_rf), "data.frame")
  expect_equal(nrow(print(wi_rf)), 6)
})




test_that("Output format - print.local_fit_explainer",{
  cr_rf <- local_fit(explainer_rf, observation = new_apartment,
                     select_points = 0.002, selected_variable = "surface")

  expect_is(print(cr_rf), "data.frame")
  expect_equal(nrow(print(cr_rf)), 6)
})


test_that("Output format - print.ceteris_paribus_explainer",{
  cp_rf <- ceteris_paribus(explainer_rf, apartments_A, y = apartments_A$m2.price)

  expect_null(print(cp_rf))
})





test_that("Output format - print.ceteris_paribus_profile",{
  vars <- c("construction.year", "surface", "floor", "no.rooms", "district")
  variable_splits <- calculate_variable_splits(apartments, vars)
  small_apartments <- select_neighbours(apartmentsTest, new_apartment, n = 10)
  small_profiles <- calculate_profiles(small_apartments, variable_splits, apartments_rf_model)

  expect_is(print(small_profiles), "data.frame")
  expect_equal(nrow(print(small_profiles)), 6)
})




