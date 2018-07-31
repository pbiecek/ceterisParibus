context("Check select_samples() function")


test_that("Output select samples",{
  expect_is(select_sample(apartmentsTest), "data.frame")
})
