

context("Parameter Testing")


test_that("Error capturing", {
  expect_error(pt_create_pParams(D = 2))
  expect_error(pt_create_pParams(V = 3))
  expect_error(pt_create_pParams(D = -2, V = 3, js = 2, label = "test"))
  expect_error(pt_create_pParams(D = 2.01, V = 3, js = 2, label = "test"))
  expect_error(pt_create_pParams(D = 2, V = 3, js = -2, label = "test"))
  expect_error(pt_create_pParams(D = 2.01, V = 3, js = 2.01, label = "test"))
  expect_error(pt_create_pParams(D = 2, V = 3, js = 2.01, label = test))
  expect_error(pt_create_pParams(D = 2, V = 3, js = 2.01, label = "test"))  
  })