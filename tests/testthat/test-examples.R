

test_that("Examples functions are ok", {
  expect_error(pt_ex_cnts(5))
  expect_error(pt_ex_nums(5))
})


test_that("Example is correct", {
  expect_equal(
    ptable::pt_ex_cnts()@pTable$p,
    c(
      1.00000000,
      0.50833333,
      0.47500000,
      0.01666667,
      0.16155827,
      0.55565037,
      0.24246618,
      0.04032518,
      0.41231301,
      0.28806097,
      0.18693903,
      0.11268699,
      0.07012498,
      0.24450007,
      0.37074990,
      0.24450007,
      0.07012498
    )
  )
  expect_equal(sum(ptable::pt_ex_nums()@pTable$p), 4)
})
