
test_that("Unsupportet model throws error", {
  expect_error(calc_kW(1, 2, model = "XXX"))
})

test_that("Unsupportet gas throws error", {
  expect_error(calc_kW(1, 2, gas = "XXX"))
  expect_error(kH(1, "XXX"))
  expect_error(calc_SN(1, "XXX"))
})

test_that("Schmidt numbers match literature values at 20°C", {
  expect_equal(round(calc_SN(20)), 600)
  expect_equal(round(calc_SN(20, "ch4")), 617)
  expect_equal(round(calc.SN(20)), 600)
  expect_equal(round(calc.SN(20, "ch4")), 617)
})

test_that("kH for 25°C is matching literature", {
  expect_equal(calc_kH(25), 0.034)
  expect_equal(calc_kH(25, "ch4"), 0.0013)
})

test_that("kW matching expected values", {
  expect_equal(calc_kW(2,18),1.488888, tolerance=1e-3)
  expect_equal(calc_kW(2,18, "ch4"),1.471282, tolerance=1e-3)
  expect_equal(calc.kW(2,18),1.488888, tolerance=1e-3)
  expect_equal(calc.kW(2,18, "ch4"),1.471282, tolerance=1e-3)

  expect_equal(calc_kW(2,18, model = "cole98"),2.846832, tolerance=1e-3)
  expect_equal(calc_kW(2,18, "ch4", model = "cole98"),2.801816, tolerance=1e-3)
  expect_equal(calc.kW(2,18, model = "cole98"),2.846832, tolerance=1e-3)
  expect_equal(calc.kW(2,18, "ch4", model = "cole98"),2.801816, tolerance=1e-3)
})
