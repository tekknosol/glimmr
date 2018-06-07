context("kW context")
library(glimmr)

test_that("Unsupportet model throws error", {
  expect_error(calc_kW(1,2, model =  "XXX"), "model not supported")
})

test_that("Unsupportet gas throws error", {
  expect_error(calc_kW(1,2, gas =  "XXX"), "gas not supported")
  expect_error(kH(1, "XXX"))
  expect_error(calc_SN(1, "XXX"))
})

test_that("Schmidt numbers match literature values at 20°C",{
  expect_equal(round(calc_SN(20)), 600)
  expect_equal(round(calc_SN(20, "ch4")), 617)
})

test_that("Time for ylab is supported", {
  expect_error(axis.flux.gas("XXX"))
  expect_error(axis.flux.co2("XXX"))
  expect_error(axis.flux.ch4("XXX"))
})

test_that("kH for 25°C is matching literature",{
  expect_equal(kH(25), 0.034)
  expect_equal(kH(25, "ch4"), 0.0013)
})
