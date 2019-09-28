test_that("Time for ylab is supported", {
  expect_error(axis.flux.gas("XXX"))
  expect_error(axis.flux.co2("XXX"))
  expect_error(axis.flux.ch4("XXX"))
})
