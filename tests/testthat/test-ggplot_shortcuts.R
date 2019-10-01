test_that("Time for ylab is supported", {
  expect_error(axis.flux.gas("XXX"))
  expect_error(axis.flux.co2("XXX"))
  expect_error(axis.flux.ch4("XXX"))

  expect_equal(axis.flux.gas("day")$y, expression(Gas ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  expect_equal(axis.flux.gas("sec")$y, expression(Gas ~ flux ~ ("µ" ~ mol ~ m^-2 ~ s^-1)))

  expect_equal(axis.flux.co2("day")$y, expression(CO[2] ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  expect_equal(axis.flux.co2("sec")$y, expression(CO[2] ~ flux ~ ("µ" ~ mol ~ m^-2 ~ s^-1)))

  expect_equal(axis.flux.ch4("day")$y, expression(CH[4] ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  expect_equal(axis.flux.ch4("sec")$y, expression(CH[4] ~ flux ~ ("µ" ~ mol ~ m^-2 ~ s^-1)))
})
