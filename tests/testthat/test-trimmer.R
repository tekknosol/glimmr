test_that("trim_time works", {
  int <- lubridate::interval(lubridate::ymd_hm("2018-06-25 12:13"), lubridate::ymd_hm("2018-06-25 12:18"))
  int2 <- lubridate::interval(lubridate::ymd_hms("2018-06-25 12:13:34"), lubridate::ymd_hms("2018-06-25 12:18:34"))
  expect_identical(trim_time(int), lubridate::interval(lubridate::ymd_hm("2018-06-25 12:14"), lubridate::ymd_hm("2018-06-25 12:18")))
  expect_identical(trim_time(int2), lubridate::interval(lubridate::ymd_hm("2018-06-25 12:14"), lubridate::ymd_hm("2018-06-25 12:18")))
})
